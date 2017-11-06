use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::io;
use std::io::Read;
use std::iter::Iterator;

use asts::NestAst;
use types::symbol;
use types::{Any, Ast};

static DELIMS: &'static [u8] = b" \t\n()\"';";

#[derive(Debug)]
pub enum ParseError {
    UnbalancedParens(usize),
    IOError(io::Error),
}
use self::ParseError::UnbalancedParens;

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self {
            UnbalancedParens(_line) => "Missing closing parenthesis",
            ParseError::IOError(err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match self {
            UnbalancedParens(_) => Some(self),
            ParseError::IOError(ref err) => Some(err),
        }
    }
}

// Similar to asts::SorA, but takes ownership.
#[derive(Debug)]
pub enum Parsed {
    Atom(Any),
    Subex(Ast),
}

pub struct Parser<Reader: io::Read> {
    head: Option<u8>,
    input: io::Bytes<Reader>,
    line: usize,
}

impl<Reader: io::Read> Parser<Reader> {
    pub fn new(reader: Reader) -> Parser<Reader> {
        Parser {
            head: None,
            input: reader.bytes(),
            line: 1,
        }
    }

    fn shift(&mut self) -> Result<(), ParseError> {
        match self.input.next() {
            Some(Ok(cc)) => {
                self.head = Some(cc);
                Ok(())
            }
            Some(Err(err)) => Err(ParseError::IOError(err)),
            None => {
                self.head = None;
                Ok(())
            }
        }
    }

    fn ignore_to_newline(&mut self) -> Result<(), ParseError> {
        loop {
            self.shift()?;
            match self.head {
                Some(b'\n') => {
                    self.line += 1;
                    return Ok(());
                }
                None => return Ok(()),
                Some(_) => (),
            }
        }
    }

    // Continue parsing a sub-expression (expecting that the opening
    // '(' has already been consumed).  Returns Some(()) (since its
    // delim should always be a ')', None for end of stream, or an
    // error for an error.
    fn subex(&mut self, mut ast: &mut Ast) -> Result<(), ParseError> {
        loop {
            self.shift()?;
            match self.head {
                Some(cc) => match cc {
                    b'(' => {
                        let nest = NestAst::new(&mut ast);
                        self.subex(nest.ast)?;
                    }
                    b')' => return Ok(()),
                    b'\n' => self.line += 1,
                    b' ' | b'\t' => continue,
                    b';' => self.ignore_to_newline()?,
                    _ => {
                        ast.push(self.atom()?);
                        match self.head {
                            None => return Err(UnbalancedParens(self.line)),
                            Some(b')') => return Ok(()),
                            Some(b'\n') => self.line += 1,
                            Some(b';') => self.ignore_to_newline()?,
                            // Nothing needs to be done for other delims
                            _ => continue,
                        }
                    }
                },
                None => return Err(UnbalancedParens(self.line)),
            }
        }
    }

    // Parse an atom returing Some(terminating delim) or None for end of stream
    fn atom(&mut self) -> Result<Any, ParseError> {
        let mut scratch = String::new();

        let to_atom = |buff: &str| {
            if let Ok(num) = buff.parse() {
                Any::Fixnum(num)
            } else {
                symbol(&buff)
            }
        };

        loop {
            match self.head {
                Some(cc) => {
                    if DELIMS.contains(&cc) {
                        return Ok(to_atom(&scratch));
                    } else {
                        scratch.push(cc as char);
                    }
                }
                None => return Ok(to_atom(&scratch)),
            }
            self.shift()?;
        }
    }

    // Skip over comments and whitespace, returning the first
    // character which does not fall into those two categories, or
    // None if no such character is found.
    fn strip(&mut self) -> Result<(), ParseError> {
        loop {
            self.shift()?;
            match self.head {
                Some(cc) => match cc {
                    b' ' => continue,

                    b'\n' | b'\t' => {
                        self.line += 1;
                        continue;
                    }

                    b';' => {
                        self.ignore_to_newline()?;
                        continue;
                    }

                    _ => return Ok(()),
                },
                None => return Ok(()),
            }
        }
    }
}

impl<Reader: io::Read> Iterator for Parser<Reader> {
    type Item = Result<Parsed, ParseError>;

    fn next(&mut self) -> Option<Result<Parsed, ParseError>> {
        match self.strip() {
            Ok(()) => self.head.map(|cc| match cc {
                b'(' => {
                    let mut ast = Ast::new();
                    let rval;
                    {
                        let mut nest = NestAst::new(&mut ast);
                        rval = self.subex(&mut nest.ast);
                    }
                    match rval {
                        Ok(_) => Ok(Parsed::Subex(ast)),
                        Err(err) => Err(err),
                    }
                }
                _ => self.atom().map(Parsed::Atom),
            }),
            Err(err) => Some(Err(err)),
        }
    }
}

pub fn from_read<T: Read>(input: T) -> Option<Result<Parsed, ParseError>> {
    Parser::new(input).next()
}

pub fn from_str(input: &str) -> Option<Result<Parsed, ParseError>> {
    from_read(input.as_bytes())
}

#[cfg(test)]
mod tests {
    use parser::{from_str, ParseError::UnbalancedParens, Parsed, Parser};
    use types::{symbol, Any, Ast};

    #[test]
    fn parse_ast() {
        {
            let mut ast = Ast::new();
            let mut parser = Parser::new("12 2)".as_bytes());

            parser.subex(&mut ast).unwrap();

            assert_eq!(ast, vec![Any::Fixnum(12), Any::Fixnum(2)]);
        }

        {
            let mut ast = Ast::new();
            let mut parser = Parser::new("1 (2 3) 4)".as_bytes());
            parser.subex(&mut ast).unwrap();

            assert_eq!(
                ast,
                vec![
                    Any::Fixnum(1),
                    Any::AstData(2),
                    Any::Fixnum(2),
                    Any::Fixnum(3),
                    Any::Fixnum(4),
                ]
            );
        }
    }

    #[test]
    fn parse_unbalanced() {
        match from_str("(12 2") {
            None => panic!("Should have result"),
            Some(Ok(_)) => panic!("Should have an error"),
            Some(Err(err)) => match err {
                UnbalancedParens(line) => assert_eq!(1, line),
                _ => panic!("Should have been UnbalancedParen"),
            },
        }
    }

    #[test]
    fn parse_int() {
        if let Parsed::Subex(ast) = from_str("(1 (2 3) 4)").unwrap().unwrap() {
            assert_eq!(
                ast,
                vec![
                    Any::AstData(5),
                    Any::Fixnum(1),
                    Any::AstData(2),
                    Any::Fixnum(2),
                    Any::Fixnum(3),
                    Any::Fixnum(4),
                ]
            );
        } else {
            panic!("Expected Some ast!");
        }
    }

    #[test]
    fn parse_symbol() {
        if let Parsed::Atom(atom) = from_str("symbol").unwrap().unwrap() {
            assert_eq!(atom, symbol("symbol"));
        } else {
            panic!("Expected Somem ast!");
        }
    }
}
