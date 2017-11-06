use std::fmt;
use types;
use types::{Any, Ast};

#[derive(PartialEq, Debug, Clone)]
pub struct Subex<'a> {
    pub ast: &'a Ast,
    pub pos: usize,
    pub end: usize,
}

impl<'a> Subex<'a> {
    // Subex will iterate over each element _in_ the ast.
    pub fn from_ast(ast: &'a Ast) -> Option<Subex> {
        match ast.first() {
            Some(&Any::AstData(len)) => Some(Subex {
                ast: ast,
                pos: 1,
                end: len + 1,
            }),
            _ => None,
        }
    }

    // Subex will iterate over every element in the Ast
    //
    // User swears the first element is an AstData
    pub fn from_ast_and_len(ast: &'a Ast, len: usize) -> Subex {
        Subex {
            ast: ast,
            pos: 1,
            end: len + 1,
        }
    }

    // Iterates over every element in the subex
    //
    // User swears the first element is a AstData of len
    pub fn from_subex_and_len(other: &Subex<'a>, len: usize) -> Subex<'a> {
        Subex {
            ast: other.ast,
            pos: other.pos + 1,
            end: other.pos + len + 1,
        }
    }

    // Iterates over each element in the sub-expression at other.pos
    pub fn from_subex(other: &Subex<'a>) -> Option<Subex<'a>> {
        match other.peek() {
            Some(ref any) => match any {
                &Any::AstData(len) => Some(Subex::from_subex_and_len(other, len)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn peek(&self) -> Option<Any> {
        if self.pos < self.end {
            Some(self.ast[self.pos].clone())
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.clone().fold(0, |acc, _| acc + 1)
    }

    pub fn is_empty(&self) -> bool {
        self.pos >= self.end
    }
}

impl<'a> Iterator for Subex<'a> {
    type Item = SorA<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.end {
            let value = &self.ast[self.pos];

            match value {
                &Any::AstData(size) => {
                    let rval = Subex::from_subex_and_len(self, size);
                    self.pos += size + 1;
                    Some(SorA::Subex(rval))
                }
                atom @ _ => {
                    self.pos += 1;
                    Some(SorA::Atom(atom))
                }
            }
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Subex<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("(")?;
        let mut itr = self.clone();

        let write_head = |f: &mut fmt::Formatter, head: SorA| -> fmt::Result {
            match head {
                SorA::Subex(sub) => f.write_fmt(format_args!("{}", sub))?,
                SorA::Atom(atom) => match atom {
                    Any::Fixnum(num) => f.write_fmt(format_args!("{}", num))?,
                    Any::Symbol(meta) => f.write_fmt(format_args!("'{}", meta.name))?,
                    _ => f.write_fmt(format_args!("{:?}", atom))?,
                },
            }
            Ok(())
        };

        itr.next().map(|head| write_head(f, head));
        for item in itr {
            f.write_str(" ")?;
            write_head(f, item)?;
        }

        f.write_str(")")
    }
}

// [S]ubex [or] [A]tom (SorA)
#[derive(PartialEq, Debug)]
pub enum SorA<'a> {
    Subex(Subex<'a>),
    Atom(&'a Any),
}

impl<'a> SorA<'a> {
    pub fn is_subex(&self) -> bool {
        match self {
            SorA::Subex(_) => true,
            _ => false,
        }
    }

    pub fn is_atom(&self) -> bool {
        match self {
            SorA::Atom(_) => true,
            _ => false,
        }
    }
}

pub trait ToAst {
    fn to_ast(&self, &mut Ast);
}

impl ToAst for usize {
    fn to_ast(&self, ast: &mut Ast) {
        ast.push(Any::Fixnum(*self))
    }
}

impl ToAst for Any {
    fn to_ast(&self, ast: &mut Ast) {
        ast.push(self.clone())
    }
}

impl ToAst for str {
    fn to_ast(&self, ast: &mut Ast) {
        ast.push(types::symbol(&self))
    }
}

pub struct Nil {}

impl ToAst for Nil {
    fn to_ast(&self, ast: &mut Ast) {
        ast.push(Any::AstData(0))
    }
}

// A RAII which terminates a sub-expression in an Ast when it
// drops.
pub struct NestAst<'a> {
    pub ast: &'a mut Ast,
    pos: usize,
}

impl<'a> NestAst<'a> {
    pub fn new(ast: &'a mut Ast) -> NestAst {
        let pos = ast.len();
        ast.push(Any::AstData(0));

        NestAst { ast: ast, pos: pos }
    }
}

impl<'a> Drop for NestAst<'a> {
    fn drop(&mut self) {
        // Offset from the AstData at pos
        (self.ast)[self.pos] = Any::AstData((self.ast).len() - self.pos - 1)
    }
}

#[macro_export]
macro_rules! ast {
    ( $( $x:expr ),* ) => {{
        struct AstBuilder {}

        impl ToAst for AstBuilder
        {
            fn to_ast(&self, ast: &mut Ast) {
                let nest = NestAst::new(ast);
                $(
                    $x.to_ast(nest.ast);
                )*
            }
        }

        &AstBuilder {}
    }}
}

pub fn build(factory: &ToAst) -> Ast {
    let mut builder = Ast::new();
    factory.to_ast(&mut builder);
    builder
}

#[cfg(test)]
mod testing {
    use asts::{build, NestAst, SorA, Subex, ToAst};
    use types;
    use types::{Any, Ast};

    #[test]
    fn walk_over_subex() {
        use self::Any::{AstData, Fixnum};

        let input = vec![
            AstData(6),
            Fixnum(1),
            Fixnum(2),
            AstData(2),
            Fixnum(3),
            Fixnum(4),
            Fixnum(5),
        ];
        let mut itr = Subex::from_ast(&input).expect("expected subex..");

        assert_eq!(Some(SorA::Atom(&Fixnum(1))), itr.next());
        assert_eq!(Some(SorA::Atom(&Fixnum(2))), itr.next());
        assert_matches!(itr.next(), Some(SorA::Subex(_)));
        assert_eq!(Some(SorA::Atom(&Fixnum(5))), itr.next());
        assert_eq!(None, itr.next());
    }

    #[test]
    fn walk_into_subex() {
        use self::Any::{AstData, Fixnum};
        let ast: Ast = vec![AstData(4), AstData(2), Fixnum(3), Fixnum(4), Fixnum(5)];

        let outer = Subex::from_ast(&ast).expect("expected a subex");

        if let Some(mut inner) = Subex::from_subex(&outer) {
            assert_eq!(Some(SorA::Atom(&Fixnum(3))), inner.next());
            assert_eq!(Some(SorA::Atom(&Fixnum(4))), inner.next());
            assert_eq!(None, inner.next());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn from_ast_and_len() {
        use self::Any::{AstData, Fixnum};

        let input: Ast = vec![AstData(2), Fixnum(1), Fixnum(2)];

        assert_eq!(
            Subex::from_ast_and_len(&input, 2),
            Subex::from_ast(&input).expect("should be ast")
        );
    }

    #[test]
    fn from_subex_and_len() {
        use self::Any::{AstData, Fixnum};

        let input = vec![AstData(4), Fixnum(3), AstData(2), Fixnum(1), Fixnum(2)];

        let mut subex = Subex::from_ast(&input).expect("should be ast");

        assert_eq!(Some(SorA::Atom(&Fixnum(3))), subex.next());

        let mut itr = Subex::from_subex_and_len(&subex, 2);

        assert_eq!(Some(SorA::Atom(&Fixnum(1))), itr.next());
        assert_eq!(Some(SorA::Atom(&Fixnum(2))), itr.next());
        assert_eq!(None, itr.next());
    }

    #[test]
    fn formatting() {
        use self::Any::{AstData, Fixnum};

        let ast = vec![AstData(3), Fixnum(1), AstData(1), Fixnum(2)];
        let input = Subex::from_ast(&ast).unwrap();
        assert_eq!(format!("{}", input), "(1 (2))")
    }

    #[test]
    fn subex_or_atom_from_next() {
        use types::Any::{AstData, Fixnum};

        let ast = vec![
            AstData(5),
            Fixnum(3),
            AstData(2),
            Fixnum(1),
            Fixnum(2),
            Fixnum(4),
        ];
        let mut outer = Subex::from_ast(&ast).expect("should be ast");

        assert_eq!(Some(SorA::Atom(&Fixnum(3))), outer.next());

        if let Some(SorA::Subex(mut subex)) = outer.next() {
            assert_eq!(Some(SorA::Atom(&Fixnum(1))), subex.next());
            assert_eq!(Some(SorA::Atom(&Fixnum(2))), subex.next());
            assert_eq!(None, subex.next());
        } else {
            panic!("Expected Subex");
        }
    }

    #[test]
    fn make_simple_ast() {
        use types::Any::{AstData, Fixnum};

        let mut ast = Ast::new();
        {
            ast!(1, 2, 3).to_ast(&mut ast);
        }

        let expect = vec![AstData(3), Fixnum(1), Fixnum(2), Fixnum(3)];

        assert_eq!(ast, expect);
    }

    #[test]
    fn simple_build() {
        use types::Any::{AstData, Fixnum};

        let ast = build(ast!(1, 2, 3));

        let expect = vec![AstData(3), Fixnum(1), Fixnum(2), Fixnum(3)];

        assert_eq!(ast, expect);
    }

    #[test]
    fn nested_build() {
        use types::Any::{AstData, Fixnum};

        let ast = build(ast!(1, ast!(2, 3), 4));

        let expect = vec![
            AstData(5),
            Fixnum(1),
            AstData(2),
            Fixnum(2),
            Fixnum(3),
            Fixnum(4),
        ];

        assert_eq!(ast, expect);
    }

    #[test]
    fn lambda_scope() {
        use asts::SorA;
        use types::Any::{Lambda, Symbol};

        let ast = build(ast!(types::lambda(2), ast!("a", "b"), ast!("+", "a", "b")));

        let mut itr = Subex::from_ast(&ast).expect("");

        assert_matches!(itr.next(), Some(SorA::Atom(Lambda(_))));
        assert_matches!(itr.next(), Some(SorA::Subex(_)));

        if let Some(SorA::Subex(mut body)) = itr.next() {
            assert_matches!(body.next(), Some(SorA::Atom(Symbol(_))));
            assert_matches!(body.next(), Some(SorA::Atom(Symbol(_))));
            assert_matches!(body.next(), Some(SorA::Atom(Symbol(_))));
            assert_matches!(body.next(), None);
        } else {
            assert!(false, "No Body!");
        }

        assert_eq!(itr.next(), None);
    }
}
