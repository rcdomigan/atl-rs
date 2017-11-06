use std::string::String;

use asts::SorA;
use primitives;
use std::collections::{HashMap, hash_map};
use std::option::Option;
use types;
use types::{Any, Ast, AtlFnSig, ClosureMetadata, ContextManager, Slot, wrap_fn};
use asts::Subex;

static TOPLEVEL_EXPECTED: &'static str = "Should always be at least toplevel scope!";

type Bindings = HashMap<String, Any>;

#[derive(Debug)]
pub struct Closure {
    local: Bindings,
    captured: Vec<Any>,
    formal_count: usize,
    destructure_offset: usize,
}

// Represents a function scope in the Environment
impl Closure {
    fn new() -> Closure {
        Closure {
            local: HashMap::new(),
            captured: Vec::new(),
            formal_count: 0,
            destructure_offset: 0,
        }
    }

    fn metadata(&self) -> ClosureMetadata {
        ClosureMetadata {
            body: 0,
            formals_count: self.formal_count,
            captured: self.captured.clone(),
        }
    }
}

pub type Undefined = HashMap<Slot, String>;

#[derive(Debug)]
pub struct Environment {
    pub slots: usize,
    closures: Vec<Closure>,
    pub undefined: Undefined,
}

// We've looked up a free-variable, here's how it should be referenced
// in this scope:
enum CaptureKind {
    Free,
    Literal(Any),
    Stack(Any),
}

#[derive(Debug)]
pub struct RustFnItr<'a> {
    inner: hash_map::Values<'a, String, Any>,
}

impl<'a> Iterator for RustFnItr<'a> {
    type Item = (types::AtlFn, Slot);
    fn next(&mut self) -> Option<(types::AtlFn, Slot)> {
        loop {
            match self.inner.next() {
                Some(Any::RustFunction(func, slot)) => return Some((func.clone(), *slot)),
                Some(_) => (),
                None => return None,
            }
        }
    }
}

impl<'a> Environment {
    /// Construct a new environment without setting up primitive
    /// mappings.  Meant for testing.
    pub fn new_empty() -> Environment {
        Environment {
            closures: vec![Closure::new()],
            slots: 0,
            undefined: HashMap::new(),
        }
    }

    pub fn new() -> Environment {
        let mut env = Environment::new_empty();
        env.wrap_fn("sub2", primitives::sub2);
        env.wrap_fn("add2", primitives::add2);
        env.wrap_fn("equal", primitives::equal);
        env.define("\\", types::lambda(0));
        env.define("define", Any::Define(0));
        env.define("Tuple", Any::Tuple);
        env.define("exit", Any::Exit);
        env.define("if", Any::If);
        env.define("True", Any::Fixnum(1));
        env.define("False", Any::Fixnum(0));
        env.define(":", Any::DeclareType);
        env
    }

    pub fn rust_functions(&self) -> RustFnItr {
        RustFnItr{inner: self.globals().values()}
    }

    pub fn wrap_fn(&mut self, key: &str, func: AtlFnSig) -> Any{
        let slot = self.slots;
        self.slots += 1;
        let rval = wrap_fn(func, slot);
        self.define(&key, rval.clone());
        rval
    }

    pub fn define(&mut self, key: &str, value: Any) {
        match self.closures.last_mut() {
            Some(closure) => {
                closure.local.insert(key.to_string(), value);
            }
            None => panic!(TOPLEVEL_EXPECTED),
        }
    }

    // If symbol is in an enclosing scope, makes sure a
    // ClosureParameter chain is setup all intervening closures and
    // return the value of the current ClosureParameter.
    fn _close_sym(closures: &mut Iterator<Item = &mut Closure>, name: &str) -> CaptureKind {
        use self::CaptureKind::{Free, Literal, Stack};
        use types::Any::{ClosureParameter, IndexParam, Parameter};
        match closures.next() {
            Some(env) => {
                if let &Some(any) = &env.local.get(name) {
                    match any {
                        // For IndexParam, it might be better to capture
                        // the whole tuple and just unpack it in the
                        // context it's being used.. this is easier
                        // and optimal probably varies case by case.
                        &Parameter(_) | &ClosureParameter(_) | &IndexParam(_, _) => {
                            return Stack(any.clone());
                        }
                        _ => return Literal(any.clone()),
                    }
                }
                match Environment::_close_sym(closures, name) {
                    Stack(parent_def) => {
                        let val = ClosureParameter(env.captured.len());
                        env.local.insert(name.to_string(), val.clone());
                        env.captured.push(parent_def);
                        Stack(val)
                    }
                    val @ _ => val,
                }
            }
            None => Free,
        }
    }

    // If `name` is free in the current function, but defined in the
    // closure, return the appropriate Some(ClosureParameter).
    fn close_sym(&mut self, name: &str) -> Any {
        use self::CaptureKind::{Free, Literal, Stack};
        match Environment::_close_sym(&mut self.closures.iter_mut().skip(1).rev().peekable(), name)
        {
            Free => match self.globals().get(name) {
                Some(any) => any.clone(),
                None => {
                    self.undefined.insert(self.slots, name.to_string());

                    let value = Any::Deref(self.slots);
                    self.slots += 1;
                    self.globals_mut().insert(name.to_string(), value.clone());
                    value
                }
            },
            Stack(val) => val,
            Literal(val) => val,
        }
    }

    fn globals_mut(&mut self) -> &mut Bindings {
        &mut self.closures[0].local
    }

    fn globals(&self) -> &Bindings {
        &self.closures[0].local
    }

    fn current_value(&mut self, input: &Any) -> Any {
        match input {
            &Any::Symbol(ref metadata) => self.close_sym(&(*metadata).name),
            _ => input.clone(),
        }
    }
}

// The environment adds a closure for the scope of a 'Context' call
impl ContextManager for Environment {
    type Witness = ();

    fn enter(&mut self) -> () {
        self.closures.push(Closure::new());
    }

    // Consume the inner context
    fn exit(&mut self, _witness: ()) {
        self.closures.pop();
    }
}

#[derive(Debug)]
pub enum ScopeError {
    Missing(String),
    Extra(String),
    WrongType(String),
}

// State is the current input expression, env, and output.  It has
// helper methods to mapping from input to the output.
struct State<'a> {
    env: &'a mut Environment,
    output: Ast,
    rest: Subex<'a>,
}

impl<'a> State<'a> {
    fn new(env: &'a mut Environment, rest: Subex<'a>, output: Ast) -> State<'a> {
        State { env, output, rest }
    }

    fn shift(mut self) -> (Option<SorA<'a>>, Self) {
        if let Some(head) = self.rest.next() {
            return (Some(head), self);
        }
        (None, self)
    }

    // Chomp uses two functions to map the first argument of rest onto
    // output.  One function, atom_cb, is used on atoms and one,
    // subex_cb, used for nested sub-expressions in the input.
    fn chomp<SubexFn, AtomFn>(
        mut self,
        subex_cb: SubexFn,
        atom_cb: AtomFn,
    ) -> Result<Self, ScopeError>
    where
        SubexFn: Fn(Self) -> Result<Self, ScopeError>,
        AtomFn: Fn(Self, &Any) -> Result<Self, ScopeError>,
    {
        let (head, tmp) = self.shift();
        self = tmp;
        match head {
            Some(SorA::Subex(sub)) => {
                let rest = self.rest;
                self.rest = sub;
                self = subex_cb(self)?;
                self.rest = rest;
                Ok(self)
            }
            Some(SorA::Atom(any)) => atom_cb(self, any),
            None => Err(ScopeError::Missing("nothing more to chomp!".to_string())),
        }
    }

    // each uses two functions to map rest onto output.  One function,
    // atom_cb, is used on atoms and one, subex_cb, used for nested
    // sub-expressions in the input.
    fn each<SubexFn, AtomFn>(
        mut self,
        subex_cb: &SubexFn,
        atom_cb: &AtomFn,
    ) -> Result<Self, ScopeError>
    where
        SubexFn: Fn(Self) -> Result<Self, ScopeError>,
        AtomFn: Fn(Self, &Any) -> Result<Self, ScopeError>,
    {
        while !self.rest.is_empty() {
            self = self.chomp(subex_cb, atom_cb)?;
        }
        Ok(self)
    }

    // Check that the input is empty (ie, we're finished)
    fn finished(self) -> Result<Self, ScopeError> {
        if !self.rest.is_empty() {
            Err(ScopeError::Extra(format!(
                "No more items expected, but got {:?}",
                self.rest
            )))
        } else {
            Ok(self)
        }
    }
}

// Destructure a formals subexpression (eg `(Tuple b c)` from `(a
// (Tuple b c) d)`.  Currently just works with Tuples.
fn unpack_formals<'a>(state: State<'a>) -> Result<State<'a>, ScopeError> {
    use self::ScopeError::WrongType;
    use types::Any::{IndexParam, Symbol};

    // local function doesn't need to be captured (unlike a closure)
    fn wrong_type<'a>(thing: &Any) -> Result<State<'a>, ScopeError> {
        Err(WrongType(format!(
            "Formals subexpression: expected Symbol, got {:?}",
            thing
        )))
    }

    // first item should be the constructor tag (or a destructured
    // constructor I guess), in anycase it won't be accessible as an
    // argument.
    state
        .chomp(unpack_formals, |mut state, atm| {
            match state.env.current_value(atm) {
                Any::Tuple => {
                    state.output.push(atm.clone());
                    Ok(state)
                }
                _ => Err(WrongType(format!(
                    "Formals subexpression: need constructor as first element.  Got {:?}",
                    atm
                ))),
            }
        })?
        .each(&unpack_formals, &|mut state, sym| {
            if let &Any::Symbol(ref metadata) = sym {
                let param: Any;
                {
                    let ref mut closure = state.env.closures.last_mut().unwrap();
                    param = IndexParam(closure.formal_count, closure.destructure_offset);
                    closure.destructure_offset += 1;
                }
                state.env.define(&metadata.name, param.clone());
                state.output.push(Symbol(metadata.clone()));
                Ok(state)
            } else {
                wrong_type(sym)
            }
        })
}

fn lambda<'a>(mut state: State<'a>) -> Result<State<'a>, ScopeError> {
    use self::ScopeError::WrongType;
    let lambda_place = state.output.len();
    nest_attr!(state.env, |mut state: State<'a>| {
        state.output.push(Any::PlaceHolder);

        // FORMALS //
        state = state.chomp(
            |mut state| {
                nest_attr!(state.output, |state: State<'a>| state.each(
                    &|mut state| nest_attr!(state.output, |mut state| {
                        state = unpack_formals(state)?;
                        state.env.closures.last_mut().unwrap().formal_count += 1;
                        Ok(state)
                    }),
                    &|mut state, atm| match atm {
                        &Any::Symbol(ref metadata) => {
                            let param =
                                Any::Parameter(state.env.closures.last().unwrap().formal_count);
                            state.env.closures.last_mut().unwrap().formal_count += 1;

                            state.env.define(&metadata.name, param.clone());
                            state.output.push(param);
                            Ok(state)
                        }
                        got @ _ => Err(WrongType(format!(
                            "Lambda formal expects symbol, got {:?}",
                            got
                        ))),
                    },
                ))
            },
            |_, atm| {
                Err(WrongType(format!(
                    "Lambda expects formals list, got {:?}",
                    atm
                )))
            },
        )?;

        // BODY //
        state = state.chomp(ast, atom)?.finished()?;

        state.output[lambda_place] =
            Any::Lambda(Box::new(state.env.closures.last().unwrap().metadata()));
        Ok(state)
    })
}

fn ast<'a>(mut state: State<'a>) -> Result<State<'a>, ScopeError> {
    use self::ScopeError::WrongType;
    nest_attr!(state.output, |state: State<'a>| state
        .chomp(
            |state| ast(state),
            |state, any| match state.env.current_value(&any) {
                Any::Define(_) => state.chomp(
                    |state| Err(WrongType(format!("Expected sym, got Ast {:?}", state.rest))),
                    |mut state, atm| match atm {
                        &Any::Symbol(ref sym) => {
                            if let Some(value) = state.env.globals().get(&sym.name) {
                                match value {
                                    &Any::Deref(existing_slot) => {
                                        state.output.push(Any::Define(existing_slot));
                                    }
                                    _ => {
                                        return Err(WrongType(format!(
                                            "Expected slot or undefined, got {:?}",
                                            value
                                        )))
                                    }
                                }
                            } else {
                                // New symbol -- put a deref in to the environment
                                let slot = state.env.slots;
                                state
                                    .env
                                    .globals_mut()
                                    .insert(sym.name.clone(), Any::Deref(slot));
                                state.output.push(Any::Define(slot));
                                state.env.slots += 1;
                            }
                            state.output.push(Any::Symbol(sym.clone()));
                            state.chomp(ast, atom)?.finished()
                        }
                        _ => Err(WrongType(format!("Expected Symbol, got {:?}", atm))),
                    },
                ),
                Any::Lambda(_) => lambda(state),
                any @ _ => atom(state, &any),
            }
        )?
        .each(&ast, &atom))
}

fn atom<'a>(mut state: State<'a>, atm: &Any) -> Result<State<'a>, ScopeError> {
    state.output.push(state.env.current_value(atm));
    Ok(state)
}

pub fn scope(env: &mut Environment, input: &Ast) -> Result<Ast, ScopeError> {
    use self::ScopeError::Missing;

    if let Some(subex) = Subex::from_ast(input) {
        Ok(ast(State::new(env, subex, Ast::new()))?.output)
    } else if let Some(_any) = input.first() {
        panic!("Ast isn't an Ast!  Why did I do this to myself!");
    } else {
        Err(Missing("No stuff!".to_string()))
    }
}

#[cfg(test)]
mod testing {
    use asts;
    use asts::{NestAst, ToAst};
    use lexical_scope::{scope, Environment};
    use types;
    use types::{Any, Ast, wrap_fn};
    use primitives::sub2;

    #[test]
    fn current_value() {
        use types::symbol;
        use types::Any::Fixnum;

        let mut bindings = Environment::new_empty();
        bindings.define("foo", Fixnum(1));

        let foo = bindings.current_value(&symbol("foo"));
        let bar = bindings.current_value(&symbol("bar"));

        assert_eq!(Fixnum(1), foo);
        assert_eq!(Any::Deref(0), bar);
    }

    // Ast just gets copied; there's no assignment to be done
    #[test]
    fn trivial_scope() {
        use types::Any::{AstData, Fixnum};
        let input: Ast = vec![
            AstData(5),
            Fixnum(1),
            AstData(2),
            Fixnum(2),
            Fixnum(3),
            Fixnum(4),
        ];

        let mut env = Environment::new_empty();
        let copied = scope(&mut env, &input).expect("Oh no?!");

        assert_eq!(input.len(), copied.len());
        for (left, right) in input.iter().zip(copied) {
            assert_eq!(left, &right);
        }
    }

    #[test]
    fn lambda_scope() {
        use asts;
        use asts::SorA;
        use std::ops::Deref;
        use types::Any;

        let mut env = Environment::new_empty();
        let output = scope(
            &mut env,
            &asts::build(ast!(types::lambda(2), ast!("a", "b"), ast!("+", "a", "b"))),
        ).expect("");

        let mut itr = asts::Subex::from_ast(&output).expect("Expected some output");

        if let Some(SorA::Atom(Any::Lambda(ref data))) = itr.next() {
            assert_eq!(
                data.deref(),
                &types::ClosureMetadata {
                    body: 0,
                    formals_count: 2,
                    captured: Vec::new(),
                }
            );
        } else {
            assert!(false, "Not Lambda?");
        }

        // don't really care about formals (at least not yet).
        assert_matches!(itr.next(), Some(SorA::Subex(_)));

        match itr.next() {
            Some(SorA::Subex(mut body)) => {
                assert_eq!(body.next(), Some(SorA::Atom(&Any::Deref(0))));
                assert_eq!(body.next(), Some(SorA::Atom(&Any::Parameter(0))));
                assert_eq!(body.next(), Some(SorA::Atom(&Any::Parameter(1))));
                assert_eq!(body.next(), None);
            },
            Some(_) => panic!("Expected Ast, got something else!"),
            None => panic!("No body!"),
        }

        assert_eq!(itr.next(), None);
    }

    #[test]
    fn nested_lambda_scope() {
        use asts;
        use std::boxed::Box;
        use types::Any;
        use types::Any::Parameter;;

        let mut env = Environment::new_empty();

        let output = scope(
            &mut env,
            &asts::build(ast!(
                types::lambda(2),
                ast!("a", "b"),
                ast!(types::lambda(1), ast!("c"), ast!("a", "b", "c"))
            )),
        ).expect("");

        let expected = asts::build(ast!(
            Any::Lambda(Box::new(types::ClosureMetadata {
                body: 0,
                formals_count: 2,
                captured: vec![],
            })),
            ast!(Parameter(0), Parameter(1)),
            ast!(
                Any::Lambda(Box::new(types::ClosureMetadata {
                    body: 0,
                    formals_count: 1,
                    captured: vec![Any::Parameter(0), Any::Parameter(1)],
                })),
                ast!(Parameter(0)),
                ast!(
                    Any::ClosureParameter(0),
                    Any::ClosureParameter(1),
                    Any::Parameter(0)
                )
            )
        ));

        assert_eq!(output, expected);
    }

    #[test]
    fn apply_lambda() {
        use asts;
        use std::boxed::Box;
        use types::Any;
        use types::Any::{Fixnum, Parameter};;

        let mut env = Environment::new_empty();
        env.wrap_fn("sub2", sub2);

        let output = scope(
            &mut env,
            &asts::build(ast!(
                ast!(types::lambda(2), ast!("a", "b"), ast!("sub2", "a", "b")),
                Fixnum(8),
                Fixnum(5)
            )),
        ).unwrap();

        let expected = asts::build(ast!(
            ast!(
                Any::Lambda(Box::new(types::ClosureMetadata {
                    body: 0,
                    formals_count: 2,
                    captured: vec![],
                })),
                ast!(Parameter(0), Parameter(1)),
                ast!(wrap_fn(sub2, 0), Parameter(0), Parameter(1))
            ),
            Fixnum(8),
            Fixnum(5)
        ));

        assert_eq!(output, expected);
    }

    #[test]
    fn using_global() {
        use asts::{Subex, SorA};

        let mut env = Environment::new_empty();
        env.define("+", Any::Fixnum(3));

        let output = scope(
            &mut env,
            &asts::build(ast!(types::lambda(2), ast!("a", "b"), ast!("+", "a", "b"))),
        ).expect("");

        let mut itr = Subex::from_ast(&output).expect("");

        assert_matches!(itr.next(), Some(SorA::Atom(&Any::Lambda(_))));

        assert_matches!(itr.next(), Some(SorA::Subex(_)));

        if let Some(SorA::Subex(mut body)) = itr.next() {
            assert_matches!(body.next(), Some(SorA::Atom(Any::Fixnum(3))));
            assert_matches!(body.next(), Some(SorA::Atom(Any::Parameter(0))));
            assert_matches!(body.next(), Some(SorA::Atom(Any::Parameter(1))));
            assert_matches!(body.next(), None);
        } else {
            assert!(false, "No Body!");
        }

        assert_eq!(itr.next(), None);
    }

    // try defining a variable before using it
    #[test]
    fn define_first() {
        let mut env = Environment::new_empty();
        env.define("define", Any::Define(0));

        let def_foo = scope(&mut env, &asts::build(ast!("define", "foo", 1))).unwrap();
        let expected = asts::build(ast!(Any::Define(0), "foo", 1));
        assert_eq!(def_foo, expected);

        assert_eq!(env.current_value(&types::symbol("foo")), Any::Deref(0));
    }

    #[test]
    fn destructure_tuple() {
        use types::Any::{IndexParam, Tuple};
        use types::{lambda, symbol};

        let mut env = Environment::new();

        let expr = scope(
            &mut env,
            &asts::build(ast!(
                lambda(2),
                ast!(ast!(Tuple, "a", "b")),
                ast!("sub2", "a", "b")
            )),
        ).unwrap();

        assert_eq!(
            expr,
            asts::build(ast!(
                lambda(1),
                ast!(ast!(Tuple, symbol("a"), symbol("b"))),
                ast!(wrap_fn(sub2, 0), IndexParam(0, 0), IndexParam(0, 1))
            ))
        )
    }
}
