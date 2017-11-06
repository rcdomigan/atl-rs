use asts::SorA;
use asts::Subex;
use lexical_scope::{Environment, Undefined};
use op_codes::{BuildCode, Op, UsePosAtDrop};
use tuple::make_tuple;
use types::{Any, Ast, AtlFn, ClosureMetadata, Slot, VMState};

#[derive(Debug)]
pub enum CompilerError {
    UnhandledType(Any),
    WrongType(Any),
    // TODO: I'd like SurplusExpression to take something like a
    // Subex, but I'm not sure how to handle the lifetime of the
    // underlying Ast.
    SurplusExpression(String),
    NoStuff,
    UndefinedSymbol(String),
}


// The context in which we're compiling an expression
#[derive(Debug)]
struct Context<'a> {
    // Is this expression in the tail position?
    tail_call: bool,

    // Is this happening in a `define` block?  This flags contexs
    // where not-yet-defined symbols may be expected, which should
    // invlude letrec contexts (although they don't yet exist in atl).
    is_define: bool,

    // Slots which have not been defined yet.
    undefined: &'a Undefined,

    closure: Option<&'a ClosureMetadata>
}

impl<'a> Context<'a> {

    // Make sure the slot corrosponds to a defined symbol.
    fn check_slot(&self, slot: Slot) -> Result<(), CompilerError> {
        if self.is_define && self.closure.is_some() {
            Ok(())
        } else {
            match self.undefined.get(&slot) {
                Some(symbol) => Err(CompilerError::UndefinedSymbol(
                    symbol.to_string()
                )),
                None => Ok(()),
            }
        }
    }

    fn replace_tail(&self, tail_call: bool) -> Self {
        Context {
            tail_call,
            is_define: self.is_define,
            undefined: self.undefined,
            closure: self.closure,
        }
    }

    fn replace_closure(&self, closure: Option<&'a ClosureMetadata>) -> Self {
        Context {
            tail_call: self.tail_call,
            is_define: self.is_define,
            undefined: self.undefined,
            closure,
        }
    }

    fn replace_define(&self, is_define: bool) -> Context {
        Context {
            tail_call: self.tail_call,
            is_define,
            undefined: self.undefined,
            closure: self.closure,
        }
    }
}

// Add the opcode for an atom found in a non-head position
fn rest_atom<'a>(
    any: &Any,
    code: &mut BuildCode,
    context: &Context,
) -> Result<(), CompilerError> {
    match any {
        &Any::Fixnum(value) => code.push(Op::Fixnum(value)),
        &Any::Bool(value) => code.push(Op::Fixnum(match value {
            true => 1,
            false => 0,
        })),
        &Any::ClosureParameter(num) => code.push(Op::ClosureArgument(num)),
        Any::Deref(slot) => {
            context.check_slot(*slot)?;
            code.push(Op::DerefFn(*slot));
        }
        // Note: arguments count backwards (right most arg is index 0).
        &Any::IndexParam(param, offset) => match context.closure {
            Some(ref closure) => {
                code.push(Op::IndexArg(closure.argument_index(param), offset))
            }
            None => panic!("Inernal compiler error!  Cannot use IndexParam outside a closure!"),
        },
        &Any::Parameter(idx) => match context.closure {
            Some(ref closure) => {
                code.push(Op::Argument(closure.argument_index(idx)))
            }
            None => panic!("Inernal compiler error!  Cannot use Parameter outside a closure!"),
        },
        &Any::RustFunction(ref _func, slot) => code.push(Op::DerefFn(slot)),
        _ => return Err(CompilerError::UnhandledType(any.clone())),
    }
    Ok(())
}

// Compile function (primitive or closure) arguments
//  code: [arg0]..[argN][N]
fn args<'a>(
    mut input: Subex<'a>,
    output: &mut BuildCode,
    context: &Context,
) -> Result<(), CompilerError> {
    if input.is_empty() {
        return Ok(());
    }

    let mut arg_count = 0;
    loop {
        let rval = dispatch(input, output, &context.replace_tail(false))?;
        arg_count += 1;

        match rval {
            Some(rest) => input = rest,
            None => break,
        }
    }

    output.push(Op::Fixnum(arg_count));
    Ok(())
}

// Setup a closure for each primitive RustFunction in env.  The
// closure wrapper for lets us treat primitive functions as values.
pub fn wrap_primitives(env: &mut Environment, code: &mut BuildCode, vm_state: &mut VMState) {
    for (func, slot) in env.rust_functions() {
        let closure_base = vm_state.store.new_closure(code.len(), &[]);
        code.push(Op::RustFunctionWrapped(func));

        if vm_state.slots.len() <= slot {
            vm_state.slots.resize(slot + 1, 0);
        }
        vm_state.slots[slot] = closure_base;
    }
    code.set_enter(code.len())
}

fn ast<'a>(
    mut rest: Subex<'a>,
    mut output: &mut BuildCode,
    context: &Context,
) -> Result<(), CompilerError> {
    if let Some(head) = rest.next() {
        match head {
            // A subex in the head position better be returning a
            // function
            SorA::Subex(inner) => {
                let inner_context = context.replace_tail(false);
                args(rest, &mut output, &inner_context)?;
                ast(inner, output, &inner_context)?;
                if context.tail_call {
                    output.push(Op::TailCall);
                } else {
                    output.push(Op::CallClosure);
                }

                Ok(())
            }
            SorA::Atom(head) => match head {
                &Any::DeclareType => {
                    rest.next(); // ignoring type information
                    match rest.next() {
                        Some(SorA::Subex(body)) => ast(body, output, context)?,
                        Some(SorA::Atom(atm)) => rest_atom(atm, output, context)?,
                        None => return Err(CompilerError::NoStuff),
                    }

                    if let Some(ref expr) = rest.next() {
                        return Err(CompilerError::SurplusExpression(format!("{:?}", expr)));
                    }
                    return Ok(());
                }
                &Any::Define(slot) => {
                    rest.next(); // not using symbol yet
                    dispatch(rest, &mut output, &context.replace_define(true))?;
                    output.push(Op::DefineGlobal(slot));
                    Ok(())
                }
                &Any::Deref(slot) => {
                    context.check_slot(slot)?;
                    args(rest, &mut output, &context)?;
                    output.push(Op::DerefFn(slot));
                    if context.tail_call {
                        output.push(Op::TailCall);
                    } else {
                        output.push(Op::CallClosure);
                    }
                    Ok(())
                }
                &Any::Exit => {
                    match dispatch(rest, &mut output, context)? {
                        Some(thing) => {
                            return Err(CompilerError::SurplusExpression(format!("{:?}", thing)))
                        }
                        None => output.push(Op::Finish),
                    }
                    Ok(())
                }
                &Any::If => {
                    let consequent_jump;
                    let alternate;

                    {
                        let mut alt_is_skipping =
                            UsePosAtDrop::new(output, |value| Op::Fixnum(value));

                        match dispatch(rest, &mut alt_is_skipping.code, context)? {
                            Some(rest) => {
                                {
                                    alt_is_skipping.code.push(Op::If);

                                    // dispatch consequent, remaining expression is alternate
                                    alternate =
                                        dispatch(rest, &mut alt_is_skipping.code, context)?;

                                    // add the instruction now and patch later so Atl
                                    // skips over the jump
                                    consequent_jump =
                                        UsePosAtDrop::will_patch(&mut alt_is_skipping.code);
                                }
                            }
                            None => return Err(CompilerError::NoStuff),
                        }
                    }

                    match alternate {
                        Some(rest) => {
                            let mut consequent_is_skipping =
                                consequent_jump(output, |value| Op::Jump(value));
                            //alternate
                            dispatch(rest, &mut consequent_is_skipping.code, context)?;
                        }
                        None => return Err(CompilerError::NoStuff),
                    }
                    Ok(())
                }
                &Any::Lambda(ref raw_metadata) => {
                    let mut metadata = raw_metadata.clone();
                    {
                        let mut skip_body = UsePosAtDrop::new(output, Op::Jump);
                        metadata.body = skip_body.code.len();

                        rest.next(); // don't have to do anything with the formals

                        dispatch(rest, &mut skip_body.code, &context.replace_tail(true).replace_closure(Some(&metadata)))?;

                        skip_body.code.push(Op::Return);
                    }

                    for capture in metadata.captured.iter() {
                        rest_atom(&capture, output, context)?;
                    }

                    output.push(Op::MakeClosure(metadata.body, metadata.captured.len()));
                    Ok(())
                }
                &Any::RustFunction(ref func, _slot) => {
                    args(rest, &mut output, context)?;
                    output.push(Op::RustFunction(func.clone()));
                    Ok(())
                }
                // construct tuple:
                &Any::Tuple => {
                    args(rest, &mut output, context)?;
                    output.push(Op::RustFunction(AtlFn(make_tuple)));
                    Ok(())
                }
                _ => return Err(CompilerError::WrongType(head.clone())),
            },
        }
    } else {
        Err(CompilerError::NoStuff)
    }
}

// Compile an expression from `subex` into `output`, returning any
// remaining expressions from the subex.
fn dispatch<'a>(
    mut rest: Subex<'a>,
    output: &mut BuildCode,
    context: &Context,
) -> Result<Option<Subex<'a>>, CompilerError> {
    if let Some(item) = rest.next() {
        match item {
            SorA::Subex(inner) => ast(inner, output, context)?,
            SorA::Atom(any) => rest_atom(any, output, context)?,
        }
        if rest.is_empty() {
            Ok(None)
        } else {
            Ok(Some(rest))
        }
    } else {
        Ok(None)
    }
}

pub fn compile(
    input: &Ast,
    mut output: BuildCode,
    undefined: &Undefined,
) -> Result<BuildCode, CompilerError> {
    let context = Context {
        tail_call: false,
        is_define: false,
        undefined,
        closure: None,
    };
    match input.first() {
        Some(&Any::AstData(len)) => {
            ast(Subex::from_ast_and_len(input, len), &mut output, &context)?;
        }
        // NOTE: this'll return the wrapped form of a RustFunction,
        // which is maybe not right (but probably doesn't matter).
        Some(any @ _) => rest_atom(any, &mut output, &context)?,
        None => return Err(CompilerError::NoStuff),
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use asts::{build, NestAst, ToAst};
    use compile::compile;
    use lexical_scope::{Environment, Undefined};
    use op_codes::{BuildCode, Op};
    use std::boxed::Box;
    use store::Store;
    use types;
    use types::{wrap_fn, Any, Ast, AtlFn, VMState};

    pub fn bin_fn(_store: &mut Store, args: &[usize]) -> usize {
        assert_eq!(args.len(), 2);
        0
    }

    pub fn fn3(_store: &mut Store, args: &[usize]) -> usize {
        assert_eq!(args.len(), 3);
        0
    }

    // Compile starting with a fresh 'output' vector, and panic if
    // there's an error
    pub fn test_compile(input: &Ast) -> Vec<Op> {
        compile(&input, BuildCode::testing_new(), &Undefined::new())
            .expect("need BuildCode")
            .dump()
    }

    #[test()]
    fn trivial_fn() {
        let input = build(ast!(wrap_fn(bin_fn, 0), 2, 3));

        let output = vec![
            Op::Fixnum(2),
            Op::Fixnum(3),
            Op::Fixnum(2),
            Op::RustFunction(AtlFn(bin_fn)),
        ];

        assert_eq!(test_compile(&input), output)
    }

    #[test()]
    fn nested_fn() {
        let input = build(ast!(
            Any::RustFunction(AtlFn(bin_fn), 0),
            ast!(Any::RustFunction(AtlFn(bin_fn), 0), 1, 2),
            3
        ));

        let output = vec![
            Op::Fixnum(1),
            Op::Fixnum(2),
            Op::Fixnum(2),
            Op::RustFunction(AtlFn(bin_fn)),
            Op::Fixnum(3),
            Op::Fixnum(2),
            Op::RustFunction(AtlFn(bin_fn)),
        ];

        assert_eq!(test_compile(&input), output)
    }

    #[test()]
    fn trivial_if() {
        let input = build(ast!(
            Any::If,
            Any::Bool(false),
            Any::Fixnum(1),
            Any::Fixnum(2)
        ));

        let output = vec![
            Op::Fixnum(5), // 0: alternate start
            Op::Fixnum(0), // 1: predicate
            Op::If,        // 2:
            Op::Fixnum(1), // 3: consequent value
            Op::Jump(6),   // 4: jump to end
            Op::Fixnum(2), // 5: alternate value
        ];

        assert_eq!(test_compile(&input), output);
    }

    #[test()]
    fn if_() {
        let input = build(ast!(
            Any::If,
            ast!(wrap_fn(bin_fn, 0), 1, 2),
            ast!(wrap_fn(bin_fn, 0), 1, 2),
            ast!(wrap_fn(bin_fn, 0), 3, 4)
        ));

        let output = vec![
            Op::Fixnum(11),                  // 0: alternate start
            Op::Fixnum(1),                   // 1: predicate
            Op::Fixnum(2),                   // 2:
            Op::Fixnum(2),                   // 3:
            Op::RustFunction(AtlFn(bin_fn)), // 4:
            Op::If,                          // 5:
            Op::Fixnum(1),                   // 6: consequent
            Op::Fixnum(2),                   // 7:
            Op::Fixnum(2),                   // 8:
            Op::RustFunction(AtlFn(bin_fn)), // 9:
            Op::Jump(15),                    // 10: jump to end
            Op::Fixnum(3),                   // 11: alternate
            Op::Fixnum(4),                   // 12:
            Op::Fixnum(2),                   // 13:
            Op::RustFunction(AtlFn(bin_fn)), // 14:
        ];

        assert_eq!(test_compile(&input), output);
    }

    #[test()]
    fn making_closure() {
        fn a() -> Any {
            Any::Parameter(0)
        };
        fn b() -> Any {
            Any::Parameter(1)
        };

        let input = build(ast!(
            types::lambda(2),
            ast!(a(), b()),
            ast!(Any::RustFunction(AtlFn(bin_fn), 0), a(), b())
        ));

        let expect = vec![
            Op::Jump(6),                     // 0:
            Op::Argument(1),                 // 1:
            Op::Argument(0),                 // 2:
            Op::Fixnum(2),                   // 3:
            Op::RustFunction(AtlFn(bin_fn)), // 4:
            Op::Return,                      // 5:
            Op::MakeClosure(1, 0),           // 6:
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test()]
    fn calling_closure() {
        fn a() -> Any {
            Any::Parameter(0)
        };
        fn b() -> Any {
            Any::Parameter(1)
        };

        let input = build(ast!(
            ast!(
                types::lambda(2),
                ast!(a(), b()),
                ast!(Any::RustFunction(AtlFn(bin_fn), 0), a(), b())
            ),
            1,
            2
        ));

        let expect = vec![
            Op::Fixnum(1),                   // 0:
            Op::Fixnum(2),                   // 1:
            Op::Fixnum(2),                   // 2:
            Op::Jump(9),                     // 3:
            Op::Argument(1),                 // 4: body
            Op::Argument(0),                 // 5:
            Op::Fixnum(2),                   // 6:
            Op::RustFunction(AtlFn(bin_fn)), // 7:
            Op::Return,                      // 8:
            Op::MakeClosure(4, 0),           // 9:
            Op::CallClosure,
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test()]
    fn make_nested_closure() {
        use self::Op;
        use types::ClosureMetadata;

        fn a_arg() -> Any {
            Any::Parameter(0)
        };
        fn a_closure() -> Any {
            Any::ClosureParameter(0)
        };
        fn b() -> Any {
            Any::Parameter(0)
        };

        fn meta0() -> ClosureMetadata {
            ClosureMetadata {
                body: 2,
                formals_count: 1,
                captured: vec![Any::Parameter(0)],
            }
        }
        fn meta1() -> ClosureMetadata {
            ClosureMetadata {
                body: 1,
                formals_count: 1,
                captured: Vec::new(),
            }
        }

        // (\fn 1\ (a) (\fn 0\ (b) (sub2 a b)))
        let input = build(ast!(
            Any::Lambda(Box::new(meta1())),
            ast!(a_arg()),
            ast!(
                Any::Lambda(Box::new(meta0())),
                ast!(b()),
                ast!(wrap_fn(bin_fn, 0), a_closure(), b())
            )
        ));

        let expect = vec![
            Op::Jump(10),                    // 0:
            Op::Jump(7),                     // 1: body 1
            Op::ClosureArgument(0),          // 2: body 0
            Op::Argument(0),                 // 3:
            Op::Fixnum(2),                   // 4:
            Op::RustFunction(AtlFn(bin_fn)), // 5:
            Op::Return,                      // 6:
            Op::Argument(0),                 // 7:
            Op::MakeClosure(2, 1),           // 8: closure 0
            Op::Return,                      // 9:
            Op::MakeClosure(1, 0),           // 10: closure 1
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test()]
    fn make_and_call_nested_closure() {
        use self::Op;
        use types::ClosureMetadata;

        fn a_arg() -> Any {
            Any::Parameter(0)
        };
        fn a_closure() -> Any {
            Any::ClosureParameter(0)
        };
        fn b() -> Any {
            Any::Parameter(0)
        };

        fn meta0() -> ClosureMetadata {
            ClosureMetadata {
                body: 2,
                formals_count: 1,
                captured: vec![Any::Parameter(0)],
            }
        }
        fn meta1() -> ClosureMetadata {
            ClosureMetadata {
                body: 1,
                formals_count: 1,
                captured: Vec::new(),
            }
        }

        // (((\fn 1\ (a) (\fn 0\ (b) (sub2 a b))) 5) 2)
        let input = build(ast!(
            ast!(
                ast!(
                    Any::Lambda(Box::new(meta1())),
                    ast!(a_arg()),
                    ast!(
                        Any::Lambda(Box::new(meta0())),
                        ast!(b()),
                        ast!(wrap_fn(bin_fn, 0), a_closure(), b())
                    )
                ),
                5
            ),
            2
        ));

        let expect = vec![
            Op::Fixnum(2),
            Op::Fixnum(1),
            Op::Fixnum(5),
            Op::Fixnum(1),
            Op::Jump(14),
            Op::Jump(11),
            Op::ClosureArgument(0),
            Op::Argument(0),
            Op::Fixnum(2),
            Op::RustFunction(AtlFn(bin_fn)),
            Op::Return,
            Op::Argument(0),
            Op::MakeClosure(6, 1),
            Op::Return,
            Op::MakeClosure(5, 0),
            Op::CallClosure,
            Op::CallClosure,
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test()]
    fn call_defined_closure() {
        // Trivial example, since integration test will have to catch
        // actually setting the closure up in a slot
        let input = build(ast!(Any::Deref(0), 1, 2));

        let expect = vec![
            Op::Fixnum(1),
            Op::Fixnum(2),
            Op::Fixnum(2),
            Op::DerefFn(0),
            Op::CallClosure,
        ];

        assert_eq!(test_compile(&input), expect);
    }

    // Should mirror 'assert-eq' integration test
    #[test()]
    fn atl_assert_eq() {
        use primitives::equal;
        use types::Any::*;
        use types::{lambda, symbol};

        // try compiling assert-eq! as defined in prelude
        let input = build(ast!(
            Define(0),
            symbol("assert-eq"),
            ast!(
                lambda(2),
                ast!("a", "b"),
                ast!(
                    If,
                    ast!(wrap_fn(equal, 0), Parameter(0), Parameter(1)),
                    Bool(true),
                    ast!(Exit, Bool(false))
                )
            )
        ));

        let expect = vec![
            Op::Jump(12),                   // 0:
            Op::Fixnum(9),                  // 1: alternate address
            Op::Argument(1),                // 2: predicate
            Op::Argument(0),                // 3:
            Op::Fixnum(2),                  // 4:
            Op::RustFunction(AtlFn(equal)), // 5:
            Op::If,                         // 6:
            Op::Fixnum(1),                  // 7: consequent
            Op::Jump(11),                   // 8:
            Op::Fixnum(0),                  // 9: alternate
            Op::Finish,                     // 10:
            Op::Return,                     // 11:
            Op::MakeClosure(1, 0),          // 12:
            Op::DefineGlobal(0),            // 13:
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test]
    fn check_tail_call() {
        use asts::Nil;
        use types::{lambda, symbol};

        // (define recur (\ () (recur)))
        let input = build(ast!(
            Any::Define(0),
            symbol("recur"),
            ast!(lambda(0), Nil {}, ast!(Any::Deref(0)))
        ));

        let expect = vec![
            Op::Jump(4),           // 0:
            Op::DerefFn(0),        // 1:
            Op::TailCall,          // 2
            Op::Return,            // 3: not needed here, but hard to avoid
            Op::MakeClosure(1, 0), // 4:
            Op::DefineGlobal(0),   // 5:
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test]
    fn tuple_with_args() {
        use types::{
            lambda, Any::{IndexParam, Parameter, Tuple},
        };
        // (\\ (a (Tuple b c) d) (fn3 a c d))
        let input = build(ast!(
            lambda(3),
            ast!("a", ast!(Tuple, "b", "c"), "d"),
            ast!(
                wrap_fn(fn3, 0),
                Parameter(2),
                IndexParam(1, 1),
                Parameter(0)
            )
        ));

        let expect = vec![
            Op::Jump(7),                  // 0:
            Op::Argument(0),              // 1:
            Op::IndexArg(1, 1),           // 2:
            Op::Argument(2),              // 3:
            Op::Fixnum(3),                // 4:
            Op::RustFunction(AtlFn(fn3)), // 5:
            Op::Return,                   // 6:
            Op::MakeClosure(1, 0),        // 7:
        ];

        assert_eq!(test_compile(&input), expect);
    }

    #[test]
    fn destructure_tuple() {
        use types::symbol;
        use types::Any::{IndexParam, Tuple};
        // (\ ((Tuple a b)) (- a b))
        let input = build(ast!(
            types::lambda(1),
            ast!(ast!(Tuple, symbol("a"), symbol("b"))),
            ast!(wrap_fn(bin_fn, 0), IndexParam(0, 0), IndexParam(0, 1))
        ));

        let expect = vec![
            Op::Jump(6),                     // 0:
            Op::IndexArg(0, 0),              // 1:
            Op::IndexArg(0, 1),              // 2:
            Op::Fixnum(2),                   // 3:
            Op::RustFunction(AtlFn(bin_fn)), // 4:
            Op::Return,                      // 5:
            Op::MakeClosure(1, 0),           // 6:
        ];

        assert_eq!(test_compile(&input), expect)
    }

    // The full Environment::new() will generate a bunch of wrapper
    // code; this just sets up a wrapper for bin_fn.
    fn setup_some_primitives() -> (BuildCode, VMState, Environment) {
        use compile::wrap_primitives;

        let mut code = BuildCode::new();
        let mut vm_state = VMState::new();
        let mut env = Environment::new_empty();
        env.wrap_fn("equal", bin_fn);
        env.wrap_fn("test", bin_fn);

        // setup the function's wrapper
        wrap_primitives(&mut env, &mut code, &mut vm_state);
        (code, vm_state, env)
    }

    #[test]
    fn test_wrap_primitives() {
        let (code, vm_state, _env) = setup_some_primitives();

        let expect: Vec<usize> = vec![1, 1, 1, 2];
        assert_eq!(*vm_state.store.test_view(), expect);

        assert_eq!(
            code.dump(),
            vec![
                Op::Jump(3),
                Op::RustFunctionWrapped(AtlFn(bin_fn)),
                Op::RustFunctionWrapped(AtlFn(bin_fn)),
            ]
        );
    }

    // Check that 
    #[test]
    fn test_undefined_symbol() {
        use asts::Nil;
        use compile::CompilerError;
        use types::lambda;

        let mut undefined = Undefined::new();
        undefined.insert(0, "floop".to_string());

        let my_compile = |ast| {
            compile(&ast, BuildCode::testing_new(), &undefined)
        };

        // Unassigned symbol in function definition should be OK:
        match my_compile(build(ast!(
            Any::Define(0),
            "recur",
            ast!(lambda(0), Nil {}, ast!(Any::Deref(0)))
        ))) {
            Ok(_) => (),
            Err(err) => {
                println!("Unexpected error: {:?}", err);
                panic!();
            },
        }

        // Unassigned symbol in non-define should fail:
        match my_compile(build(ast!(lambda(0), Nil {}, ast!(Any::Deref(0))))) {
            Err(CompilerError::UndefinedSymbol(sym)) => {
                assert_eq!(sym, "floop")
            },
            Err(err) => {
                println!("Unexpected error: {:?}", err);
                panic!();
            },
            Ok(_) => {
                panic!("Unexpected success");
            }
        }

        // Unassigned symbol constant define should fail:
        match my_compile(build(ast!(
            Any::Define(0),
            "constant",
            // Pretending floop is a thunk since nested case seems
            // more likely to fail:
            ast!(Any::Deref(0))
        ))) {
            Err(CompilerError::UndefinedSymbol(sym)) => {
                assert_eq!(sym, "floop")
            },
            Err(err) => {
                println!("Unexpected error: {:?}", err);
                panic!();
            },
            Ok(_) => {
                panic!("Unexpected success");
            }
        }
    }
}
