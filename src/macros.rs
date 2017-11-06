// For implementing macros in ATL (as opposed to macros which are part
// of its implementation)
use rep::REP;
use types::{Ast, Any, FnArgs, FnRet};
use lexical_scope::Environment;

fn add_defaults(mut env: Environment) -> Environment {
    // env.define("nth", Any::RustFunction(get_nth));
    env.define("quote");
    // env.define("nest", Any::RustFuncitono(nest));
    env
}

// (nth index ast)
fn ast_nth(args: FnArgs) -> FnRet {
    let pntr = args.store.any.add(
        args.store.asts[args[1]][args[0]]
    );
    args.returns(pntr)
}

fn quote(args: FnArgs) -> FnRet {
    let rval = args[0];
    args.returns(rval)
}

#[cfg(test)]
mod testing {
    use types::{lambda, quote, Any, FnArgs, FnRet};
    use asts::build;

    pub fn sub2(args: FnArgs) ->  FnRet {
        assert_eq!(args.len(), 2);
        let rval = args[0] - args[1];
        args.returns(rval)
    }

    #[test]
    fn quote() {
        let rval = rep.run("(quote 3 2 1)");
        assert_eq!(rep.vm,
        );
    }

    // #[test]
    // fn nth() {
    //     let mut rep = REP::new();

    //     if let Some(pntr) = rep.run(
    //     ) {
    //         asset_eq!(
    //             rep.run("(nth 2 (quote foo bla ))"),
    //         );
    //     } else { asset!(false); }
    // }

    // #[test]
    // fn sub3_macro() {
    //     let sub = Any::RustFunction(sub2);

    //     let macro = build(
    //         ast!(Macro,
    //              ast!(lambda(),
    //                   ast!("input", "output"),
    //                   ast!("output",
    //                        quote(sub2),
    //                        ast!("nth", 0, "input"),
    //                        ast!(nest,
    //                             "output",
    //                             quote(sub2),
    //                             ast!("nth", 1, "input"),
    //                             ast!("next", 2, "input")))))
    //     );
    // }
}
