#[cfg(test)]
extern crate atl;
use atl::rep::REP;
use atl::store::Store;
use atl::types::wrap_fn;
use std::path::PathBuf;

// Load something from a manifest relative path
fn load_rel(mut rep: REP, tail: &str) -> REP {
    let path: PathBuf = [env!("CARGO_MANIFEST_DIR"), tail].iter().collect();
    rep.load(path.as_path());
    rep
}

pub fn sub2(_store: &mut Store, args: &[usize]) -> usize {
    assert_eq!(args.len(), 2);
    args[0] - args[1]
}

fn run_bare(input: &str) -> Option<usize> {
    let mut rep = REP::new();

    // use functions which add some more checking
    rep.env.define("sub2", wrap_fn(sub2, 0));

    rep.run(input.as_bytes())
}

fn run(input: &str) -> Option<usize> {
    let mut rep = REP::new();

    // use functions which add some more checking
    rep.env.wrap_fn("sub2", sub2);
    rep = load_rel(rep, "src/prelude/prelude.atl");
    rep.run(input.as_bytes())
}

#[test()]
fn run_closure() {
    assert_eq!(run_bare("((\\ (a b) (sub2 a b)) 8 5)"), Some(3));
}

#[test()]
fn run_nested_closure() {
    assert_eq!(
        run_bare(&"(((\\ (a b) (\\ (c) (sub2 (sub2 a b) c))) 8 5) 2)"),
        Some(1)
    );
}

#[test()]
fn run_destructure_tuple() {
    assert_eq!(run_bare(&"((\\ ((Tuple a b c)) a) (Tuple 2 3 5))"), Some(2));

    assert_eq!(run_bare(&"((\\ ((Tuple a b c)) b) (Tuple 2 3 5))"), Some(3));

    assert_eq!(run_bare(&"((\\ ((Tuple a b c)) c) (Tuple 2 3 5))"), Some(5));

    assert_eq!(
        run_bare(&"((\\ ((Tuple a b) c) (sub2 (sub2 a c) b)) (Tuple 9 2) 5)"),
        Some(2)
    );
}

#[test()]
fn test_imports() {
    assert_eq!(
        load_rel(REP::new(), "tests/atl/single-line.atl")
            .vm
            .state
            .stack,
        [5]
    );
    assert_eq!(
        load_rel(REP::new(), "tests/atl/multi-line.atl")
            .vm
            .state
            .stack,
        [8]
    );
}

// test the ATL prelude assert-eq
#[test()]
fn test_assert_eq() {
    assert_eq!(run(&"(assert-eq 1 1)"), Some(1));
    assert_eq!(run(&"(assert-eq 0 1)"), Some(0));
}

// Run the Atl test suite for some core/intrinsic features
#[test()]
fn test_core() {
    let mut rep = load_rel(REP::new(), &"src/prelude/prelude.atl");
    println!("A");
    rep = load_rel(rep, &"tests/atl/core.atl");
    println!("B");
    assert_eq!(rep.vm.state.stack, [1]);
}
