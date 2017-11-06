extern crate atl;

use atl::rep::REP;
use std::io::{stdin, stdout, Write};

fn main() {
    let mut rep = REP::new();

    loop {
        print!("> ");
        stdout().flush().unwrap();
        rep.run(stdin());
    }
}
