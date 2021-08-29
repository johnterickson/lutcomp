use std::io::{self, Read};

use assemble::*;

fn main() {
    let input = {
        let mut s = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut s).unwrap();
        s
    };
    let _rom = assemble_from_str(&input);
    // let mut c = Computer::with_print(rom, true);
    // while c.step() { }
}