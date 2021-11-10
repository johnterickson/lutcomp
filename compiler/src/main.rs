use std::{env, io::{self, Read}};

use compiler::*;

fn main() -> Result<(), std::io::Error> {
    let input = {
        let mut s = String::new();
        let stdin = io::stdin();
        stdin.lock().read_to_string(&mut s)?;
        s
    };

    let (_program, assembly) = compile("main", &input, &env::current_dir().unwrap());

    let rom = assemble::assemble(assembly);

    let args : Vec<_> = std::env::args().skip(1).map(|arg| u8::from_str_radix(&arg, 16).unwrap()).collect();

    main_inner(&rom, args);
    Ok(())
}