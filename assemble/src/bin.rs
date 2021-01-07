use sim::*;

fn main() {
    let rom = assemble();
    let mut c = Computer::with_print(rom, true);
    while c.step { }
}