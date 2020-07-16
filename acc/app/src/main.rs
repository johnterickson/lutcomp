
use alu::*;
use assemble::*;
use ucode::*;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => {
            ucode();
        }
        Some("alu") => {
            alu();
        }
        Some("assemble") => {
            assemble();
        }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
