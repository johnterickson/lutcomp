
#[cfg(unix)]
use ghdl_rs::build_vhdl;

fn main() {
    #[cfg(unix)]
    build_vhdl("../circuit/HD44780U.vhdl", "hd44780u");
}