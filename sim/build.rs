use ghdl_rs::build_vhdl;


fn main() {
    build_vhdl("../circuit/HD44780U.vhdl", "hd44780u");
}