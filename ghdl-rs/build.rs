extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=ghdlvpi");
    println!("cargo:rustc-link-lib=ghdl");

    println!("cargo:rustc-link-arg=-l:libz.so");

    println!("cargo:rustc-link-search=/usr/local/lib/ghdl");
    println!("cargo:rustc-link-lib=grt");

    println!("cargo:rustc-link-search=/usr/local/lib/ghdl/std/v08");
    println!("cargo:rustc-link-arg=-l:env.o");
    println!("cargo:rustc-link-arg=-l:std_standard.o");
    println!("cargo:rustc-link-arg=-l:textio.o");
    println!("cargo:rustc-link-arg=-l:textio-body.o");

    println!("cargo:rustc-link-search=/home/john/lutcomp/circuit");
    println!("cargo:rustc-link-arg=-l:HD44780U.o");
    println!("cargo:rustc-link-arg=-l:e~hd44780u.o");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
