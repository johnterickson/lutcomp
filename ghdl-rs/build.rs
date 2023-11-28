extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let bindings = bindgen::Builder::default()
        .clang_arg("-I.")
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // rust won't cast to u8 as part of const expr...
        .blocklist_type("std_logic_states")
        //u128
        .blocklist_function("max_align_t")
        .blocklist_function("strtold")
        .blocklist_function("q.cvt")
        .blocklist_function("q.cvt_r")
        .blocklist_type("_Float64x")
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
