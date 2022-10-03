extern crate bindgen;
extern crate execute;

use std::env;
use std::path::PathBuf;

use std::process::{Command, Stdio};
use execute::Execute;

fn main() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let vhdl_file = "HD44780U";
    let vhdl_path = format!("../circuit/{}.vhdl", vhdl_file);
    let device = "hd44780u";

    println!("cargo:rerun-if-changed={vhdl_path}");
    println!("cargo:rerun-if-changed=vhpi.ver");
    println!("cargo:rerun-if-changed=wrapper.h");
    println!("cargo:rerun-if-changed=entry.c");

    fn run<T: AsRef<std::ffi::OsStr>>(program: &str, args: &[T]) -> String {
        let mut cmd = Command::new(program);
        for arg in args {
            cmd.arg(arg);
        }
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let output = cmd.execute_output().unwrap();
        if Some(0) != output.status.code() {
            eprintln!("{}", String::from_utf8(output.stdout).unwrap());
            eprintln!("{}", String::from_utf8(output.stderr).unwrap());
            panic!("{:?} {:?}", cmd.get_program(), cmd.get_args());
        } else {
            String::from_utf8(output.stdout).unwrap()
        }
    }

    fn ghdl<T: AsRef<std::ffi::OsStr>>(args: &[T]) -> String {
        run("ghdl", args)
    }

    ghdl(&["-a", "--std=08", 
        // &format!("--workdir={}", out_path.display()),
        &vhdl_path]);

    ghdl(&[
        "--vpi-compile",
        "-v",
        "gcc",
        "-c",
        "entry.c",
    ]);

    ghdl(&[
        "--vpi-link",
        "-v",
        "gcc",
        "-o", &format!("{}.vpi", device),
        "entry.o",
    ]);

    // e.g. ghdl -e -Wl,test.c -Wl,-shared -Wl,-Wl,--version-script=./test.ver -Wl,-Wl,-u,ghdl_main -o tb.lib tb
    run("ghdl-llvm",
        &["-e", "--std=08", 
    // &format!("--workdir={}", out_path.display()),
        &format!("-Wl,{}.vpi", device),
        "-Wl,-shared",
        "-Wl,-Wl,--version-script=./vhpi.ver",
        "-Wl,-Wl,-u,ghdl_main",
        "-o", &format!("{}.lib", device),
        device]);

    let bindings = bindgen::Builder::default()
        .clang_arg("-I.")
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
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
