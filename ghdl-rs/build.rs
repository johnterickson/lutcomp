extern crate bindgen;
extern crate execute;

use std::env;
use std::path::PathBuf;

use std::process::{Command, Stdio};
use execute::Execute;

fn build_vhdl(vhdl_path: &str, device: &str) {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    println!("cargo:rerun-if-changed={vhdl_path}");
    println!("cargo:rerun-if-changed=vhpi.ver");
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
        &format!("--workdir={}", out_path.display()),
        &vhdl_path]);

    let entry_o = out_path.join("entry.o").display().to_string();
    ghdl(&[
        "--vpi-compile",
        "-v",
        "gcc",
        "-c", "entry.c",
        "-o", &entry_o,
    ]);


    let device_vpi = out_path.join(format!("{}.vpi", device)).display().to_string();
    ghdl(&[
        "--vpi-link",
        "-v",
        "gcc",
        "-o", &device_vpi,
        &entry_o,
    ]);

    // e.g. ghdl -e -Wl,test.c -Wl,-shared -Wl,-Wl,--version-script=./test.ver -Wl,-Wl,-u,ghdl_main -o tb.lib tb
    let device_lib = out_path.join(format!("{}.lib", device)).display().to_string();
    run("ghdl-llvm",
        &["-e", "--std=08", 
        &format!("--workdir={}", out_path.display()),
        &format!("-Wl,{}", &device_vpi),
        "-Wl,-shared",
        "-Wl,-Wl,--version-script=./vhpi.ver",
        "-Wl,-Wl,-u,ghdl_main",
        "-o", &device_lib,
        device]);

    let device_rs = out_path.join(format!("{}.rs", device)).display().to_string();
    let mut rs = String::new();

    use std::fmt::Write;

    writeln!(&mut rs, "#[allow(dead_code)]").unwrap();
    writeln!(&mut rs, "const {}_lib_path: &'static str = \"{}\";", device, &device_lib).unwrap();
    writeln!(&mut rs, "#[allow(dead_code)]").unwrap();
    writeln!(&mut rs, "const {}_vpi_path: &'static str = \"{}\";", device, &device_vpi).unwrap();

    std::fs::write(device_rs, rs).unwrap();
}


fn main() {
    build_vhdl("../circuit/HD44780U.vhdl", "hd44780u");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

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
