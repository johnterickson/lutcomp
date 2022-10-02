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

    // fn gcc<T: AsRef<std::ffi::OsStr>>(args: &[T]) -> String {
    //     run("gcc", args)
    // }

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
        // "-Wl,-shared",
        // "-Wl,--version-script=./vhpi.ver",
        // "-o", "entry.o"
    ]);

    ghdl(&[
        "--vpi-link",
        "-v",
        "gcc",
        "-o", "entry.vpi",
        "entry.o",
    ]);

    // e.g. ghdl -e -Wl,test.c -Wl,-shared -Wl,-Wl,--version-script=./test.ver -Wl,-Wl,-u,ghdl_main -o tb.lib tb
    ghdl(&["-e", "--std=08", 
    // &format!("--workdir={}", out_path.display()),
        "-Wl,entry.vpi",
        "-Wl,-shared",
        "-Wl,-Wl,--version-script=./vhpi.ver",
        "-Wl,-Wl,-u,ghdl_main",
        "-o", &format!("{}.lib", device),
        device]);

    // println!("cargo:rustc-link-arg={}", "/usr/local/lib/ghdl/libgrt.a");
    // // println!("cargo:rustc-link-arg={}", "/usr/local/lib/libghdlvpi.so");
    // println!("cargo:rustc-link-arg={}", "-ldl");
    // println!("cargo:rustc-link-arg={}", "-lm");
    // println!("cargo:rustc-link-arg={}", "-lz");

    // let elaborated_o_pwd = format!("e~{}.o", device);
    // let elaborated_o_out = out_path.clone().join(&elaborated_o_pwd);
    // std::fs::rename(&elaborated_o_pwd, &elaborated_o_out).unwrap();
    // let elaborated_lst_pwd = format!("e~{}.lst", device);
    // let elaborated_lst_out = out_path.clone().join(&elaborated_lst_pwd);


    // ghdl(&["--bind", "--std=08", 
    //     "-Wl,-shared",
    //     // &format!("--workdir={}", out_path.display()), 
    //     device]);

    // let link_args = ghdl(&["--list-link", "--std=08", 
    // // &format!("--workdir={}", out_path.display()), 
    // device]);
    // for line in link_args.as_str().lines() {
    //     if line.contains(device) || line.contains(vhdl_file) {
    //         let path = std::env::current_dir().unwrap().join(line);
    //         println!("cargo:rustc-link-arg={}", path.display());
    //     } else {
    //         println!("cargo:rustc-link-arg={}", line);
    //     }
    // }

    // let _ = std::fs::rename(&elaborated_lst_pwd, &elaborated_lst_out);

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .blocklist_item("vlog_startup_routines") 
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
