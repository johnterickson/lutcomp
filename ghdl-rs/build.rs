extern crate bindgen;
extern crate execute;

use std::env;
use std::path::PathBuf;

use std::process::{Command, Stdio};
use execute::Execute;

fn main() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let vhdl = "../circuit/HD44780U.vhdl";
    let device = "hd44780u";

    fn ghdl<T: AsRef<std::ffi::OsStr>>(args: &[T]) -> String {
        let mut cmd = Command::new("ghdl");
        for arg in args {
            cmd.arg(arg);
        }
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let output = cmd.execute_output().unwrap();
        if Some(0) != output.status.code() {
            eprintln!("{}", String::from_utf8(output.stdout).unwrap());
            eprintln!("{}", String::from_utf8(output.stderr).unwrap());
            panic!("{:?}", cmd);
        } else {
            String::from_utf8(output.stdout).unwrap()
        }
    }

    ghdl(&["-a", "--std=08", &format!("--workdir={}", out_path.display()), vhdl]);

    ghdl(&["--bind", "--std=08", &format!("--workdir={}", out_path.display()), device]);
    let elaborated_o_pwd = format!("e~{}.o", device);
    let elaborated_o_out = out_path.clone().join(&elaborated_o_pwd);
    std::fs::rename(&elaborated_o_pwd, &elaborated_o_out).unwrap();
    let elaborated_lst_pwd = format!("e~{}.lst", device);
    let elaborated_lst_out = out_path.clone().join(&elaborated_lst_pwd);

    let link_args = ghdl(&["--list-link", "--std=08", &format!("--workdir={}", out_path.display()), device]);
    for line in link_args.as_str().lines() {
        if line == elaborated_o_pwd {
            println!("cargo:rustc-link-arg={}", elaborated_o_out.display());
        } else {
            println!("cargo:rustc-link-arg={}", line);
        }
    }

    let _ = std::fs::rename(&elaborated_lst_pwd, &elaborated_lst_out);

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
