#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

extern crate execute;
use std::env;
use std::marker::PhantomPinned;
use std::process::{Command, Stdio};
use std::ptr::null_mut;
use std::sync::{Mutex, MutexGuard};
use execute::Execute;

extern crate dlopen;
use dlopen::raw::Library;

extern crate strum;
#[macro_use]
extern crate strum_macros;

extern crate packed_struct;
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use std::convert::TryFrom;
use std::path::PathBuf;
use std::ffi::CStr;
use std::ffi::CString;

#[derive(Clone, Copy, PartialEq, Eq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
pub enum StdLogic {
    HDL_U = 0,
    HDL_X = 1,
    HDL_0 = 2,
    HDL_1 = 3,
    HDL_Z = 4,
    HDL_W = 5,
    HDL_L = 6,
    HDL_H = 7,
    HDL_D = 8,
}

const STD_LOGIC_CHARS: &[char] = &['U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'];

impl std::fmt::Display for StdLogic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", STD_LOGIC_CHARS[*self as usize])
    }
}

impl TryFrom<u8> for StdLogic {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if let Some(l) = StdLogic::from_primitive(value as u8) {
            Ok(l)
        } else {
            Err(())
        }
    }
}

impl TryFrom<char> for StdLogic {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        for (i, c) in STD_LOGIC_CHARS.iter().enumerate() {
            if *c == value {
                return Ok(StdLogic::from_primitive(i as u8).unwrap());
            }
        }

        Err(())
    }
}

impl StdLogic {
    pub fn from_str(s: &str) -> Option<usize> {
        let mut val = 0;
        for c in s.chars() {
            if let Ok(l) = StdLogic::try_from(c) {
                val |= match l {
                    StdLogic::HDL_0 => 0,
                    StdLogic::HDL_1 => 1,
                    _ => return None,
                };
                val <<= 1;
            } else {
                return None;
            }
        }
        Some(val)
    }
}

#[derive(Clone)]
pub struct Net {
    pub name: String,
    pub handle: vpiHandle,
    pub width: u32,
    pub dir: u32,
    pub kind: u32,
}

impl Net {
    pub fn from_net(handle: vpiHandle, ghdl: &mut GhdlDevice) -> Self {
        let name = ghdl.get_str(vpiName as i32, handle).to_owned();
        let width = ghdl.get(vpiSize as i32, handle) as u32;
        let dir = ghdl.get(vpiDirection as i32, handle) as u32;
        let kind = ghdl.get(vpiType as i32, handle) as u32;

        Net {
            name,
            handle,
            width,
            dir,
            kind
        }
    }
}


pub fn build_vhdl(vhdl_path: &str, device: &str) {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    println!("cargo:rerun-if-changed={vhdl_path}");

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

    run("ghdl-llvm", &[
        "-a", "--std=08", 
        &format!("--workdir={}", out_path.display()),
        &vhdl_path]);

    let entry_c = out_path.join("entry.c").display().to_string();
    std::fs::write(&entry_c, "void (*vlog_startup_routines[]) () = { 0 };").unwrap();

    let entry_o = out_path.join("entry.o").display().to_string();
    run("ghdl-llvm", &[
        "--vpi-compile",
        "-v",
        "gcc",
        "-c", &entry_c,
        "-o", &entry_o,
    ]);


    let device_vpi = out_path.join(format!("{}.vpi", device)).display().to_string();
    run("ghdl-llvm", &[
        "--vpi-link",
        "-v",
        "gcc",
        "-o", &device_vpi,
        &entry_o,
    ]);

    // e.g. ghdl -e -Wl,test.c -Wl,-shared -Wl,-Wl,--version-script=./test.ver -Wl,-Wl,-u,ghdl_main -o tb.lib tb
    let vhpi_ver = out_path.join("entry.c").display().to_string();
    std::fs::write(&vhpi_ver, "
    VHPIDIRECT {
        global:
      ghdl_main;
      grt_init;
      grt_main_options;
      grt_main_elab;
      __ghdl_simulation_init;
      __ghdl_simulation_step;
        local:
              *;
      };
    ").unwrap();
    let device_lib = out_path.join(format!("{}.lib", device)).display().to_string();
    run("ghdl-llvm",
        &["-e", "--std=08", 
        &format!("--workdir={}", out_path.display()),
        &format!("-Wl,{}", &device_vpi),
        "-Wl,-shared",
        &format!("-Wl,-Wl,--version-script={}", &vhpi_ver),
        "-Wl,-Wl,-u,ghdl_main",
        "-o", &device_lib,
        device]);

    let device_rs = out_path.join(format!("{}.rs", device)).display().to_string();
    std::fs::write(device_rs, format!("
        #[allow(dead_code)]
        #[allow(non_upper_case_globals)]
        const {}_LIB_PATH: &'static str = \"{}\";
        #[allow(dead_code)]
        #[allow(non_upper_case_globals)]
        const {}_VPI_PATH: &'static str = \"{}\";",
        device, &device_lib,
        device, &device_vpi
    )).unwrap();
}

static lock: Mutex<()> = Mutex::new(());

pub struct GhdlDevice {
    _lib: Library,
    thunk: vpi_thunk,
    __ghdl_simulation_step: __ghdl_simulation_stepPtr,
    _pin: PhantomPinned,
    cb: Option<Box<dyn FnMut(&t_cb_data)->i32 + 'static>>,
    _lock_guard: MutexGuard<'static, ()>,
}

impl GhdlDevice {
    pub fn new(lib_path: &str, vpi_path: &str) -> GhdlDevice {
        let _lock_guard = lock.lock().unwrap();

        let lib = Library::open(lib_path).unwrap();

        let thunk = unsafe {
            let mut thunk: vpi_thunk = std::mem::zeroed();
            thunk.vpi_get_vlog_info = lib.symbol("vpi_get_vlog_info").unwrap();
            thunk.vpi_register_cb = lib.symbol("vpi_register_cb").unwrap();
            thunk.vpi_iterate = lib.symbol("vpi_iterate").unwrap();
            thunk.vpi_scan = lib.symbol("vpi_scan").unwrap();
            thunk.vpi_free_object = lib.symbol("vpi_free_object").unwrap();
            thunk.vpi_get = lib.symbol("vpi_get").unwrap();
            thunk.vpi_get_str = lib.symbol("vpi_get_str").unwrap();
            thunk.vpi_handle = lib.symbol("vpi_handle").unwrap();
            thunk.vpi_get_value = lib.symbol("vpi_get_value").unwrap();
            thunk.vpi_put_value = lib.symbol("vpi_put_value").unwrap();
            thunk.vpi_get_time = lib.symbol("vpi_get_time").unwrap();
            thunk
        };

        unsafe {
            let mut info: s_vpi_vlog_info = std::mem::zeroed();
            thunk.vpi_get_vlog_info.unwrap()(&mut info);
            let product = std::ffi::CStr::from_ptr(info.product);
            let version = std::ffi::CStr::from_ptr(info.version);
            dbg!(product);
            dbg!(version);
        }

        unsafe {
            let grt_init: grt_initPtr = lib.symbol("grt_init").unwrap();
            let grt_init = grt_init.unwrap();
            let grt_main_options: grt_main_optionsPtr = lib.symbol("grt_main_options").unwrap();
            let grt_main_options = grt_main_options.unwrap();
            let grt_main_elab: grt_main_elabPtr = lib.symbol("grt_main_elab").unwrap();
            let grt_main_elab = grt_main_elab.unwrap();
            let __ghdl_simulation_init: __ghdl_simulation_initPtr =
                lib.symbol("__ghdl_simulation_init").unwrap();
            let __ghdl_simulation_init = __ghdl_simulation_init.unwrap();

            grt_init();
            let progname = CString::new("ghdl").unwrap();

            // let trace_arg = CString::new("--vpi-trace").unwrap();
            let vpi_arg = CString::new(format!("--vpi={}", vpi_path)).unwrap();
            let stop_delta_arg = CString::new("--stop-delta=1000000").unwrap();

            let args = [
                progname.as_ptr(),
                // trace_arg.as_ptr(),
                stop_delta_arg.as_ptr(),
                vpi_arg.as_ptr(),
            ];

            grt_main_options(progname.as_ptr(), args.len() as i32, args.as_ptr());
            grt_main_elab();

            __ghdl_simulation_init();
        }

        let __ghdl_simulation_step = unsafe { lib.symbol("__ghdl_simulation_step").unwrap() };

        GhdlDevice {
            _lib: lib,
            thunk,
            __ghdl_simulation_step,
            _pin: PhantomPinned {},
            cb: None,
            _lock_guard
        }
    }

    pub fn simulation_step(&mut self) -> i32 {
        unsafe { self.__ghdl_simulation_step.unwrap()()}
    }

    pub fn get_vlog_info(&self, vlog_info_p: p_vpi_vlog_info) -> PLI_INT32 {
        unsafe { self.thunk.vpi_get_vlog_info.unwrap()(vlog_info_p) }
    }

    unsafe extern "C" fn static_call_back(cb_data: *mut t_cb_data) -> i32 {
        let ghdl = (*cb_data).user_data as *mut GhdlDevice;
        let ghdl = &mut *ghdl;
        ghdl.call_back(&mut *cb_data)
    }

    fn call_back(&mut self, cb_data: &mut t_cb_data) -> i32 {
        if let Some(cb) = self.cb.as_mut() {
            cb(cb_data)
        } else {
            0
        }
    }

    pub fn set_callback(&mut self, cb: impl FnMut(&t_cb_data)->i32 + 'static) {
        self.cb = Some(Box::new(cb));
    }

    pub fn register_cb(&mut self, reason: i32, obj: *mut u32) -> vpiHandle{
        unsafe {
            let mut cb = t_cb_data {
                reason, 
                cb_rtn: Some(Self::static_call_back), 
                obj, 
                time: null_mut(),
                value: null_mut(),
                index: 0,
                user_data: self as *mut GhdlDevice as *mut i8
            };
            // dbg!(cb.user_data as *mut c_void);
            self.thunk.vpi_register_cb.unwrap()(&mut cb)
        }
    }

    pub fn iterate(&self, type_: PLI_INT32, refHandle: vpiHandle) -> vpiHandle {
        unsafe { self.thunk.vpi_iterate.unwrap()(type_, refHandle) }
    }

    pub fn scan(&self, iterator: vpiHandle) -> vpiHandle {
        unsafe { self.thunk.vpi_scan.unwrap()(iterator) }
    }

    pub fn free_object(&self, object: vpiHandle) -> PLI_INT32 {
        unsafe { self.thunk.vpi_free_object.unwrap()(object) }
    }

    pub fn get(&self, property: PLI_INT32, object: vpiHandle) -> PLI_INT32 {
        unsafe { self.thunk.vpi_get.unwrap()(property, object) }
    }

    pub fn get_str(&mut self, property: PLI_INT32, object: vpiHandle) -> &str {
        unsafe { 
            CStr::from_ptr(self.thunk.vpi_get_str.unwrap()(property, object)).to_str().unwrap()
        }
    }

    pub fn handle(&self, type_: PLI_INT32, refHandle: vpiHandle) -> vpiHandle {
        unsafe { self.thunk.vpi_handle.unwrap()(type_, refHandle) }
    }

    pub fn get_value(&self, expr: vpiHandle, value_p: p_vpi_value) {
        unsafe { self.thunk.vpi_get_value.unwrap()(expr, value_p) }
    }

    pub fn get_value_BinStr(&mut self, expr: vpiHandle) -> &str {
        unsafe { 
            let mut str_val: s_vpi_value = std::mem::zeroed();
            str_val.format = vpiBinStrVal as i32;
            self.thunk.vpi_get_value.unwrap()(expr, &mut str_val);

            let vals = CStr::from_ptr(str_val.value.str_);
            vals.to_str().unwrap()
        }
    }

    pub fn put_value_int(&mut self, expr: vpiHandle, new_val: i32) {
        unsafe { 
            let mut val: s_vpi_value = std::mem::zeroed();
            val.format = vpiIntVal as i32;
            val.value.integer = new_val;
            let mut time = t_vpi_time {
                type_: vpiSimTime as i32,
                high: 0,
                low: 1,
                real: 0.0
            };
            self.put_value(expr, &mut val, &mut time, vpiNoDelay as i32);
        }
    }

    pub fn put_value_BinStr(&mut self, expr: vpiHandle, new_val: &str) {
        unsafe { 
            let mut val: s_vpi_value = std::mem::zeroed();
            val.format = vpiBinStrVal as i32;
            let new_val = CString::new(new_val).unwrap();
            val.value.str_ = new_val.as_ptr() as *mut i8;
            let mut time = t_vpi_time {
                type_: vpiSimTime as i32,
                high: 0,
                low: 1,
                real: 0.0
            };
            self.put_value(expr, &mut val, &mut time, vpiNoDelay as i32);
        }
    }

    pub fn put_value(
        &mut self,
        object: vpiHandle,
        value_p: p_vpi_value,
        time_p: p_vpi_time,
        flags: PLI_INT32,
    ) -> vpiHandle {
        unsafe { self.thunk.vpi_put_value.unwrap()(object, value_p, time_p, flags) }
    }

    pub fn get_time(&mut self, object: vpiHandle) -> u64 {
        unsafe { 
            let mut time: t_vpi_time = std::mem::zeroed();
            time.type_ = vpiSimTime as i32;
            self.thunk.vpi_get_time.unwrap()(object, &mut time);
            (time.high as u64) << 32 | (time.low as u64)
        }
    }

}

// impl IndexMut<&str> for GhdlDevice {

// }