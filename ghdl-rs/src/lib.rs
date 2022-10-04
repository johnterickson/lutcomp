#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
include!(concat!(env!("OUT_DIR"), "/hd44780u.rs"));

extern crate dlopen;

extern crate strum;
#[macro_use]
extern crate strum_macros;

extern crate packed_struct;
extern crate packed_struct_codegen;

use std::convert::TryFrom;

use packed_struct::prelude::*;

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

use dlopen::raw::Library;
use std::ffi::CStr;
use std::ffi::CString;

pub struct GhdlDevice {
    _lib: Library,
    thunk: vpi_thunk,
    __ghdl_simulation_step: __ghdl_simulation_stepPtr,
}

impl GhdlDevice {
    pub fn new(lib_path: &str, vpi_path: &str) -> GhdlDevice {
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

            let args = [
                progname.as_ptr(),
                // trace_arg.as_ptr(),
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
        }
    }

    pub fn simulation_step(&mut self) -> ::std::os::raw::c_int {
        unsafe { self.__ghdl_simulation_step.unwrap()() }
    }

    pub fn get_vlog_info(&self, vlog_info_p: p_vpi_vlog_info) -> PLI_INT32 {
        unsafe { self.thunk.vpi_get_vlog_info.unwrap()(vlog_info_p) }
    }

    pub fn register_cb(&mut self, cb_data_p: p_cb_data) -> vpiHandle {
        unsafe { self.thunk.vpi_register_cb.unwrap()(cb_data_p) }
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
            self.put_value(expr, &mut val, std::ptr::null_mut(), 0);
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
}

#[cfg(test)]
mod tests {

    use super::*;

    #[allow(unused_variables)]
    #[test]
    fn vhdl_iter() {
        let mut hd44780u = GhdlDevice::new(hd44780u_lib_path, hd44780u_vpi_path);

        // #[no_mangle]
        // pub unsafe extern "C" fn ValueChange(user_data: *mut t_cb_data) -> i32 {
        //     let user_data = &*user_data as &t_cb_data;
        //     let net_name = CStr::from_ptr(THUNK.unwrap().vpi_get_str.unwrap()(vpiName as i32, user_data.obj))
        //             .to_str().unwrap();
        //     println!("Value changed: {}", net_name);
        //     dbg!(user_data);
        //     0
        // }

        use std::collections::BTreeMap;
        use std::ptr;

        // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc
        // for VPI example

        let mut iter = hd44780u.iterate(vpiModule as i32, ptr::null_mut());
        let module = hd44780u.scan(iter);
        hd44780u.free_object(iter);

        let module_name = hd44780u.get_str(vpiName as i32, module);
        dbg!(module_name);

        let scope = hd44780u.handle(vpiScope as i32, module);
        dbg!(scope);

        let mut inputs = BTreeMap::new();
        let mut outputs = BTreeMap::new();

        for kind in [vpiNet] {
            iter = hd44780u.iterate(kind as i32, scope);
            let mut net: vpiHandle = ptr::null_mut();
            while ptr::null_mut() != iter
                && ptr::null_mut() != {
                    net = hd44780u.scan(iter);
                    net
                }
            {
                let net_name = hd44780u.get_str(vpiName as i32, net).to_owned();
                eprint!("{} {}", kind, &net_name);
                let net_width = hd44780u.get(vpiSize as i32, net);
                let net_dir = hd44780u.get(vpiDirection as i32, net);
                eprintln!(
                    " {}:{}",
                    match net_dir as u32 {
                        vpiInput => "in",
                        vpiOutput => "out",
                        vpiInout => "inout",
                        vpiNoDirection => "no direction",
                        _ => panic!("unknown dir {}", net_dir),
                    },
                    net_width
                );
                match net_dir as u32 {
                    vpiInput => {
                        inputs.insert(net_name, (kind, net_width, net));
                    }
                    vpiOutput => {
                        outputs.insert(net_name, (kind, net_width, net));
                    }
                    _ => {
                        hd44780u.free_object(net);
                    }
                }
            }
        }

        for (name, (kind, width, net)) in &inputs {
            hd44780u.put_value_int(*net, 0);
        }

        // for (name, (kind, width, net)) in &outputs {
        //     let mut cb: t_cb_data = std::mem::zeroed();
        //     cb.reason = cbValueChange as i32;
        //     cb.cb_rtn = Some(ValueChange);
        //     cb.obj = *net;

        //     let registration = hd44780u.register_cb(&mut cb);
        // }

        let mut step_count = 0;

        let mut step_result;
        loop {
            if step_count < 10 {
                let (_, _, net) = &inputs["clk"];
                let clk_val = hd44780u.get_value_BinStr(*net);
                let clk_val= clk_val.chars().next().unwrap();

                let new_val = match StdLogic::try_from(clk_val).unwrap() {
                    StdLogic::HDL_0 => Some(1),
                    StdLogic::HDL_1 => Some(0),
                    _ => None,
                };

                if let Some(new_val) = new_val {
                    hd44780u.put_value_int(*net, new_val);
                }
            }

            step_result = hd44780u.simulation_step();
            dbg!(step_result);

            for (name, (kind, width, net)) in inputs.iter().chain(outputs.iter()) {
                let str_val = hd44780u.get_value_BinStr(*net);
                println!(" {name} {kind} {width} {}", str_val);
            }

            if step_result >= 3 {
                break;
            }

            step_count += 1;
        }
    }
}
