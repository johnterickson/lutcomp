#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

extern crate dlopen;


#[cfg(test)]
mod tests {
    static mut THUNK: Option<vpi_thunk> = None;

    use std::ffi::CString;
    use dlopen::raw::Library;

    use super::*;

    #[test]
    fn vhdl_iter() {
        let progname = std::env::args().next().unwrap();
        // dbg!(&progname);
        let progname = CString::new(progname).unwrap();

        unsafe {
            let vhdl_lib_path = "/home/john/lutcomp/ghdl-rs/hd44780u.lib";
            let lib = Library::open(vhdl_lib_path).unwrap();
            dbg!(&lib);
            let ghdl_main: ghdl_mainPtr = lib.symbol("ghdl_main").unwrap();
            let ghdl_main = ghdl_main.unwrap();
            dbg!(ghdl_main);

            // dbg!(lib.symbol::<*const c_void>("vlog_startup_routines"));

            let mut thunk: vpi_thunk = std::mem::zeroed();
            thunk.vpi_get_vlog_info = lib.symbol("vpi_get_vlog_info").unwrap();
            let vpi_get_vlog_info = thunk.vpi_get_vlog_info.unwrap();
            thunk.vpi_register_cb = lib.symbol("vpi_register_cb").unwrap();
            let vpi_register_cb = thunk.vpi_register_cb.unwrap();
            thunk.vpi_iterate = lib.symbol("vpi_iterate").unwrap();
            let vpi_iterate = thunk.vpi_iterate.unwrap();
            thunk.vpi_scan = lib.symbol("vpi_scan").unwrap();
            let vpi_scan = thunk.vpi_scan.unwrap();
            thunk.vpi_free_object = lib.symbol("vpi_free_object").unwrap();
            let vpi_free_object = thunk.vpi_free_object.unwrap();
            thunk.vpi_get = lib.symbol("vpi_get").unwrap();
            let vpi_get = thunk.vpi_get.unwrap();
            thunk.vpi_get_str = lib.symbol("vpi_get_str").unwrap();
            let vpi_get_str = thunk.vpi_get_str.unwrap();
            thunk.vpi_handle = lib.symbol("vpi_handle").unwrap();
            let vpi_handle = thunk.vpi_handle.unwrap();
            thunk.vpi_get_value = lib.symbol("vpi_get_value").unwrap();
            let vpi_get_value = thunk.vpi_get_value.unwrap();
            THUNK = Some(thunk);

            let mut info: s_vpi_vlog_info = std::mem::zeroed();
            vpi_get_vlog_info(&mut info);
            let product = std::ffi::CStr::from_ptr(info.product);
            let version = std::ffi::CStr::from_ptr(info.version);
            dbg!(product);
            dbg!(version);

            let grt_init: grt_initPtr = lib.symbol("grt_init").unwrap(); let grt_init = grt_init.unwrap();
            let grt_main_options: grt_main_optionsPtr = lib.symbol("grt_main_options").unwrap(); let grt_main_options = grt_main_options.unwrap();
            let grt_main_elab: grt_main_elabPtr = lib.symbol("grt_main_elab").unwrap(); let grt_main_elab = grt_main_elab.unwrap(); 
            let __ghdl_simulation_init: __ghdl_simulation_initPtr = lib.symbol("__ghdl_simulation_init").unwrap(); let __ghdl_simulation_init = __ghdl_simulation_init.unwrap(); 
            let __ghdl_simulation_step: __ghdl_simulation_stepPtr = lib.symbol("__ghdl_simulation_step").unwrap(); let __ghdl_simulation_step = __ghdl_simulation_step.unwrap();

            #[no_mangle]
            pub unsafe extern "C" fn Compiled(user_data: *mut t_cb_data) -> i32 {
                println!("Compiled!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn StartOfSimulation(user_data: *mut t_cb_data) -> i32 {
                println!("Start!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn EndOfSimulation(user_data: *mut t_cb_data) -> i32 {
                println!("End!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn ValueChange(user_data: *mut t_cb_data) -> i32 {
                let user_data = &*user_data as &t_cb_data;
                let net_name = CStr::from_ptr(THUNK.unwrap().vpi_get_str.unwrap()(vpiName as i32, user_data.obj))
                        .to_str().unwrap();
                println!("Value changed: {}", net_name);
                dbg!(user_data);
                0
            }

            // let trace_arg = CString::new("--vpi-trace").unwrap();
            let vpi_arg = CString::new(format!("--vpi=/home/john/lutcomp/ghdl-rs/hd44780u.vpi")).unwrap();

            let args = [
                progname.as_ptr(),
                // trace_arg.as_ptr(),
                vpi_arg.as_ptr(),
            ];

            grt_init();
            dbg!(grt_main_options(progname.as_ptr(), args.len() as i32, args.as_ptr()));
            dbg!(grt_main_elab());

            {
                let mut cb: t_cb_data = std::mem::zeroed();
                cb.reason = cbEndOfCompile as i32;
                cb.cb_rtn = Some(Compiled);
                cb.user_data = std::ptr::null_mut();

                let registration = vpi_register_cb(&mut cb);
            }

            {
                let mut cb: t_cb_data = std::mem::zeroed();
                cb.reason = cbStartOfSimulation as i32;
                cb.cb_rtn = Some(StartOfSimulation);
                cb.user_data = std::ptr::null_mut();

                let registration = vpi_register_cb(&mut cb);
            }

            {
                let mut cb: t_cb_data = std::mem::zeroed();
                cb.reason = cbEndOfSimulation as i32;
                cb.cb_rtn = Some(EndOfSimulation);
                cb.user_data = std::ptr::null_mut();

                let registration = vpi_register_cb(&mut cb);
            }

            __ghdl_simulation_init();

            use std::ffi::CStr;
            use std::ptr;
            use std::collections::BTreeMap;

            // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc 
            // for VPI example

            let mut iter = vpi_iterate(vpiModule as i32, ptr::null_mut());
            let module = vpi_scan(iter);
             vpi_free_object(iter);

            let module_name = CStr::from_ptr(vpi_get_str(vpiName as i32, module));
            dbg!(module_name);

            let scope = vpi_handle(vpiScope as i32, module);
            dbg!(scope);

            let mut inputs = BTreeMap::new();
            let mut outputs = BTreeMap::new();

            for kind in [vpiNet] { //[vpiModule, vpiPort, vpiNet, vpiNetArray] {
                iter = vpi_iterate(kind as i32, scope);
                let mut net: vpiHandle = ptr::null_mut();
                while ptr::null_mut() != iter && ptr::null_mut() != {net = vpi_scan(iter); net} {
                    let net_name = CStr::from_ptr(vpi_get_str(vpiName as i32, net))
                        .to_str().unwrap().to_owned();
                    eprint!("{} {}", kind, &net_name);
                    let net_width = vpi_get(vpiSize as i32, net);
                    let net_dir = vpi_get(vpiDirection as i32, net);
                    eprintln!(" {}:{}",
                        match net_dir as u32 {
                            vpiInput => "in",
                            vpiOutput => "out",
                            vpiInout => "inout",
                            vpiNoDirection => "no direction",
                            _ => panic!("unknown dir {}", net_dir),
                        },
                        net_width);
                    match net_dir as u32 {
                        vpiInput => {
                            inputs.insert(
                                net_name,
                                (kind, net_width, net));
                        },
                        vpiOutput => {
                            outputs.insert(
                                net_name,
                                (kind, net_width, net));
                        },
                        _ => {
                            vpi_free_object(net);
                        }
                    }
                }
            }

            for (name, (kind, width, net)) in inputs.iter().chain(outputs.iter()) {
                let mut cb: t_cb_data = std::mem::zeroed();
                cb.reason = cbValueChange as i32;
                cb.cb_rtn = Some(ValueChange);
                cb.obj = *net;

                let registration = vpi_register_cb(&mut cb);
            }

            let mut step_result;
            loop {

                step_result = __ghdl_simulation_step();

                for (name, (kind, width, net)) in inputs.iter().chain(outputs.iter()) {
                    let mut str_val: s_vpi_value = std::mem::zeroed();
                    str_val.format = vpiBinStrVal as i32;
                    vpi_get_value(*net, &mut str_val);

                    let mut str_val: s_vpi_value = std::mem::zeroed();
                    str_val.format = vpiBinStrVal as i32;
                    vpi_get_value(*net, &mut str_val);

                    let vals = str_val.value.str_ as *const u8;
                    let vals = std::slice::from_raw_parts(vals, *width as usize);
                    let vals = std::str::from_utf8(vals).unwrap();
                    println!("{name} {kind} {width} {}", vals);
                }
                
                dbg!(step_result);
                if step_result >= 3 {
                    break;
                }
            }
        }
    }
}
