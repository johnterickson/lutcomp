#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

extern crate dlopen;

#[cfg(test)]
mod tests {
    use std::ffi::{CString,c_void};
    use dlopen::raw::Library;

    use super::*;

    // #[no_mangle]
    // pub static vlog_startup_routines: [Option<unsafe extern "C" fn()>;1] =
    //     [//Some(Startup), 
    //     None];
     
    #[test]
    fn vhdl_iter() {
        let progname = std::env::args().next().unwrap();
        // dbg!(&progname);
        let progname = CString::new(progname).unwrap();

        unsafe {
            // let libghdlvpi = Library::open("/usr/local/lib/libghdlvpi.so").unwrap();
            // dbg!(&libghdlvpi);
            // let vpi_thunk: *const c_void = libghdlvpi.symbol("VPI_THUNK").unwrap();
            // dbg!(&vpi_thunk);
            // dbg!(*vpi_thunk);

            // let vpi_thunk = vpi_thunk.as_ref().unwrap();

            let vhdl_lib_path = "/home/john/lutcomp/ghdl-rs/hd44780u.lib";
            let lib = Library::open(vhdl_lib_path).unwrap();
            dbg!(&lib);
            let ghdl_main: ghdl_mainPtr = lib.symbol("ghdl_main").unwrap();
            let ghdl_main = ghdl_main.unwrap();
            dbg!(ghdl_main);

            // dbg!(lib.symbol::<*const c_void>("vlog_startup_routines"));

            let mut thunk: vpi_thunk = std::mem::zeroed();
            thunk.vpi_get_vlog_info = lib.symbol("vpi_get_vlog_info").unwrap();
            thunk.vpi_register_cb = lib.symbol("vpi_register_cb").unwrap();

            let mut info: s_vpi_vlog_info = std::mem::zeroed();
            thunk.vpi_get_vlog_info.unwrap()(&mut info);
            dbg!(&info);
            let product = std::ffi::CStr::from_ptr(info.product);
            let version = std::ffi::CStr::from_ptr(info.version);
            dbg!(product);
            dbg!(version);

            let grt_init: Option<unsafe extern "C" fn () -> ()> = lib.symbol("grt_init").unwrap();
            dbg!(grt_init);

            #[no_mangle]
            pub unsafe extern "C" fn Compiled(user_data: *mut t_cb_data) -> i32 {
                println!("Hello!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn StartOfSimulation(user_data: *mut t_cb_data) -> i32 {
                println!("Hello!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn EndOfSimulation(user_data: *mut t_cb_data) -> i32 {
                println!("Goodbye!");
                dbg!(user_data);
                0
            }

            // #[no_mangle]
            // pub unsafe extern "C" fn Startup() 
            {
                {
                    let mut cb: t_cb_data = std::mem::zeroed();
                    cb.reason = cbEndOfCompile as i32;
                    cb.cb_rtn = Some(Compiled);
                    cb.user_data = std::ptr::null_mut();

                    let registration = thunk.vpi_register_cb.unwrap()(&mut cb);
                    dbg!(&registration);
                }

                {
                    let mut cb: t_cb_data = std::mem::zeroed();
                    cb.reason = cbStartOfSimulation as i32;
                    cb.cb_rtn = Some(StartOfSimulation);
                    cb.user_data = std::ptr::null_mut();

                    let registration = thunk.vpi_register_cb.unwrap()(&mut cb);
                    dbg!(&registration);
                }

                {
                    let mut cb: t_cb_data = std::mem::zeroed();
                    cb.reason = cbEndOfSimulation as i32;
                    cb.cb_rtn = Some(EndOfSimulation);
                    cb.user_data = std::ptr::null_mut();

                    let registration = thunk.vpi_register_cb.unwrap()(&mut cb);
                    dbg!(&registration);
                }
            }

            let trace_arg = CString::new("--vpi-trace").unwrap();
            let vpi_arg = CString::new(format!("--vpi=/home/john/lutcomp/ghdl-rs/entry.vpi")).unwrap();
            // let run_arg = CString::new("-v").unwrap();

            let args = [
                progname.as_ptr(),
                // run_arg.as_ptr(),
                trace_arg.as_ptr(),
                vpi_arg.as_ptr(),
            ];

            dbg!(ghdl_main(args.len() as i32, args.as_ptr()));

            // let libghdlvpi_name = CString::new("/usr/local/lib/libghdl-2_0_0.so").unwrap();
            // let libghdlvpi_mod = grt_dynload_open (libghdlvpi_name.as_ptr());
            // dbg!(libghdlvpi_mod);


            // let lib_path = CString::new("/home/john/lutcomp/ghdl-rs/hd44780u.lib").unwrap();
            // // dbg!(loadVpiModule(lib_path.as_ptr()));

            // let lib = grt_dynload_open(lib_path.as_ptr());
            // dbg!(lib);

            // let ghdl_main = CString::new("ghdl_main").unwrap();
            // let ghdl_main = grt_dynload_symbol(lib, ghdl_main.as_ptr());
            // dbg!(ghdl_main);



            // // let loadVpiModule = CString::new("loadVpiModule").unwrap();
            // let loadVpiModule  = grt_dynload_symbol(lib, loadVpiModule.as_ptr());
            // let loadVpiModule: loadVpiModulePtr  = Some(std::mem::transmute(loadVpiModule));
            // dbg!(loadVpiModule);
            // loadVpiModule.unwrap()(lib_path.as_ptr());


            // grt_init();



            // #[no_mangle]
            // pub unsafe extern "C" fn Startup() {
            //     {
            //         let mut cb: t_cb_data = std::mem::zeroed();
            //         cb.reason = cbEndOfCompile as i32;
            //         cb.cb_rtn = Some(Compiled);
            //         cb.user_data = std::ptr::null_mut();

            //         let registration = vpi_register_cb(&mut cb);
            //         dbg!(&registration);
            //     }

            //     {
            //         let mut cb: t_cb_data = std::mem::zeroed();
            //         cb.reason = cbStartOfSimulation as i32;
            //         cb.cb_rtn = Some(StartOfSimulation);
            //         cb.user_data = std::ptr::null_mut();

            //         let registration = vpi_register_cb(&mut cb);
            //         dbg!(&registration);
            //     }

            //     {
            //         let mut cb: t_cb_data = std::mem::zeroed();
            //         cb.reason = cbEndOfSimulation as i32;
            //         cb.cb_rtn = Some(EndOfSimulation);
            //         cb.user_data = std::ptr::null_mut();

            //         let registration = vpi_register_cb(&mut cb);
            //         dbg!(&registration);
            //     }
            // }





            // let mut args = [
            //     progname.as_ptr() as *mut std::ffi::c_void
            // ];
            // //ghdl_main(args.len() as i32, args.as_ptr() as *mut *mut std::ffi::c_void);
            
            // grt_init ();
            

            // let args = [
            //      progname.as_ptr(),
            //     // "--vpi-trace".as_ptr() as *const i8,
            //      std::ptr::null()
            // ];
            // grt_main_options (progname.as_ptr(), (args.len() - 1) as i32, args.as_ptr());
            // grt_main_elab ();

            // // Startup();

            // __ghdl_simulation_init();


            // use std::ffi::CStr;
            // use std::ptr;
            // use std::collections::BTreeMap;

            // // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc 
            // // for VPI example

            // let mut info: s_vpi_vlog_info = std::mem::zeroed();
	        // vpi_get_vlog_info(&mut info);
            // dbg!(&info);
            // let product = std::ffi::CStr::from_ptr(info.product);
            // let version = std::ffi::CStr::from_ptr(info.version);
            // dbg!(product);
            // dbg!(version);

            // let mut iter = vpi_iterate(vpiModule as i32, ptr::null_mut());
            // let module = vpi_scan(iter);
            // vpi_free_object(iter);

            // let module_name = CStr::from_ptr(vpi_get_str(vpiName as i32, module));
            // dbg!(module_name);

            // let scope = vpi_handle (vpiScope as i32, module);
            // dbg!(scope);

            // let mut inputs = BTreeMap::new();
            // let mut outputs = BTreeMap::new();

            // for kind in [vpiNet] { //[vpiModule, vpiPort, vpiNet, vpiNetArray] {
            //     iter = vpi_iterate(kind as i32, scope);
            //     let mut net: vpiHandle = ptr::null_mut();
            //     while ptr::null_mut() != iter && ptr::null_mut() != {net = vpi_scan(iter); net} {
            //         let net_name = CStr::from_ptr(vpi_get_str(vpiName as i32, net))
            //             .to_str().unwrap().to_owned();
            //         eprint!("{} {}", kind, &net_name);
            //         let net_width = vpi_get(vpiSize as i32, net);
            //         let net_dir = vpi_get(vpiDirection as i32, net);
            //         eprintln!(" {}:{}",
            //             match net_dir as u32 {
            //                 vpiInput => "in",
            //                 vpiOutput => "out",
            //                 vpiInout => "inout",
            //                 vpiNoDirection => "no direction",
            //                 _ => panic!("unknown dir {}", net_dir),
            //             },
            //             net_width);
            //         match net_dir as u32 {
            //             vpiInput => {
            //                 inputs.insert(
            //                     net_name,
            //                     (kind, net_width, net));
            //             },
            //             vpiOutput => {
            //                 outputs.insert(
            //                     net_name,
            //                     (kind, net_width, net));
            //             },
            //             _ => {
            //                 vpi_free_object(net);
            //             }
            //         }
            //     }
            // }

            // let mut step_result;
            // loop {
            //     step_result = __ghdl_simulation_step();
            //     dbg!(step_result);
            //     if step_result >= 3 {
            //         break;
            //     }
            // }
        }
    }
}
