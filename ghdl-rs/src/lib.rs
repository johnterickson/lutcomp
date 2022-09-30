#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use std::ffi::CString;

    use super::*;
     
    #[test]
    fn round_trip_compression_decompression() {
        let progname = std::env::args().next().unwrap();
        // dbg!(&progname);
        let progname = CString::new(progname).unwrap();

        unsafe {
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

            #[no_mangle]
            pub unsafe extern "C" fn Startup() {
                {
                    let mut cb: t_cb_data = std::mem::zeroed();
                    cb.reason = cbStartOfSimulation as i32;
                    cb.cb_rtn = Some(StartOfSimulation);
                    cb.user_data = std::ptr::null_mut();

                    let registration = vpi_register_cb(&mut cb);
                    dbg!(&registration);
                }

                {
                    let mut cb: t_cb_data = std::mem::zeroed();
                    cb.reason = cbEndOfSimulation as i32;
                    cb.cb_rtn = Some(EndOfSimulation);
                    cb.user_data = std::ptr::null_mut();

                    let registration = vpi_register_cb(&mut cb);
                    dbg!(&registration);
                }
            }

            #[no_mangle]
            pub static _vlog_startup_routines: [Option<unsafe extern "C" fn()>; 2] =
                [Some(Startup), None];



            // let mut args = [
            //     progname.as_ptr() as *mut std::ffi::c_void
            // ];
            // //ghdl_main(args.len() as i32, args.as_ptr() as *mut *mut std::ffi::c_void);
            
            grt_init ();
            

            let args = [
                 progname.as_ptr(),
                //  "--vpi-trace".as_ptr() as *const i8,
                 std::ptr::null()
            ];
            grt_main_options (progname.as_ptr(), (args.len() - 1) as i32, args.as_ptr());
            grt_main_elab ();

            __ghdl_simulation_init();


            use std::ffi::CStr;
            use std::ptr;
            use std::collections::BTreeMap;
            
            Startup();

            // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc 
            // for VPI example

            let mut info: s_vpi_vlog_info = std::mem::zeroed();
	        vpi_get_vlog_info(&mut info);
            dbg!(&info);
            let product = std::ffi::CStr::from_ptr(info.product);
            let version = std::ffi::CStr::from_ptr(info.version);
            dbg!(product);
            dbg!(version);

            let mut iter = vpi_iterate(vpiModule as i32, ptr::null_mut());
            let top = vpi_scan(iter);
            vpi_free_object(iter);

            let top_name = CStr::from_ptr(vpi_get_str(vpiName as i32, top));
            dbg!(top_name);

            let mut inputs = BTreeMap::new();
            let mut outputs = BTreeMap::new();

            for kind in 1..=125 {//[vpiModule, vpiPort, vpiNet, vpiNetArray]
                iter = vpi_iterate(kind, top);
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

            let mut step_result;
            loop {
                step_result = __ghdl_simulation_step();
                dbg!(step_result);
                if step_result >= 3 {
                    break;
                }
            }
        }
    }
}
