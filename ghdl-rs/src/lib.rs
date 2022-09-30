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

            // #[no_mangle]
            // pub unsafe extern "C" fn StartOfSimulation(user_data: *const vhpiCbDataT) {
            //     println!("Hello!");
            //     dbg!(user_data);
            // }

            // #[no_mangle]
            // pub unsafe extern "C" fn Startup() {
            //     let mut cb: vhpiCbDataT = std::mem::zeroed();
            //     cb.reason = vhpiCbStartOfSimulation as i32;
            //     cb.cb_rtn = Some(StartOfSimulation);
            //     cb.user_data = std::ptr::null_mut();

            //     let registration = vhpi_register_cb(&mut cb, 0);
            //     dbg!(&registration);
            // }

            #[no_mangle]
            pub unsafe extern "C" fn StartOfSimulation(user_data: *mut t_cb_data) -> i32 {
                println!("Hello!");
                dbg!(user_data);
                0
            }

            #[no_mangle]
            pub unsafe extern "C" fn Startup() {
                let mut cb: t_cb_data = std::mem::zeroed();
                cb.reason = cbStartOfSimulation as i32;
                cb.cb_rtn = Some(StartOfSimulation);
                cb.user_data = std::ptr::null_mut();

                let registration = vpi_register_cb(&mut cb);
                dbg!(&registration);
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
                 progname.as_ptr()
            ];
            grt_main_options (progname.as_ptr(), 1, args.as_ptr());
            grt_main_elab ();

            Startup();

            __ghdl_simulation_init();

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
