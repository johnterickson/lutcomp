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
            
 
            //https://github.com/cocotb/cocotb/blob/e504999a1e28b855fe2d72e200a23e714b2af109/cocotb/share/lib/vhpi/VhpiImpl.cpp#L843

            // let mut info: s_vpi_vlog_info = std::mem::zeroed();
	        // vhpi_get_cb_info(&mut info);
            // dbg!(&info);
            // let product = std::ffi::CStr::from_ptr(info.product);
            // let version = std::ffi::CStr::from_ptr(info.version);
            // dbg!(product);
            // dbg!(version);

            for i in vhpiOneToOneT_vhpiAbstractLiteral..= vhpiOneToOneT_vhpiGenIndex {
                let root = vhpi_handle(i, ptr::null_mut());
                dbg!(root);
                if root != ptr::null_mut() {
                    break;
                }
            }

            let root = vhpi_handle_by_name("hd44780u".as_ptr() as *const i8, ptr::null_mut());
            dbg!(root);

            // let mut iter = vhpi_iterate(vpiModule as i32, ptr::null_mut());
            // let top = vpi_scan(iter);
            // vpi_free_object(iter);

            // let top_name = CStr::from_ptr(vpi_get_str(vpiName as i32, top));
            // dbg!(top_name);

            // let mut inputs = BTreeMap::new();
            // let mut outputs = BTreeMap::new();

            // for kind in 1..=125 {//[vpiModule, vpiPort, vpiNet, vpiNetArray]
            //     iter = vpi_iterate(kind, top);
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
