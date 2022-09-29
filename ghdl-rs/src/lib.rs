#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use std::{ptr::null, ffi::CString};

    use super::*;
    
    #[test]
    fn round_trip_compression_decompression() {
        let progname = std::env::args().next().unwrap();
        dbg!(&progname);
        let progname = CString::new(progname).unwrap();

        let args = [
            null()
        ];

        unsafe {
            grt_init ();
            grt_main_options (progname.as_ptr(), 1, args.as_ptr());
            grt_main_elab ();
            __ghdl_simulation_init();
        }
    }
}
