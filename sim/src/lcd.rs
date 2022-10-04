#![allow(non_upper_case_globals)]

use std::collections::VecDeque;

use crate::*;

#[allow(dead_code)]
enum Protocol { Bits4, Bits8 }

#[allow(dead_code)]
pub struct Lcd {
    bottom_nibble: Option<u8>,
    protocol: Protocol,
    busy: bool,
    pub queue: VecDeque<u8>,

}

impl Lcd {
    pub fn new() -> Self {
        Lcd {
            busy: false,
            queue: VecDeque::new(),
            protocol: Protocol::Bits8,
            bottom_nibble: None,
        }
    }
}

impl Device for Lcd {
    fn process(&mut self) {
        
    }

    fn ready_to_write(&self) -> bool {
        true
    }

    fn ready_to_read(&self) -> bool {
        false
    }

    fn read(&mut self) -> u8 {
        if self.busy { 0x80 } else { 0x00 }
    }

    fn write(&mut self, b: u8) {
        self.queue.push_back(b);
    }
}

include!(concat!(env!("OUT_DIR"), "/hd44780u.rs"));


#[cfg(test)]
mod tests {

    use std::convert::TryFrom;

    use ghdl_rs::*;

    use super::*;

    #[test]
    fn vhdl_iter() {
        let mut hd44780u = GhdlDevice::new(hd44780u_LIB_PATH, hd44780u_VPI_PATH);

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

        for (_name, (_kind, _width, net)) in &inputs {
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
