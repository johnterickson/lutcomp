#![allow(dead_code)]

use std::{marker::PhantomPinned, ptr, convert::TryFrom};

use crate::*;
use ghdl_rs::*;
use lcd::*;

include!(concat!(env!("OUT_DIR"), "/hd44780u.rs"));

pub struct HD44780U {
    pub ghdl: Box<GhdlDevice>,
    pub nets: BTreeMap<String,Net>,
    pub by_handle: BTreeMap<vpiHandle,Net>,
    pub buffer: [[u8;DISPLAY_PIX_COLS];DISPLAY_PIX_ROWS],
    _pin: PhantomPinned,
}

impl HD44780U {
    fn call_back(&mut self, cb_data: &t_cb_data) -> i32 {
        assert_eq!(cb_data.reason, cbValueChange as i32);
        let net = &self.by_handle[&cb_data.obj];
        let name = &net.name;
        let str_val = self.ghdl.get_value_BinStr(net.handle);
        // println!("cbValueChange {name} = {str_val}");

        match (name.as_str(), str_val) {
            ("pix_clk", "1") => {
                let pix_addr = usize::from_str_radix(self.ghdl.get_value_BinStr(self.nets["pix_addr"].handle), 2).unwrap();
                let pix_row = pix_addr / DISPLAY_PIX_COLS;
                let pix_col = pix_addr % DISPLAY_PIX_COLS;
                let pix_val = usize::from_str_radix(self.ghdl.get_value_BinStr(self.nets["pix_val"].handle), 2).unwrap();
                // println!("[{},{}]={}", pix_row, pix_col, pix_val);
                self.buffer[pix_row][pix_col] = pix_val.try_into().unwrap();
            }
            ("cgrom_addr", addr) => {
                let addr_val = usize::from_str_radix(addr, 2).unwrap();
                let char = addr_val / CHAR_ROWS;
                let row = addr_val % CHAR_ROWS;
                let data = CG_ROM[char][row];
                // println!("cgrom_addr={}=0b{:b} ==> char=0x{}='{}',row={},data=0b{:06b}",
                //     addr, addr_val, char as u8, char as u8 as char, row, data);
                self.ghdl.put_value_int(self.nets["cgrom_data"].handle, data.into());
            }
            _ => {}
        }
        0
    }

    pub fn new() -> Box<Self> {
        // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc
        // for VPI example

        let ghdl = GhdlDevice::new(hd44780u_LIB_PATH, hd44780u_VPI_PATH);
        let mut ghdl = Box::new(ghdl);

        let module = {
            let iter = ghdl.iterate(vpiModule as i32, ptr::null_mut());
            let module = ghdl.scan(iter);
            ghdl.free_object(iter);
            module
        };

        // let module_name = ghdl.get_str(vpiName as i32, module);
        // dbg!(module_name);

        let scope = ghdl.handle(vpiScope as i32, module);
        // dbg!(scope);

        let mut nets = BTreeMap::new();
        let mut by_handle = BTreeMap::new();

        let iter = ghdl.iterate(vpiNet as i32, scope);
        let mut net: vpiHandle = ptr::null_mut();
        while ptr::null_mut() != iter
            && ptr::null_mut() != {
                net = ghdl.scan(iter);
                net
            }
        {
            let net = Net::from_net(net, &mut ghdl);
            nets.insert(net.name.to_owned(), net.clone());
            by_handle.insert(net.handle, net);
        }

        for net in nets.values() {
            if net.dir == vpiInput {
                continue;
            }
            ghdl.register_cb(
                cbValueChange as i32,
                net.handle);
        }

        let mut boxed = Box::new(HD44780U {
            ghdl,
            nets,
            by_handle,
            buffer: [[0u8;DISPLAY_PIX_COLS];DISPLAY_PIX_ROWS],
            _pin: PhantomPinned {},
        });

        unsafe {
            let boxed_ptr = boxed.as_mut() as *mut HD44780U;
            boxed.ghdl.set_callback(move |cb_data| {
                let boxed = &mut *boxed_ptr;
                boxed.call_back(cb_data)
            });
        }

        boxed
    }

    pub fn toggle_clk_and_step(&mut self, clk_steps: usize) -> i32 {
        let mut result = None;
        for _ in 0..clk_steps {
            let net = &self.nets["clk"];
            let clk_val = self.ghdl.get_value_BinStr(net.handle);
            let clk_val= clk_val.chars().next().unwrap();

            let new_val = match StdLogic::try_from(clk_val).unwrap() {
                StdLogic::HDL_0 => Some(1),
                StdLogic::HDL_1 => Some(0),
                _ => None,
            };

            if let Some(new_val) = new_val {
                self.ghdl.put_value_int(net.handle, new_val);
            }

            loop {
                let step_result = self.ghdl.simulation_step();
                // dbg!(step_result);
                // self.dump_nets();
                result = Some(step_result);
                // if step_result > GHDL_STEP_RESULT_NON_DELTA {
                //     return step_result;
                // }
                if step_result != GHDL_STEP_RESULT_DELTA {
                    break;
                }
            }
        }
        result.unwrap()
    }

    pub fn dump_nets(&mut self) {
        for (name, net) in &self.nets {
            let str_val = self.ghdl.get_value_BinStr(net.handle);
            println!(" {name}: {} {}[{}] = {}", net.dir, net.kind, net.width, str_val);
        }
    }

    pub fn init(&mut self) {
        for net in self.nets.values().filter(|n| n.dir == vpiInput) {
            self.ghdl.put_value_int(net.handle, 0);
        }
        self.toggle_clk_and_step(4);
        // self.dump_nets();

        assert_eq!("00000000", self.ghdl.get_value_BinStr(self.nets["state"].handle));

        self.ghdl.put_value_int(self.nets["db_in"].handle, 0x38);

        self.toggle_clk_and_step(4);
        // self.dump_nets();

        assert_eq!("00000000", self.ghdl.get_value_BinStr(self.nets["state"].handle));

        self.ghdl.put_value_int(self.nets["en"].handle, 1);
        self.toggle_clk_and_step(4);
        // self.dump_nets();

        assert_eq!("00000001", self.ghdl.get_value_BinStr(self.nets["state"].handle));

        self.ghdl.put_value_int(self.nets["en"].handle, 0);
        self.toggle_clk_and_step(4);
        self.ghdl.put_value_int(self.nets["en"].handle, 1);
        self.toggle_clk_and_step(4);
        // self.dump_nets();

        assert_eq!("00000011", self.ghdl.get_value_BinStr(self.nets["state"].handle));
    }

    pub fn clear(&mut self) {
        self.ghdl.put_value_int(self.nets["rs"].handle, 0);
        self.ghdl.put_value_int(self.nets["rw"].handle, 0);
        self.ghdl.put_value_int(self.nets["en"].handle, 0);
        self.ghdl.put_value_int(self.nets["db_in"].handle, 0x1); // clear command
        self.toggle_clk_and_step(4);
        self.ghdl.put_value_int(self.nets["en"].handle, 1);
        self.toggle_clk_and_step(4);
    }

    fn step_until_idle_with_cb<F: FnMut(&mut Self)>(&mut self, mut cb: F) {
        while self.ghdl.get_value_BinStr(self.nets["state"].handle) != "00000011" {
            cb(self);
            let step_result = self.toggle_clk_and_step(1);
            // dbg!(step_result);
            if step_result >= 4 {

                break;
            }
        }
    }

    fn step_until_idle(&mut self) {
        self.step_until_idle_with_cb(|_|{});
    }

    pub fn set_pos(&mut self, i: u8) {
        self.ghdl.put_value_int(self.nets["rs"].handle, 0);
        self.ghdl.put_value_int(self.nets["rw"].handle, 0);
        self.ghdl.put_value_int(self.nets["en"].handle, 0);
        self.ghdl.put_value_int(self.nets["db_in"].handle, 0x80 | i as i32);
        self.toggle_clk_and_step(1);

        self.ghdl.put_value_int(self.nets["en"].handle, 1);
        self.toggle_clk_and_step(1);
        self.ghdl.put_value_int(self.nets["en"].handle, 0);
    }

    pub fn write(&mut self, ch: char) {
        self.ghdl.put_value_int(self.nets["rs"].handle, 1);
        self.ghdl.put_value_int(self.nets["rw"].handle, 0);
        self.ghdl.put_value_int(self.nets["en"].handle, 0);
        self.ghdl.put_value_int(self.nets["db_in"].handle, ch as u8 as i32);
        self.toggle_clk_and_step(1);

        self.ghdl.put_value_int(self.nets["en"].handle, 1);
        self.toggle_clk_and_step(1);
        self.ghdl.put_value_int(self.nets["en"].handle, 0);
    }

    pub fn dump_display(&mut self) {
        for row in &self.buffer {
            for pix in row {
                print!("{}", if *pix == 0 { '.' } else { '*' });
            }
            println!();
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hd44780u_init() {
        let mut hd44780u = HD44780U::new();
        hd44780u.init();
        hd44780u.step_until_idle();
        hd44780u.dump_nets();
    }

    #[test]
    fn hd44780u_set_pos() {
        let mut hd44780u = HD44780U::new();
        hd44780u.init();
        hd44780u.step_until_idle();

        hd44780u.set_pos(2);
        hd44780u.step_until_idle();
        assert_eq!(
            "0000010",
            hd44780u.ghdl.get_value_BinStr(hd44780u.nets["ac"].handle));
    }

    #[test]
    fn hd44780u_write_char() {
        let mut hd44780u = HD44780U::new();
        hd44780u.init();
        hd44780u.step_until_idle();

        hd44780u.set_pos(0);
        hd44780u.step_until_idle();

        hd44780u.write('F');
        hd44780u.step_until_idle();

        let base_pix_row = 0;
        let base_pix_col = 0;

        let expected_rows = CG_ROM['F' as u8 as usize];
        for y in 0..CHAR_ROWS {
            for x in 0..CHAR_COLS {
                let expected = (expected_rows[y] >> (CHAR_COLS-x)) & 0x1;
                let actual = hd44780u.buffer[base_pix_row+y][base_pix_col+x];
                assert_eq!(expected, actual, "Mismatch at {},{}", x, y);
            }
        }
    }

    #[test]
    fn hd44780u_clear() {
        let mut hd44780u = HD44780U::new();
        hd44780u.init();
        hd44780u.step_until_idle();

        hd44780u.clear();
        let mut seen = BTreeSet::new();
        hd44780u.step_until_idle_with_cb(|hd| {
            seen.insert(hd.ghdl.get_value_BinStr(hd.nets["ac"].handle).to_owned());
        });

        for row in &ROW_START {
            for col in 0..DISPLAY_CHAR_COLS {
                let binary = format!("{:07b}", *row as usize + col);
                assert!(seen.contains(&binary));
            }
        }

        assert_eq!(
            "0000000",
            hd44780u.ghdl.get_value_BinStr(hd44780u.nets["ac"].handle));
    }
}