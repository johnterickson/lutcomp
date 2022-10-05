#![allow(non_upper_case_globals)]
#![allow(dead_code)]

use std::{ptr::{self}, convert::TryFrom, marker::PhantomPinned};

use ghdl_rs::*;

use crate::*;

enum Protocol { Bits4, Bits8 }

const CHAR_PIX_ROWS: usize = 10;
const CGROM_PIX_ROWS: usize = 8;
const CHAR_PIX_COLS: usize = 6;
const DISPLAY_CHAR_ROWS: usize = 2;
const DISPLAY_CHAR_COLS: usize = 20;
const DISPLAY_PIX_ROWS: usize = DISPLAY_CHAR_ROWS * CHAR_PIX_ROWS;
const DISPLAY_PIX_COLS: usize = DISPLAY_CHAR_COLS * CHAR_PIX_COLS;

const CHAR_ROWS: usize = 8;
pub const CHAR_COLS: usize = 5;

pub struct Lcd {
    protocol: Protocol,
    lazy_controller: Option<Box<HD44780U>>,
}

impl Lcd {
    pub fn new() -> Self {
        Lcd {
            protocol: Protocol::Bits8,
            lazy_controller: None,
        }
    }

    fn controller(&mut self) -> &mut HD44780U {
        if self.lazy_controller.is_none() {
            self.lazy_controller = Some(HD44780U::new());
            self.lazy_controller.as_mut().unwrap().init();
        }

        self.lazy_controller.as_mut().unwrap()
    }
}

impl Device for Lcd {
    fn process(&mut self) {
        if let Some(c) = &mut self.lazy_controller {
            let addr = c.ghdl.get_value_BinStr(c.nets["cgrom_addr"].handle);
            if let Some(addr) = StdLogic::from_str(addr) {
                let char = addr / CHAR_ROWS;
                assert!((0..=255).contains(&char));
                let row = addr % CHAR_ROWS;
                assert!((0..CHAR_ROWS).contains(&row));

                c.ghdl.put_value_int(c.nets["pix_val"].handle, CG_ROM[char][row] as i32);
            }
            c.toggle_clk_and_step(1);
        }
    }

    fn ready_to_write(&self) -> bool {
        // todo check busy
        true
    }

    fn ready_to_read(&self) -> bool {
        false
    }

    fn read(&mut self) -> u8 {
        0 // todo busy
    }

    fn write(&mut self, _b: u8) {
        // do nothing for now
    }
}

include!(concat!(env!("OUT_DIR"), "/hd44780u.rs"));

// pub struct HD44780U {
//     inner: Box<HD44780U_inner>
// }

// impl HD44780U {
//     pub fn new() -> Self {
//         HD44780U { inner: Box::new(HD44780U_inner::new()) }
//     }

//     extern "C" fn static_call_back(cb_data: *mut t_cb_data) -> i32 {
//         unsafe {
//             let hd = (*cb_data).user_data as *mut HD44780U_inner;
//             let hd = &mut *hd;
//             let cb_data = &*cb_data;
//             hd.call_back(cb_data)
//         }
//     }
// }

pub struct HD44780U {
    pub ghdl: Box<GhdlDevice>,
    pub nets: BTreeMap<String,Net>,
    pub by_handle: BTreeMap<vpiHandle,Net>,
    _pin: PhantomPinned,
}

impl HD44780U {
    fn call_back(&mut self, cb_data: &t_cb_data) -> i32 {
        assert_eq!(cb_data.reason, cbValueChange as i32);
        let net = &self.by_handle[&cb_data.obj];
        let name = &net.name;
        let str_val = self.ghdl.get_value_BinStr(net.handle);
        println!("cbValueChange {name} = {str_val}");

        match (name.as_str(), str_val) {
            ("pix_clk", "1") => {

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

        // ghdl.set_callback(|cb_data| {
        // });

        let mut boxed = Box::new(HD44780U {
            ghdl,
            nets,
            by_handle,
            _pin: PhantomPinned {}
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

    pub fn toggle_clk_and_step(&mut self, steps: usize) -> u32 {
        let mut result = u32::MAX;
        for _ in 0..steps {
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

            result = self.ghdl.simulation_step();
            if result >= 3 {
                break;
            }
        }
        result
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

        self.ghdl.put_value_int(self.nets["en"].handle, 0);
        self.ghdl.put_value_int(self.nets["db_in"].handle, 0x81);
        self.toggle_clk_and_step(4);
        // self.dump_nets();

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
        assert_eq!("00000101", self.ghdl.get_value_BinStr(self.nets["state"].handle));

        while self.ghdl.get_value_BinStr(self.nets["state"].handle) != "00000011" {
            let step_result = self.toggle_clk_and_step(1);
            if step_result >=3 {
                dbg!(step_result);
                break;
            }
        }

        assert_eq!("00000011", self.ghdl.get_value_BinStr(self.nets["state"].handle));
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hd44780u_init() {
        let mut hd44780u = HD44780U::new();
        hd44780u.init();
        hd44780u.dump_nets();
    }

    // #[test]
    // fn hd44780u_clear() {
    //     let mut hd44780u = HD44780U::new();
    //     hd44780u.init();
    //     hd44780u.dump_nets();

    //     hd44780u.clear();
    //     hd44780u.dump_nets();
    // }
}

fn generate_cgrom() -> [[u8;CHAR_ROWS];256] {
    use image::io::Reader as ImageReader;
    use az::CheckedCast;

    let mut chars = [[0u8;CHAR_ROWS];256];

    let img = concat!(env!("CARGO_MANIFEST_DIR"), "/../circuit/lcd_chars_b_w.png");

    let img = ImageReader::open(img).unwrap()
        .decode().unwrap();
    let samples = img.as_rgb8().unwrap();

    let zero_pix_left = 238.0;
    let zero_pix_right = 259.0;
    let zero_pix_cols = 5.0;
    let zero_pix_top = 153.0;
    let zero_pix_bottom = 190.0;
    let zero_pix_rows = 7.0;

    let pix_cols_per_square = (zero_pix_right - zero_pix_left) / (zero_pix_cols - 1.0);
    let pix_rows_per_square = (zero_pix_bottom - zero_pix_top) / (zero_pix_rows - 1.0);

    let zero_lo_nibble = 0;
    let zero_hi_nibble = 3;

    let ff_pix_left = 742.0;
    let ff_pix_top = 1086.0;

    let pix_cols_per_box = (ff_pix_left - zero_pix_left) / ((0xF - zero_hi_nibble) as f64);
    let pix_rows_per_box = (ff_pix_top - zero_pix_top) / ((0xF - zero_lo_nibble) as f64);

    let origin_pix_left = zero_pix_left - (zero_hi_nibble as f64 * pix_cols_per_box);
    let origin_pix_top = zero_pix_top - (zero_lo_nibble as f64 * pix_cols_per_box);

    for c in 0x0..=0xFFu8 {
        let lo_nibble = c & 0xF;
        let hi_nibble = (c>>4) & 0xF;

        let rows = &mut chars[c as usize];

        if hi_nibble != 0 {
            let left = origin_pix_left + (hi_nibble as f64) * pix_cols_per_box;
            let top = origin_pix_top + (lo_nibble as f64) * pix_rows_per_box;

            for row in 0..CHAR_ROWS {
                let row_byte = &mut rows[row as usize];
                let pix_row: u32 = (top + (row as f64) * pix_rows_per_square).round().checked_cast().unwrap();
                for col in 0..CHAR_COLS {
                    let pix_col: u32 = (left + ((4-col) as f64) * pix_cols_per_square).round().checked_cast().unwrap();
                    let pixel = samples.get_pixel(pix_col, pix_row);
                    let pixel: u32 = pixel.0.iter().map(|i| *i as u32).sum::<u32>();
                    let pixel = pixel / (3*128);
                    assert!((0..=1).contains(&pixel));
                    let pixel = 1 - pixel;
                    *row_byte |= (pixel << col) as u8;
                }
            }
        }
    }

    chars
}

use lazy_static::lazy_static;
lazy_static! {
    pub static ref CG_ROM: [[u8;CHAR_ROWS];256] = generate_cgrom();
}


#[cfg(test)]
mod cgrom_tests {
    use super::*;

    #[test]
    fn cgrom_generate() {
        let cgrom = &CG_ROM;
        let a = cgrom['A' as u8 as usize];
        assert_eq!("01110", &format!("{:05b}", a[0]));
        assert_eq!("10001", &format!("{:05b}", a[1]));
        assert_eq!("10001", &format!("{:05b}", a[2]));
        assert_eq!("10001", &format!("{:05b}", a[3]));
        assert_eq!("11111", &format!("{:05b}", a[4]));
        assert_eq!("10001", &format!("{:05b}", a[5]));
        assert_eq!("10001", &format!("{:05b}", a[6]));
        assert_eq!("00000", &format!("{:05b}", a[7]));
    }
}