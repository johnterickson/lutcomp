#![allow(non_upper_case_globals)]

use std::{collections::VecDeque, ptr, convert::TryFrom};

use ghdl_rs::*;

use crate::*;

#[allow(dead_code)]
enum Protocol { Bits4, Bits8 }

#[allow(dead_code)]
pub struct Lcd {
    bottom_nibble: Option<u8>,
    protocol: Protocol,
    busy: bool,
    pub queue: VecDeque<u8>,
    // controller: HD44780U,

}

impl Lcd {
    pub fn new() -> Self {
        // let mut controller = HD44780U::new();
        // controller.init();
        Lcd {
            busy: false,
            queue: VecDeque::new(),
            protocol: Protocol::Bits8,
            bottom_nibble: None,
            // controller: controller,
        }
    }
}

impl Device for Lcd {
    fn process(&mut self) {
        // self.controller.toggle_clk_and_step(1);
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


pub struct HD44780U {
    pub ghdl: GhdlDevice,
    pub nets: BTreeMap<String,Net>,
}

impl HD44780U {
    pub fn new() -> Self {
        // see https://gitlab.ensta-bretagne.fr/bollenth/ghdl-vpi-virtual-board/-/blob/master/src/vpi.cc
        // for VPI example

        let mut ghdl = GhdlDevice::new(hd44780u_LIB_PATH, hd44780u_VPI_PATH);

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

        let iter = ghdl.iterate(vpiNet as i32, scope);
        let mut net: vpiHandle = ptr::null_mut();
        while ptr::null_mut() != iter
            && ptr::null_mut() != {
                net = ghdl.scan(iter);
                net
            }
        {
            let net = Net::from_net(net, &mut ghdl);
            nets.insert(net.name(&mut ghdl).to_owned(), net);
        }

        HD44780U {
            ghdl,
            nets
        }
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
            println!(" {name}: {}[{}] = {}", net.kind, net.width, str_val);
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
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hd44780u_init() {
        let mut _hd44780u = HD44780U::new();
    }
}

const CHAR_ROWS: usize = 8;
pub const CHAR_COLS: usize = 5;

fn generate_cgrom() -> [[u8;CHAR_ROWS];256] {
    use image::io::Reader as ImageReader;
    use az::CheckedCast;

    let mut chars = [[0u8;CHAR_ROWS];256];

    let img = concat!(env!("CARGO_MANIFEST_DIR"), "/../circuit/lcd_chars_b_w.png");
    dbg!(img);

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