#![allow(non_upper_case_globals)]
#![allow(dead_code)]

use std::{ptr::{self}, convert::TryFrom, marker::PhantomPinned};


use crate::*;

enum Protocol { Bits4, Bits8 }

const CHAR_PIX_ROWS: usize = 10;
const CGROM_PIX_ROWS: usize = 8;
const CHAR_PIX_COLS: usize = 6;
const DISPLAY_CHAR_ROWS: usize = 4;
const DISPLAY_CHAR_COLS: usize = 20;
const DISPLAY_PIX_ROWS: usize = DISPLAY_CHAR_ROWS * CHAR_PIX_ROWS;
const DISPLAY_PIX_COLS: usize = DISPLAY_CHAR_COLS * CHAR_PIX_COLS;

const ROW_START: [u8;DISPLAY_CHAR_ROWS] = [ 0x0, 0x40, 0x14, 0x54  ];

const CHAR_ROWS: usize = 8;
pub const CHAR_COLS: usize = 5;

pub struct Lcd {
    protocol: Protocol,
    #[cfg(unix)]
    lazy_controller: Option<Box<HD44780U>>,
}

impl Lcd {
    pub fn new() -> Self {
        Lcd {
            protocol: Protocol::Bits8,
            #[cfg(unix)]
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