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