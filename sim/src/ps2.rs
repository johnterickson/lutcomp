use std::collections::VecDeque;

use crate::*;

pub struct Ps2Keyboard {
    pub queue: VecDeque<u8>,
}

impl Device for Ps2Keyboard {
    fn process(&mut self) {
    }

    fn ready_to_write(&self) -> bool {
        false
    }

    fn ready_to_read(&self) -> bool {
        !self.queue.is_empty()
    }

    fn read(&mut self) -> u8 {
        self.queue.pop_front().unwrap()
    }

    fn write(&mut self, _b: u8) {
        panic!();
    }
}