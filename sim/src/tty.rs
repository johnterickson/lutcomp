use std::collections::VecDeque;

use crate::*;

pub struct Tty {
    pub read_stdin: bool,
    pub block_for_stdin: bool,
    pub tty_in: VecDeque<u8>,
    pub tty_out: VecDeque<u8>,
}

impl Tty {
    pub fn new(read_stdin: bool, block_for_stdin: bool) -> Self {
        Tty {
            read_stdin,
            block_for_stdin,
            tty_in: VecDeque::new(),
            tty_out: VecDeque::new(),
        }
    }
}

impl Device for Tty {
    fn process(&mut self) {
        if self.read_stdin {
            let stdin_channel = NONBLOCKING_STDIN.lock().unwrap();
            let line = if self.block_for_stdin {
                stdin_channel.recv().map(Some).unwrap_or_default()
            } else {
                stdin_channel.try_recv().map(Some).unwrap_or_default()
            };

            if let Some(line) = line {
                for c in line.chars() {
                    self.tty_in.push_back(c as u8);
                }
            }
        }
    }

    fn ready_to_write(&self) -> bool {
        true
    }

    fn ready_to_read(&self) -> bool {
        !self.tty_in.is_empty()
    }

    fn read(&mut self) -> u8 {
        self.tty_in.pop_front().unwrap()
    }

    fn write(&mut self, b: u8) {
        self.tty_out.push_back(b);
        eprint!("{}", b as char);
    }
}