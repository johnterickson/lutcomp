extern crate packed_struct;
use packed_struct::prelude::*;


use strum::IntoEnumIterator;

use alu::*;
use common::*;
use std::{collections::VecDeque, convert::TryInto, fmt::Debug, io, sync::{Mutex, mpsc::{self, Receiver}}, thread};
use ucode::*;

fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();
        tx.send(buffer).unwrap();
    });
    rx
}


use lazy_static::lazy_static;
lazy_static! {
    pub static ref NONBLOCKING_STDIN: Mutex<Receiver<String>> = Mutex::new(spawn_stdin_channel());
}

const MEM_BITS: usize = 19;

const ROM_MIN: usize = 0;
const ROM_SIZE: usize = 1 << MEM_BITS;
const ROM_MAX: usize = ROM_MIN + ROM_SIZE - 1;

const RAM_MIN: usize = ROM_MAX + 1;
const RAM_SIZE: usize = 1 << MEM_BITS;
const RAM_MAX: usize = RAM_MIN + RAM_SIZE - 1;

pub struct Computer<'a> {
    rom: Vec<u8>,
    ram: Vec<u8>,
    tty_in: VecDeque<u8>,
    tty_out: VecDeque<u8>,
    alu_lut: &'a [u8],
    ucode_rom: &'a [(u8, &'static str, u32)],
    regs: [u8; 4],
    addr: [u8; 4],
    pc: [u8; 4],
    pcr: [u8; 4],
    upc: u8,
    alu: u8,
    flags: Flags,
    ir0: u8,
    in1: u8,
    print: bool,
}

impl<'a> Debug for Computer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upc:{:02x}", self.upc)?;
        write!(f, " pc:{:05x}", u32::from_le_bytes(self.pc))?;
        write!(f, " pcr:{:08x}", u32::from_le_bytes(self.pcr))?;
        write!(f, " regs:{:08x}", u32::from_le_bytes(self.regs))?;
        write!(f, " addr:{:05x}", u32::from_le_bytes(self.addr))?;
        write!(f, " ir0:{:02x}", self.ir0)?;
        write!(f, " in1:{:02x}", self.in1)?;
        write!(f, " flags:[{:?}]", self.flags)?;
        Ok(())
    }
}

impl<'a> Computer<'a> {
    pub fn new(rom: Vec<u8>) -> Computer<'a> {
        Computer::with_print(rom, false)
    }

    pub fn with_print(rom: Vec<u8>, print: bool) -> Computer<'a> {
        let c = Computer {
            rom,
            ram: vec![0u8; 1 << MEM_BITS],
            tty_in: VecDeque::new(),
            tty_out: VecDeque::new(),
            alu_lut: &alu::ALU,
            ucode_rom: &ucode::UCODE,
            regs: [0u8; 4],
            addr: [0u8; 4],
            pc: [0u8; 4],
            pcr: [0u8; 4],
            upc: 0,
            alu: 0,
            flags: Flags::empty(),
            ir0: 0,
            in1: 0,
            print,
        };

        assert_eq!(c.alu_lut.len(), 1 << MEM_BITS);
        assert_eq!(c.ucode_rom.len(), 1 << MEM_BITS);
        c
    }

    fn mem_slice_mut(&mut self, addr_bus: u32, len: usize) -> &mut [u8] {
        let addr_bus = addr_bus & 0x0FFFFF;
        match addr_bus as usize {
            a if ROM_MIN <= a && a <= ROM_MAX => &mut self.rom[a - ROM_MIN..(len + a - ROM_MIN)],
            a if RAM_MIN <= a && a <= RAM_MAX => &mut self.ram[a - RAM_MIN..(len + a - RAM_MIN)],
            _ => panic!(format!("Invalid address: {:08x}", addr_bus)),
        }
    }

    fn mem_byte_mut(&mut self, addr_bus: u32) -> &mut u8 {
        &mut self.mem_slice_mut(addr_bus, 1)[0]
    }

    pub fn mem_word_mut(&mut self, addr_bus: u32) -> &mut [u8; 4] {
        let slice: &mut [u8; 4] = self.mem_slice_mut(addr_bus, 4).try_into().unwrap();
        slice
    }

    pub fn step(&mut self) -> bool {
        if self.print {
            println!("\n{:?}", &self);
        }

        let urom_entry = MicroEntry {
            flags: self.flags.bits().into(),
            instruction: self.ir0,
        };
        let mut urom_addr = u16::from_le_bytes(urom_entry.pack_lsb()) as usize;
        urom_addr <<= 7;
        urom_addr += self.upc as usize;

        let opcode = Opcode::iter()
            .filter(|o| *o as u8 == urom_entry.instruction)
            .next();
        if self.print {
            println!(
                "urom_addr {:05x} = {:?} {:?} + {:02x}",
                urom_addr, urom_entry, opcode, self.upc);
        }

        let i0 = self.ucode_rom[urom_addr];
        let i1 = self.ucode_rom[urom_addr + 1];
        let urom_op = MicroOp::unpack(&[i0.0, i1.0]).unwrap();
        if self.print {
            println!("urom_op: {:?} {}:{}", urom_op, i0.1, i0.2);
        }

        let addr_bus = u32::from_le_bytes(match urom_op.address_bus_out {
            AddressBusOutputLevel::Addr => self.addr,
            AddressBusOutputLevel::Pc => self.pc,
        });

        let data_bus = match urom_op.data_bus_out {
            DataBusOutputLevel::Alu => Some(self.alu),
            DataBusOutputLevel::Halt => return false,
            DataBusOutputLevel::Imm => Some(*urom_op.immediate),
            DataBusOutputLevel::Mem => {
                if self.print {
                    println!("addr_bus: {:08x}", addr_bus);
                }
                Some(*self.mem_byte_mut(addr_bus))
            }
            DataBusOutputLevel::Next => {
                self.upc = 0;
                return true;
            }
            DataBusOutputLevel::Pc => {
                self.pc = self.pcr;
                None
            }
            DataBusOutputLevel::TtyIn => {
                if self.print {
                    let stdin_channel = NONBLOCKING_STDIN.lock().unwrap();
                    if let Ok(line) = stdin_channel.try_recv() {
                        for c in line.chars() {
                            self.tty_in.push_back(c as u8);
                        }
                    }
                }
                let peek = self.tty_in.front();
                Some(peek.map_or(0x00, |c| 0x80 | *c))
            }
            DataBusOutputLevel::W => Some(self.regs[0]),
            DataBusOutputLevel::X => Some(self.regs[1]),
            DataBusOutputLevel::Y => Some(self.regs[2]),
            DataBusOutputLevel::Z => Some(self.regs[3]),
            DataBusOutputLevel::Reserved7 => return false,
        };

        if let Some(data_bus) = data_bus {
            if self.print {
                println!("data_bus: {:02x}", data_bus);
            }
        }

        match urom_op.data_bus_load {
            DataBusLoadEdge::Addr0 => self.addr[0] = data_bus.unwrap(),
            DataBusLoadEdge::Addr1 => self.addr[1] = data_bus.unwrap(),
            DataBusLoadEdge::Addr2 => self.addr[2] = data_bus.unwrap(),
            DataBusLoadEdge::Alu => {
                let lut_entry = LutEntry {
                    in1: self.in1,
                    in2: data_bus.unwrap(),
                    op: urom_op.alu_opcode,
                };
                let lut_entry_index = lut_entry.to_index() as usize;
                if self.print {
                    print!("lut_entry:{:05x}={:?} => ", lut_entry_index, lut_entry);
                }
                let lut_output = self.alu_lut[lut_entry_index];
                if self.print {
                    println!("{:02x}", lut_output);
                }
                self.alu = lut_output;
            }
            DataBusLoadEdge::Flags => {
                self.flags = Flags::from_bits_truncate(data_bus.unwrap());
            }
            DataBusLoadEdge::In1 => self.in1 = data_bus.unwrap(),
            DataBusLoadEdge::IR0 => self.ir0 = data_bus.unwrap(),
            DataBusLoadEdge::Mem => {
                if self.print {
                    println!("addr_bus: {:08x}", addr_bus);
                }
                *self.mem_byte_mut(addr_bus) = data_bus.unwrap();
            }
            DataBusLoadEdge::PcInc => {
                self.pc = (u32::from_le_bytes(self.pc) + 1).to_le_bytes();
            }
            DataBusLoadEdge::PcR => {
                self.pcr = self.addr;
            }
            DataBusLoadEdge::TtyIn => {
                let _ = self.tty_in.pop_front();
            }
            DataBusLoadEdge::TtyOut => {
                self.tty_out.push_back(data_bus.unwrap());
                if self.print {
                    eprint!("{}", data_bus.unwrap() as char);
                }
            }
            DataBusLoadEdge::W => self.regs[0] = data_bus.unwrap(),
            DataBusLoadEdge::X => self.regs[1] = data_bus.unwrap(),
            DataBusLoadEdge::Y => self.regs[2] = data_bus.unwrap(),
            DataBusLoadEdge::Z => self.regs[3] = data_bus.unwrap(),
        }

        if urom_op.data_bus_out == DataBusOutputLevel::Pc {
            self.pc = self.pcr;
        }

        self.upc += 2;

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryInto;

    #[test]
    fn halt() {
        let mut rom = Vec::new();
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}
    }

    #[test]
    fn loadimm8() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(4);
        rom.push(0xef);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(0xef, *c.mem_byte_mut(0x80004));
    }

    #[test]
    fn andimm8() {
        let mut rom = Vec::new();
        rom.push(Opcode::AndImm8 as u8);
        rom.push(0x4);
        rom.push(0b0011);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80004) = 0b0101;

        while c.step() {}

        assert_eq!(0b0001, *c.mem_byte_mut(0x80004));
    }

    #[test]
    fn orimm8() {
        let mut rom = Vec::new();
        rom.push(Opcode::OrImm8 as u8);
        rom.push(0x4);
        rom.push(0b0011);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80004) = 0b0101;

        while c.step() {}

        assert_eq!(0b0111, *c.mem_byte_mut(0x80004));
    }

    #[test]
    fn xorimm8_nonzero() {
        let mut rom = Vec::new();
        rom.push(Opcode::XorImm8 as u8);
        rom.push(0x4);
        rom.push(0b0011);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80004) = 0b0101;

        while c.step() {}

        assert_eq!(0b0110, *c.mem_byte_mut(0x80004));
        assert_eq!(c.flags.contains(Flags::ZERO), false);
    }

    #[test]
    fn xorimm8_zero() {
        let mut rom = Vec::new();
        rom.push(Opcode::XorImm8 as u8);
        rom.push(0x4);
        rom.push(0b01010101);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80004) = 0b01010101;

        while c.step() {}

        assert_eq!(0b0, *c.mem_byte_mut(0x80004));
        assert_eq!(c.flags.contains(Flags::ZERO), true);
    }

    #[test]
    fn loadimm32() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0xef);
        rom.push(0xcd);
        rom.push(0xab);
        rom.push(0x89);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(0x89abcdef, u32::from_le_bytes(*c.mem_word_mut(0x80004)));
    }

    #[test]
    fn ttyout() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(0);
        rom.push(0x41);
        rom.push(0x42);
        rom.push(0x43);
        rom.push(0x0A);
        rom.push(Opcode::TtyOut as u8);
        rom.push(0x00);
        rom.push(Opcode::TtyOut as u8);
        rom.push(0x01);
        rom.push(Opcode::TtyOut as u8);
        rom.push(0x02);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        let chars = c.tty_out.iter().copied().collect();
        assert_eq!("ABC", String::from_utf8(chars).unwrap().as_str());
    }

    #[test]
    fn ttyin() {
        let mut rom = Vec::new();
        rom.push(Opcode::TtyIn as u8);
        rom.push(0);
        rom.push(Opcode::TtyIn as u8);
        rom.push(1);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.tty_in.push_front('A' as u8);

        while c.step() {}

        assert_eq!(0x80 | ('A' as u8), *c.mem_byte_mut(0x80000));
        assert_eq!(0, *c.mem_byte_mut(0x80001));
    }

    #[test]
    fn load8() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0x34);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Load8 as u8);
        rom.push(4);
        rom.push(8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.mem_word_mut(0x81234)
            .copy_from_slice(&u32::to_le_bytes(0xDEADBEEF));

        while c.step() {}

        assert_eq!(0xEF, *c.mem_byte_mut(0x80008));
    }

    #[test]
    fn store8() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(0x0);
        rom.push(0xAA);
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0x34);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Store8 as u8);
        rom.push(0);
        rom.push(4);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(0xAA, *c.mem_byte_mut(0x80000));
        assert_eq!(0x81234, u32::from_le_bytes(*c.mem_word_mut(0x80004)));
        assert_eq!(0xAA, *c.mem_byte_mut(0x81234));
    }

    #[test]
    fn load32() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0x34);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Load32 as u8);
        rom.push(4);
        rom.push(8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.mem_word_mut(0x81234)
            .copy_from_slice(&u32::to_le_bytes(0xDEADBEEF));

        while c.step() {}

        assert_eq!(0xDEADBEEF, u32::from_le_bytes(*c.mem_word_mut(0x80008)));
    }

    #[test]
    fn store32() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(0x0);
        rom.push(0xEF);
        rom.push(0xCD);
        rom.push(0xAB);
        rom.push(0x89);
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0x34);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Store32Part1 as u8);
        rom.push(0);
        rom.push(4);
        rom.push(Opcode::Store32Part2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
        assert_eq!(0x81234, u32::from_le_bytes(*c.mem_word_mut(0x80004)));
        assert_eq!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x81234)));
    }

    #[test]
    fn storeimm32() {
        let mut rom = Vec::new();
        rom.push(Opcode::StoreImm32 as u8);
        rom.push(0xC);
        rom.push(0xEF);
        rom.push(0xCD);
        rom.push(0xAB);
        rom.push(0x89);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_word_mut(0x8000C) = u32::to_le_bytes(0x81234);

        while c.step() {}

        assert_eq!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x81234)));
    }

    #[test]
    fn or32() {
        let mut rom = Vec::new();
        rom.push(Opcode::Or32 as u8);
        rom.push(0x4);
        rom.push(0x8);
        rom.push(0xc);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
        c.mem_word_mut(0x80004)
            .copy_from_slice(&u32::to_le_bytes(0x0F0F0F0F));
        c.mem_word_mut(0x80008)
            .copy_from_slice(&u32::to_le_bytes(0x40302010));

        while c.step() {}

        assert_eq!(0x4F3F2F1F, u32::from_le_bytes(*c.mem_word_mut(0x8000c)));
    }

    fn add8_helper(carry_in: bool, a: u8, b: u8) {
        println!("testing {}+{}+{}", carry_in, a, b);

        let mut rom = Vec::new();
        rom.push(Opcode::Add8 as u8);
        rom.push(0x0);
        rom.push(0x1);
        rom.push(0x2);
        rom.push(Opcode::Halt as u8);

        let mut sum = (a as u16) + (b as u16);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80000) = a;
        *c.mem_byte_mut(0x80001) = b;

        if carry_in {
            c.flags |= Flags::CARRY;
            sum += 1;
        }

        while c.step() {}

        assert_eq!(
            (sum & 0xFF) as u8,
            *c.mem_byte_mut(0x80002)
        );
        assert_eq!(
            sum > 0xFF,
            c.flags.contains(Flags::CARRY)
        );
    }

    #[test]
    fn add8() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for carry_in in &[false, true] {
            for a in &values {
                for b in &values {
                    add8_helper(*carry_in, *a, *b);
                }
            }
        }
    }

    fn mul8_helper(a: u8, b: u8) {
        println!("testing {}*{}", a, b);

        let mut rom = Vec::new();
        rom.push(Opcode::Mul8Part1 as u8);
        rom.push(0x4);
        rom.push(0x5);
        rom.push(Opcode::Mul8Part2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80004) = a;
        *c.mem_byte_mut(0x80005) = b;

        while c.step() {}

        assert_eq!(
            (a as u32) * (b as u32),
            u32::from_le_bytes(*c.mem_word_mut(0x80000))
        );
    }



    #[test]
    fn mul8() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for a in &values {
            for b in &values {
                mul8_helper(*a, *b);
            }
        }
    }

    #[test]
    fn add32() {
        let mut rom = Vec::new();
        rom.push(Opcode::Add32Part1 as u8);
        rom.push(0x4);
        rom.push(0x8);
        rom.push(0xc);
        rom.push(Opcode::Add32Part2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
        c.ram.as_mut_slice()[4..8].copy_from_slice(&u32::to_le_bytes(0x12345678));
        c.ram.as_mut_slice()[8..0xc].copy_from_slice(&u32::to_le_bytes(0x11111111));

        while c.step() {}

        assert_eq!(
            0x23456789,
            u32::from_le_bytes(c.ram[0xc..0x10].try_into().unwrap())
        );
    }

    #[test]
    fn copy32() {
        let mut rom = Vec::new();
        rom.push(Opcode::Copy32 as u8);
        rom.push(0x4);
        rom.push(0x8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
        c.mem_word_mut(0x80004)
            .copy_from_slice(&u32::to_le_bytes(0x12345678));

        while c.step() {}

        assert_eq!(0x12345678, u32::from_le_bytes(*c.mem_word_mut(0x80008)));
    }

    fn add_tester(carry_in: bool, in1: u32, in2: u32, sum: u32, carry_out: bool) {
        add_tester_with_carry_out(carry_in, in1, in2, sum, carry_out);
        add_tester_with_carry_out(carry_in, in2, in1, sum, carry_out);
        add_tester_without_carry_out(carry_in, in1, in2, sum);
        add_tester_without_carry_out(carry_in, in2, in1, sum);
    }

    fn add_tester_with_carry_out(carry_in: bool, in1: u32, in2: u32, sum: u32, carry_out: bool) {
        println!(
            "test case {:?} + {:08x} + {:08x} -> {:08x} + {:?}",
            carry_in, in1, in2, sum, carry_out
        );

        let mut rom = Vec::new();
        rom.push(Opcode::Add32Part1 as u8);
        rom.push(0);
        rom.push(4);
        rom.push(8);
        rom.push(Opcode::Add32Part2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.mem_word_mut(0x80000).copy_from_slice(&in1.to_le_bytes());
        c.mem_word_mut(0x80004).copy_from_slice(&in2.to_le_bytes());

        if carry_in {
            c.flags |= Flags::CARRY;
        }

        while c.step() {}

        assert_eq!(sum, u32::from_le_bytes(*c.mem_word_mut(0x80008)));
        assert_eq!(carry_out, c.flags.contains(Flags::CARRY));
    }

    fn add_tester_without_carry_out(carry_in: bool, in1: u32, in2: u32, sum: u32) {
        println!(
            "test case {:?} + {:08x} + {:08x} -> {:08x}",
            carry_in, in1, in2, sum
        );

        let mut rom = Vec::new();

        rom.push(Opcode::Add32NoCarryOut as u8);
        rom.push(0);
        rom.push(4);
        rom.push(8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.mem_word_mut(0x80000).copy_from_slice(&in1.to_le_bytes());
        c.mem_word_mut(0x80004).copy_from_slice(&in2.to_le_bytes());

        if carry_in {
            c.flags |= Flags::CARRY;
        }

        while c.step() {}

        assert_eq!(in1, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
        assert_eq!(in2, u32::from_le_bytes(*c.mem_word_mut(0x80004)));
        assert_eq!(sum, u32::from_le_bytes(*c.mem_word_mut(0x80008)));
    }

    #[test]
    fn add_no_carry() {
        add_tester(false, 1, 2, 3, false);
        add_tester(false, 0x01010101, 0x02020202, 0x03030303, false);
        add_tester(false, 0x01010101, 0xFEFEFEFE, 0xFFFFFFFF, false);
    }

    #[test]
    fn add_internal_carry() {
        add_tester(false, 0xFF, 1, 0x100, false);
        add_tester(false, 0xFFFF, 1, 0x10000, false);
        add_tester(false, 0xFFFFFF, 1, 0x1000000, false);
        add_tester(false, 0xFFFFFFFF, 1, 0x0, true);
    }

    #[test]
    fn add_incoming_carry() {
        add_tester(true, 1, 2, 4, false);
        add_tester(true, 0xFF, 0, 0x100, false);
    }

    #[test]
    fn add32_coverage() {
        use itertools::Itertools;
        let values = &[0x0, 0x1, 0xFF];
        for carry_in in &[false, true] {
            for (((a1, a2), a3), a4) in values
                .iter()
                .cartesian_product(values)
                .cartesian_product(values)
                .cartesian_product(values)
            {
                let a = u32::from_le_bytes([*a1, *a2, *a3, *a4]);
                for (((b1, b2), b3), b4) in values
                    .iter()
                    .cartesian_product(values)
                    .cartesian_product(values)
                    .cartesian_product(values)
                {
                    let b = u32::from_le_bytes([*b1, *b2, *b3, *b4]);
                    if a >= b {
                        let sum = a as u64 + b as u64 + *carry_in as u64;
                        let carry_out = sum > u32::max_value() as u64;
                        let sum = (sum & 0xFFFFFFFF) as u32;
                        add_tester(*carry_in, a, b, sum, carry_out);
                    }
                }
            }
        }
    }
}
