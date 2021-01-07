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
    pub pc: [u8; 4],
    upc: u8,
    alu: u8,
    pub flags: Flags,
    pub ir0: u8,
    in1: u8,
    print: bool,
}

impl<'a> Debug for Computer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upc:{:02x}", self.upc)?;
        write!(f, " pc:{:05x}", u32::from_le_bytes(self.pc))?;
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
            ram: Vec::new(),
            tty_in: VecDeque::new(),
            tty_out: VecDeque::new(),
            alu_lut: &alu::ALU,
            ucode_rom: &ucode::UCODE,
            regs: [0u8; 4],
            addr: [0u8; 4],
            pc: [0u8; 4],
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
        self.try_mem_slice_mut(addr_bus, len).unwrap()
    }

    pub fn try_mem_slice_mut(&mut self, addr_bus: u32, len: usize) -> Option<&mut [u8]> {
        let addr_bus = addr_bus & 0x0FFFFF;
        Some(match addr_bus as usize {
            a if ROM_MIN <= a && a + len < ROM_MAX => {
                let end_index = len + a - ROM_MIN;
                if end_index > self.rom.len() {
                    self.rom.extend(std::iter::repeat(0xCCu8).take(end_index - self.rom.len()));
                }
                &mut self.rom[a - ROM_MIN..(len + a - ROM_MIN)]
            },
            a if RAM_MIN <= a && a + len < RAM_MAX => {
                let end_index = len + a - RAM_MIN;
                if end_index > self.ram.len() {
                    self.ram.extend(std::iter::repeat(0xCCu8).take(end_index - self.ram.len()));
                }
                &mut self.ram[a - RAM_MIN..(len + a - RAM_MIN)]
            },
            _ => return None,
        })
    }

    pub fn mem_byte_mut(&mut self, addr_bus: u32) -> &mut u8 {
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
            DataBusOutputLevel::PcSPE => {
                //self.pc = self.pcr;
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
                self.pc = if urom_op.data_bus_out == DataBusOutputLevel::PcSPE {
                    addr_bus.to_le_bytes()
                } else {
                    (u32::from_le_bytes(self.pc) + 1).to_le_bytes()
                };
            }
            DataBusLoadEdge::TtyIn => {
                let _ = self.tty_in.pop_front();
            }
            DataBusLoadEdge::TtyOut => {
                self.tty_out.push_back(data_bus.unwrap());
                eprint!("{}", data_bus.unwrap() as char);
            }
            DataBusLoadEdge::W => self.regs[0] = data_bus.unwrap(),
            DataBusLoadEdge::X => self.regs[1] = data_bus.unwrap(),
            DataBusLoadEdge::Y => self.regs[2] = data_bus.unwrap(),
            DataBusLoadEdge::Z => self.regs[3] = data_bus.unwrap(),
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

        let mut c = Computer::with_print(rom, false);

        while c.step() {}
    }

    #[test]
    fn jmpimm() {
        let mut rom = Vec::new();
        rom.push(Opcode::JmpImm as u8);
        rom.push(0x07);
        rom.push(0x00);
        rom.push(0x00);
        for _ in 0..10 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(7, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jz_zero() {
        let mut rom = Vec::new();
        rom.push(Opcode::JzImm as u8);
        rom.push(0x07);
        rom.push(0x00);
        rom.push(0x00);
        for _ in 0..10 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::new(rom);

        c.flags |= Flags::ZERO;

        while c.step() {}

        assert_eq!(7, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jz_nonzero() {
        let mut rom = Vec::new();
        rom.push(Opcode::JzImm as u8);
        rom.push(0x07);
        rom.push(0x00);
        rom.push(0x00);
        for _ in 0..10 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(4, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jmpreg() {
        let mut rom = Vec::new();
        rom.push(Opcode::JmpReg as u8);
        rom.push(0x04);
        for _ in 0..20 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::with_print(rom, false);

        *c.mem_word_mut(0x80004) = u32::to_le_bytes(7);

        while c.step() {}

        assert_eq!(7, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jmpmem() {
        let mut rom = Vec::new();
        rom.push(Opcode::JmpMem as u8);
        rom.push(0x04);
        for _ in 0..20 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::with_print(rom, false);

        *c.mem_word_mut(0x80004) = u32::to_le_bytes(0x80008);
        *c.mem_word_mut(0x80008) = u32::to_le_bytes(7);

        while c.step() {}

        assert_eq!(7, u32::from_le_bytes(c.pc));
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

    fn imm8(op: Opcode, reg_value: u8, imm: u8, result: u8) {
        let reg_index = 3;
        let mut rom = Vec::new();
        rom.push(op as u8);
        rom.push(reg_index);
        rom.push(imm);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        let reg_addr = 0x80000 + reg_index as u32;
        *c.mem_byte_mut(reg_addr) = reg_value;

        while c.step() {}

        assert_eq!(result, *c.mem_byte_mut(reg_addr));
        assert_eq!(c.flags.contains(Flags::CARRY), false);
        assert_eq!(c.flags.contains(Flags::ZERO), result == 0);
    }

    fn run(flags: Flags, op: Opcode, args: &[(u8, u8)]) -> Computer {
        let mut rom = Vec::new();
        rom.push(op as u8);
        for (i, arg) in args.iter().enumerate() {
            rom.push(arg.0);
        }
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::with_print(rom, true);
        for arg in args {
            *c.mem_byte_mut(0x80000 + arg.0 as u32) = arg.1;
        }

        c.flags = flags;

        while c.step() {}

        c
    }

    fn reg8(op: Opcode, carry_in: bool, arg1_value: u8, arg2_value: u8, result: u8, carry_out: bool) {
        let reg_a = 3;
        let reg_b = 4;
        let reg_c = 5;

        let args = [(reg_a,arg1_value),(reg_b, arg2_value), (reg_c, 0xFF)];
        let mut c = run(
            if carry_in {Flags::CARRY} else { Flags::empty() },
            op,
            &args
        );

        assert_eq!(result, *c.mem_byte_mut(0x80000 + reg_c as u32));
        assert_eq!(c.flags.contains(Flags::CARRY), carry_out);
        assert_eq!(c.flags.contains(Flags::ZERO), result == 0);
    }

    #[test]
    fn and8() {
        for a in 0..=3 {
            for b in 0..=3 {
                imm8(Opcode::AndImm8, a, b, a & b);
                reg8(Opcode::And8, false, a, b, a & b, false);
            }
        }
    }

    #[test]
    fn or8() {
        for a in 0..=3 {
            for b in 0..=3 {
                imm8(Opcode::OrImm8, a, b, a | b);
                reg8(Opcode::Or8, false, a, b, a | b, false);
            }
        }
    }

    #[test]
    fn xor8() {
        for a in 0..=3 {
            for b in 0..=3 {
                imm8(Opcode::XorImm8, a, b, a ^ b);
                reg8(Opcode::Xor8, false, a, b, a ^ b, false);
            }
        }
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

    fn modify8(op: Opcode, input: u8, output: u8) {
        let mut rom = Vec::new();
        rom.push(op as u8);
        rom.push(3);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        *c.mem_byte_mut(0x80003) = input;

        while c.step() {}

        assert_eq!(output, *c.mem_byte_mut(0x80003));
    }

    #[test]
    fn invert() {
        for a in 0..=8 {
            modify8(Opcode::Invert8, a, !a);
        }
    }

    #[test]
    fn negate() {
        for a in 0..=8 {
            modify8(Opcode::Negate8, a, ((-1*(a as i16)) & 0xFF) as u8);
        }
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
    fn push8() {
        let mut rom = Vec::new();
        rom.push(Opcode::Push8 as u8);
        rom.push(0x1);
        rom.push(Opcode::Push8 as u8);
        rom.push(0x2);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::with_print(rom, false);

        *c.mem_byte_mut(0x80001) = 0xAA;
        *c.mem_byte_mut(0x80002) = 0xBB;
        // set up stack pointer
        *c.mem_word_mut(0x80000 + REG_SP as u32) = u32::to_le_bytes(0x81001);

        while c.step() {}

        assert_eq!(0x80FFF, u32::from_le_bytes(*c.mem_word_mut(0x80000 + REG_SP as u32)));
        assert_eq!(0xAA, *c.mem_byte_mut(0x81000));
        assert_eq!(0xBB, *c.mem_byte_mut(0x80FFF));
    }

    #[test]
    fn pop8() {
        let mut rom = Vec::new();
        rom.push(Opcode::Pop8 as u8);
        rom.push(0x1);
        rom.push(Opcode::Pop8 as u8);
        rom.push(0x2);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::with_print(rom, false);

        *c.mem_byte_mut(0x80FFF) = 0xAA;
        *c.mem_byte_mut(0x81000) = 0xBB;

        // set up stack pointer
        *c.mem_word_mut(0x80000 + REG_SP as u32) = u32::to_le_bytes(0x80FFF);

        while c.step() {}

        assert_eq!(0x81001, u32::from_le_bytes(*c.mem_word_mut(0x80000 + REG_SP as u32)));
        assert_eq!(0xAA, *c.mem_byte_mut(0x80001));
        assert_eq!(0xBB, *c.mem_byte_mut(0x80002));
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

    #[test]
    fn cmp8() {
        let values = [0u8, 1, 2, 3, 4, 7, 0x7F, 0x80, 0xFE, 0xFF];
        for carry_in in &[false, true] {
            for a in &values {
                for b in &values {

                    let args = [(3,*b),(2,*a)];
                    let c = run(
                        if *carry_in { Flags::CARRY } else { Flags::empty() },
                        Opcode::Cmp8,
                        &args
                    );

                    let neg_b = ((*b as u16) ^ 0xFF) + 1u16;
                    let neg_b_carry = ((neg_b >> 8) & 0x1) != 0;
                    let sum = (*a as u16) + neg_b;

                    let carry_out = ((sum >> 8) & 0x1) != 0;
                    let zero = (sum & 0xFF) == 0;
                    let neg = ((sum >> 7) & 0x1) != 0;

                    dbg!((a,b,neg_b,neg_b_carry,sum,carry_out,zero,neg, c.flags));
                    
                    // Jump if below <==> CF==1
                    assert_eq!(carry_out, c.flags.contains(Flags::CARRY));
                    assert_eq!(!carry_out, *a < *b);
                    assert_eq!(zero, c.flags.contains(Flags::ZERO));
                    assert_eq!(zero , *a == *b);
                }
            }
        }
    }

    #[test]
    fn add8() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for carry_in in &[false, true] {
            for a in &values {
                for b in &values {
                    let mut sum = (*a as u16) + (*b as u16);

                    if *carry_in {
                        sum += 1;
                    }

                    reg8(
                        Opcode::Add8,
                        *carry_in, *a, *b,
                        (sum & 0xFF) as u8,
                        sum > 0xFF
                    );
                }
            }
        }
    }

    #[test]
    fn add8_nocarryin() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for carry_in in &[false, true] {
            for a in &values {
                for b in &values {
                    let sum = (*a as u16) + (*b as u16);
                    reg8(
                        Opcode::Add8NoCarryIn,
                        *carry_in, *a, *b,
                        (sum & 0xFF) as u8,
                        sum > 0xFF
                    );
                }
            }
        }
    }

    #[test]
    fn add8_nocarry() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for carry_in in &[false, true] {
            for a in &values {
                for b in &values {
                    reg8(
                        Opcode::Add8NoCarry,
                        *carry_in, *a, *b,
                        a.wrapping_add(*b),
                        *carry_in
                    );
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

        let mut c = Computer::with_print(rom, false);

        *c.mem_byte_mut(0x80004) = a;
        *c.mem_byte_mut(0x80005) = b;

        while c.step() {}

        assert_eq!(
            (a as u16) * (b as u16),
            u16::from_le_bytes(c.mem_word_mut(0x80000)[0..2].try_into().unwrap())
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
        c.mem_word_mut(0x80004).copy_from_slice(&u32::to_le_bytes(0x12345678));
        c.mem_word_mut(0x80008).copy_from_slice(&u32::to_le_bytes(0x11111111));

        while c.step() {}

        assert_eq!(
            0x23456789,
            u32::from_le_bytes(*c.mem_word_mut(0x8000C))
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
        add32_tester(carry_in, in1, in2, sum, carry_out);
        add32_tester(carry_in, in2, in1, sum, carry_out);
        add32nocarryin_tester(carry_in, in1, in2);
        add32nocarryin_tester(carry_in, in2, in1);
        addimm32nocarry_tester(carry_in, in1, in2);
        addimm32nocarry_tester(carry_in, in2, in1);
    }

    fn add32_tester(carry_in: bool, in1: u32, in2: u32, sum: u32, carry_out: bool) {
        println!(
            "add32_tester test case {:?} + {:08x} + {:08x} -> {:08x} + {:?}",
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

    fn add32nocarryin_tester(carry_in: bool, in1: u32, in2: u32) {
        let sum = in1.wrapping_add(in2);
        println!(
            "add32nocarryin_tester test case {:08x} + {:08x} -> {:08x}",
            in1, in2, sum
        );

        let mut rom = Vec::new();

        rom.push(Opcode::Add32NoCarryIn as u8);
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

    fn addimm32nocarry_tester(carry_in: bool, in1: u32, in2: u32) {
        let sum = in1.wrapping_add(in2);

        println!(
            "addimm32ignorecarry_tester test case {:08x} + {:08x} -> {:08x}",
            in1, in2, sum
        );

        let mut rom = Vec::new();

        rom.push(Opcode::AddImm32IgnoreCarry as u8);
        rom.push(0);
        rom.extend_from_slice(&u32::to_le_bytes(in2));
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        c.mem_word_mut(0x80000).copy_from_slice(&in1.to_le_bytes());

        if carry_in {
            c.flags |= Flags::CARRY;
        }

        while c.step() {}

        assert_eq!(sum, u32::from_le_bytes(*c.mem_word_mut(0x80000)));
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
