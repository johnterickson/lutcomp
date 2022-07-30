extern crate packed_struct;
use packed_struct::prelude::PackedStruct;

use strum::IntoEnumIterator;

use alu::*;
use common::*;
use std::{borrow::Cow, collections::{BTreeSet, BTreeMap, VecDeque}, convert::TryInto, fmt::Debug, io, ops::Range, sync::{Mutex, mpsc::{self, Receiver}}, thread};
use ucode::*;

fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || {
        loop {
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer).unwrap();
            tx.send(buffer).unwrap();
        }
    });
    rx
}


use lazy_static::lazy_static;
lazy_static! {
    pub static ref NONBLOCKING_STDIN: Mutex<Receiver<String>> = Mutex::new(spawn_stdin_channel());
}

pub struct Computer<'a> {
    pub image: Cow<'a, Image>,
    ram: Vec<u8>,
    pub tty_in: VecDeque<u8>,
    pub tty_out: VecDeque<u8>,
    alu_lut: &'a [u8],
    ucode_rom: &'a [(u8, &'static str, u32)],
    regs: [u8; 4],
    addr: [u8; 4],
    pub pc: [u8; 4],
    upc: u8,
    alu: u8,
    pub flags: Flags,
    pub ir0: u8,
    pub ir0_pc: Option<u32>,
    in1: u8,
    print: bool,
    pub stdin_out: bool,
    trap_addrs: BTreeSet<u32>,
    pub tick_count: u64,
    pub pc_hit_count: Option<BTreeMap<u32, u64>>,
    pub stack_dump_rate: u64,
    prev_log: Option<(u32, Option<u8>)>,
    pub block_for_stdin: bool,
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
        write!(f, " ticks:{}", self.tick_count)?;
        Ok(())
    }
}

impl<'a> Computer<'a> {
    pub fn from_raw(rom: Vec<u8>) -> Computer<'a> {
        Computer::from_raw_with_print(rom, false)
    }

    pub fn from_raw_with_print(mut rom: Vec<u8>, print: bool) -> Computer<'a> {
        rom.resize(ROM_SIZE as usize, 0xFF);
        Computer::from_image(
            Cow::Owned(Image{
                bytes: rom,
                start_addr: 0,
                symbols: BTreeMap::new(),
                functions: BTreeMap::new(),
            }),
            print)
    }

    pub fn from_image(image: Cow<'a, Image>, print: bool) -> Computer<'a> {
        let start_pc = image.start_addr.to_le_bytes();
        let c = Computer {
            image,
            ram: vec![0xCC; RAM_SIZE as usize],
            tty_in: VecDeque::new(),
            tty_out: VecDeque::new(),
            alu_lut: &alu::ALU,
            ucode_rom: &ucode::UCODE,
            regs: [0xFFu8; 4],
            addr: [0xFFu8, 0xFFu8, 0xFFu8, 0x0u8],
            pc: start_pc,
            upc: 0,
            alu: 0xFF,
            flags: Flags::all(),
            ir0: 0xFF,
            ir0_pc: None,
            in1: 0xFF,
            print,
            stdin_out: false,
            trap_addrs: BTreeSet::new(),
            pc_hit_count: None,
            tick_count: 0,
            prev_log: None,
            stack_dump_rate: 0,
            block_for_stdin: false,
        };

        assert_eq!(c.alu_lut.len(), 1 << MEM_BITS_PER_CHIP);
        assert_eq!(c.ucode_rom.len(), 1 << MEM_BITS_PER_CHIP);
        c
    }

    pub fn add_data_trap(&mut self, addr: u32) {
        self.trap_addrs.insert(addr);
    }

    pub fn reg_u8(&self, reg_num: u8) -> u8 {
        self.mem_byte(common::RAM_MIN + reg_num as u32)
    }

    pub fn reg_u8_mut(&mut self, reg_num: u8) -> &mut u8 {
        self.mem_byte_mut(common::RAM_MIN + reg_num as u32)
    }

    pub fn reg_u32(&self, reg_num: u8) -> u32 {
        assert_eq!(reg_num % 4, 0);
        self.mem_word(common::RAM_MIN + reg_num as u32)
    }

    pub fn reg_u32_set(&mut self, reg_num: u8, value: u32) {
        assert!(reg_num % 4 == 0);
        *self.mem_word_mut(common::RAM_MIN + reg_num as u32) = value.to_le_bytes();
    }

    fn check_for_trap(&self, mut range: Range<u32>) {
        for addr in &mut range {
            assert!(
                !self.trap_addrs.contains(&addr),
                "Accessing trapped memory address 0x{:x} as part of access 0x{:x} of len 0x{:x}",
                addr, range.start, range.len());
        }
    }

    pub fn try_mem_slice(&self, addr: u32, len: u32) -> Option<&[u8]> {
        self.check_for_trap(addr..addr+len);
        let chip_address = addr & CHIP_ADDRESS_MASK;
        let chip_select = addr >> MEM_BITS_PER_CHIP;

        match chip_select {
            0 => {
                assert!(ROM_MIN <= addr && addr <= ROM_MAX);
                if chip_address < self.image.start_addr {
                    None
                } else {
                    let image_offset = (chip_address - self.image.start_addr) as usize;
                    if let Some(slice) = self.image.bytes.get(image_offset..image_offset+(len as usize)) {
                        Some(slice)
                    } else {
                        None
                    }
                }
            },
            1 => {
                assert!(RAM_MIN <= addr && addr <= RAM_MAX);
                if let Some(slice) = self.ram.get(chip_address as usize .. (chip_address+len) as usize) {
                    Some(slice)
                } else {
                    None
                }
            },
            _ => None
        }
    }

    pub fn mem_slice(&self, addr: u32, len: u32) -> &[u8] { 
        self.try_mem_slice(addr, len).expect(&format!("Invalid memory read: {:08x}", addr))
    }

    pub fn try_mem_slice_mut(&mut self, addr: u32, len: u32) -> Option<&mut [u8]> {
        self.check_for_trap(addr..addr+len);
        let chip_address = addr & CHIP_ADDRESS_MASK;
        let chip_select = addr >> MEM_BITS_PER_CHIP;

        let chip = match chip_select {
            0 => None,
            1 => {
                assert!(RAM_MIN <= addr && addr <= RAM_MAX);
                Some(&mut self.ram)
            },
            _ => None,
        };
        if let Some(chip) = chip {
            Some(&mut chip[chip_address as usize .. (chip_address+len) as usize])
        } else {
            None
        }
    }

    pub fn mem_slice_mut(&mut self, addr: u32, len: u32) -> &mut [u8] { 
        if let Some(slice) = self.try_mem_slice_mut(addr, len) {
            slice
        } else {
            panic!("Invalid memory write: {:08x}", addr);
        }
    }

    pub fn mem_byte(&self, addr: u32) -> u8 {
        self.mem_slice(addr, 1)[0]
    }

    pub fn mem_word(&self, addr: u32) -> u32 {
        if addr % 4 != 0 {
            panic!("Access of {:08x} is not word-aligned.", addr);
        }
        u32::from_le_bytes(self.mem_slice(addr,4).try_into().unwrap())
    }

    pub fn mem_byte_mut(&mut self, addr_bus: u32) -> &mut u8 {
        &mut self.mem_slice_mut(addr_bus, 1)[0]
    }

    pub fn mem_word_mut(&mut self, addr_bus: u32) -> &mut [u8; 4] {
        let slice: &mut [u8; 4] = self.mem_slice_mut(addr_bus, 4).try_into().unwrap();
        slice
    }

    fn log(&mut self, data_bus: Option<u8>) {
        let log = (u32::from_le_bytes(self.pc), data_bus);
        if Some(log) != self.prev_log {
            // if let Some(data_bus) = log.1 {
            //     // println!("{:06x}  {:02x}", log.0, data_bus);
            // } else {
            //     // println!("{:06x}  xx", log.0);
            // }
            self.prev_log = Some(log);
        }
    }

    pub fn step(&mut self) -> bool {
        if self.print {
            println!("\n{:?}", &self);
        }

        self.tick_count += 8;

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

        // match opcode {
        //     Some(Opcode::Store32Part1) => {
        //         let addr_reg = *self.mem_byte_mut(self.ir0_pc + 2);
        //         let addr = self.reg_u32(addr_reg);
        //         assert!(addr % 4 == 0);
        //     }
        //     _ => {}
        // }

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
                Some(self.mem_byte(addr_bus))
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
                if self.stdin_out {
                    let stdin_channel = NONBLOCKING_STDIN.lock().unwrap();
                    let line = if self.block_for_stdin {
                        stdin_channel.recv().map(|l| Some(l)).unwrap_or_default()
                    } else {
                        stdin_channel.try_recv().map(|l| Some(l)).unwrap_or_default()
                    };

                    if let Some(line) = line {
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

        self.log(data_bus);

        if let Some(data_bus) = data_bus {
            if self.print {
                println!("data_bus: {:02x}", data_bus);
            }
        }

        match urom_op.data_bus_load {
            DataBusLoadEdge::None => {},
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
            DataBusLoadEdge::IR0 => {
                self.ir0_pc = Some(addr_bus);
                self.ir0 = data_bus.unwrap();
            },
            DataBusLoadEdge::Mem => {
                if self.print {
                    println!("addr_bus: {:08x}", addr_bus);
                }
                *self.mem_byte_mut(addr_bus) = data_bus.unwrap();
            }
            DataBusLoadEdge::PcInc => {
                self.pc = (if urom_op.data_bus_out == DataBusOutputLevel::PcSPE {
                    addr_bus
                } else {
                    u32::from_le_bytes(self.pc) + 1
                }).to_le_bytes();

                self.log(data_bus);
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

        if let (Some(pc),Some(pc_hit_count)) = (self.ir0_pc, &mut self.pc_hit_count) {
            *pc_hit_count.entry(pc).or_insert(0) += 1;
        }

        if self.stack_dump_rate != 0 && self.tick_count % self.stack_dump_rate == 0 {
            let orig_sp = self.reg_u32(REG_SP);
            
            print!("# STACK DUMP");

            let print_pc = |pc| {
                if let Some(f) = self.image.find_containing_function(pc) {
                    print!("={}+0x{:x}", f.2, pc - f.0);
                }
            };

            if let Some(pc) = self.ir0_pc {
                print!(" pc:{:05x}", pc);
                print_pc(pc);
            }
                
            
            print!(" sp:{:05x}", orig_sp);

            let mut sp = orig_sp;
            while sp < RAM_MAX {
                if let Some(a) = self.try_mem_slice(sp, 4) {
                    let mut addr = [0u8;4];
                    addr.copy_from_slice(&a);
                    let addr = u32::from_le_bytes(addr);
                    if addr != 0 && addr < ROM_MAX {
                        print!(" [sp+0x{:02x}]:{:05x}", sp - orig_sp, addr);
                        print_pc(addr);
                    }
                }
                sp += 4;
            }
            println!();
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halt() {
        let mut rom = Vec::new();
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw_with_print(rom, false);

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

        let mut c = Computer::from_raw(rom);

        while c.step() {}

        assert_eq!(7+4, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jz_zero() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(0x00);
        rom.push(0xAA);
        rom.push(Opcode::JzImm as u8);
        rom.push(0x10);
        rom.push(0x00);
        rom.push(0x00);
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(0x00);
        rom.push(0xBB);
        for _ in 0..100 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::from_raw(rom);
        c.flags |= Flags::ZERO;

        while c.step() {}

        assert_eq!(0xAA, c.reg_u8(0));
        assert_eq!(0x14, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jz_nonzero() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(0x00);
        rom.push(0xAA);
        rom.push(Opcode::JzImm as u8);
        rom.push(0x10);
        rom.push(0x00);
        rom.push(0x00);
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(0x00);
        rom.push(0xBB);
        let expected_pc = rom.len() as u32 + 4;
        for _ in 0..100 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::from_raw(rom);
        c.flags.remove(Flags::ZERO);

        while c.step() {}

        assert_eq!(0xBB, c.reg_u8(0));
        assert_eq!(expected_pc, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jmpreg() {
        let mut rom = Vec::new();
        rom.push(Opcode::JmpReg as u8);
        rom.push(0x04);
        for _ in 0..20 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::from_raw_with_print(rom, false);

        c.reg_u32_set(4, 7);

        while c.step() {}

        assert_eq!(7+4, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn jmpmem() {
        let mut rom = Vec::new();
        rom.push(Opcode::JmpMem as u8);
        rom.push(0x04);
        for _ in 0..20 {
            rom.push(Opcode::Halt as u8);
        }

        let mut c = Computer::from_raw_with_print(rom, false);

        c.reg_u32_set(8, common::ROM_MIN as u32 + 7);
        c.reg_u32_set(4, common::RAM_MIN as u32 + 8);

        while c.step() {}

        assert_eq!(7+4, u32::from_le_bytes(c.pc));
    }

    #[test]
    fn loadimm8() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm8 as u8);
        rom.push(4);
        rom.push(0xef);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        while c.step() {}

        assert_eq!(0xef, c.reg_u8(4));
    }

    fn imm8(op: Opcode, reg_value: u8, imm: u8, result: u8) {
        let reg_index = 3;
        let mut rom = Vec::new();
        rom.push(op as u8);
        rom.push(reg_index);
        rom.push(imm);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        *c.reg_u8_mut(reg_index) = reg_value;

        while c.step() {}

        assert_eq!(result, c.reg_u8(reg_index));
        assert_eq!(c.flags.contains(Flags::CARRY), false);
        assert_eq!(c.flags.contains(Flags::ZERO), result == 0);
    }

    fn imm32(op: Opcode, reg_value: u32, imm: u32, result: u32) {
        let reg_index = 4;
        let mut rom = Vec::new();
        rom.push(op as u8);
        rom.push(reg_index);
        rom.push(((imm >> 00) & 0xFF) as u8);
        rom.push(((imm >> 08) & 0xFF) as u8);
        rom.push(((imm >> 16) & 0xFF) as u8);
        rom.push(((imm >> 24) & 0xFF) as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        c.reg_u32_set(reg_index, reg_value);

        while c.step() {}

        assert_eq!(result, c.reg_u32(reg_index));
        assert_eq!(c.flags.contains(Flags::CARRY), false);
        assert_eq!(c.flags.contains(Flags::ZERO), result == 0);
    }

    fn run(flags: Flags, op: Opcode, args: &[(u8, u8)]) -> Computer {
        let mut rom = Vec::new();
        rom.push(op as u8);
        for arg in args {
            rom.push(arg.0);
        }
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw_with_print(rom, false);
        for arg in args {
            *c.reg_u8_mut(arg.0) = arg.1;
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

        println!("{:?}, {}", &args, carry_in);
        let c = run(
            if carry_in {Flags::CARRY} else { Flags::empty() },
            op,
            &args
        );

        assert_eq!(result, c.reg_u8(reg_c));
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

    fn shift8_case(mode: ShiftMode, dir: ShiftDirection, amount: u8, value: u8) {
        // println!("{:?} {:?} {:02x} {:02x}", mode, dir, amount, value);
        let mut rom = Vec::new();
        rom.push(Opcode::Shift8 as u8);
        rom.push(ShiftCommand { mode, dir }.pack().unwrap()[0]);
        rom.push(0);
        rom.push(4);
        rom.push(8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw_with_print(rom, false);
        *c.reg_u8_mut(0) = amount;
        *c.reg_u8_mut(4) = value;
        while c.step() {}

        let expected = match mode {
            ShiftMode::Rotate => {
                match dir {
                    ShiftDirection::Left => value.rotate_left(amount.into()),
                    ShiftDirection::Right => value.rotate_right(amount.into()),
                }
            },
            ShiftMode::Logical => {
                match dir {
                    ShiftDirection::Left => value << amount,
                    ShiftDirection::Right => value >> amount,
                }
            },
            ShiftMode::Arithmetic => {
                let value = value as i8;
                (match dir {
                    ShiftDirection::Left => value << amount,
                    ShiftDirection::Right => value >> amount,
                }) as u8
            },
            ShiftMode::Reserved3 => todo!(),
        };

        assert_eq!(expected, c.reg_u8(8), "{:?} {:?} {:02x} {:02x}", mode, dir, amount, value);
    }

    #[test]
    fn shift8() {
        for mode in [ShiftMode::Arithmetic, ShiftMode::Logical, ShiftMode::Rotate] {
            for dir in [ShiftDirection::Left, ShiftDirection::Right] {
                for amount in 0..8 {
                    for value in [0x00,0x01,0x08, 0x09, 0x0F, 0x10, 0x80, 0x90, 0xF0, 0xFF] {
                        shift8_case(mode, dir, amount, value)
                    }
                }
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

        let mut c = Computer::from_raw(rom);

        while c.step() {}

        assert_eq!(0x89abcdef, c.reg_u32(4));
    }

    #[test]
    fn orimm32() {
        let values = [0,1,0xFF,0xAABBCCDD];
        for a in &values {
            for b in &values {
                imm32(Opcode::OrImm32, *a, *b, a | b);
            }
        }
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

        let mut c = Computer::from_raw(rom);

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

        let mut c = Computer::from_raw(rom);

        c.tty_in.push_front('A' as u8);

        while c.step() {}

        assert_eq!(0x80 | ('A' as u8), c.reg_u8(0));
        assert_eq!(0, c.reg_u8(1));
    }

    fn modify8(op: Opcode, input: u8, output: u8) {
        let mut rom = Vec::new();
        rom.push(op as u8);
        rom.push(3);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        *c.reg_u8_mut(3) = input;

        while c.step() {}

        assert_eq!(output, c.reg_u8(3));
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

        let mut c = Computer::from_raw(rom);

        *c.mem_word_mut(0x81234) = u32::to_le_bytes(0xDEADBEEF);

        while c.step() {}

        assert_eq!(0xEF, c.reg_u8(8));
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

        let mut c = Computer::from_raw(rom);

        while c.step() {}

        assert_eq!(0xAA, c.reg_u8(0));
        assert_eq!(0x81234, c.reg_u32(4));
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

        let mut c = Computer::from_raw_with_print(rom, false);

        *c.reg_u8_mut(1) = 0xAA;
        *c.reg_u8_mut(2) = 0xBB;
        // set up stack pointer
        c.reg_u32_set(common::REG_SP, 0x81001);

        while c.step() {}

        assert_eq!(0x80FFF, c.reg_u32(common::REG_SP));
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

        let mut c = Computer::from_raw_with_print(rom, false);

        *c.mem_byte_mut(0x80FFF) = 0xAA;
        *c.mem_byte_mut(0x81000) = 0xBB;

        // set up stack pointer
        c.reg_u32_set(REG_SP, 0x80FFF);

        while c.step() {}

        assert_eq!(0x81001, c.reg_u32(REG_SP));
        assert_eq!(0xAA, c.reg_u8(1));
        assert_eq!(0xBB, c.reg_u8(2));
    }

    fn load32_test(addr_reg: u8, dest_reg: u8) {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(addr_reg);
        rom.push(0x34);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Load32 as u8);
        rom.push(addr_reg);
        rom.push(dest_reg);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        *c.mem_word_mut(0x81234) = u32::to_le_bytes(0xDEADBEEF);

        while c.step() {}

        assert_eq!(0xDEADBEEF, c.reg_u32(dest_reg));
    }

    #[test]
    fn load32() {
        load32_test(4, 8);
        load32_test(4, 4);
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
        rom.push(Opcode::Store32_1 as u8);
        rom.push(0);
        rom.push(4);
        rom.push(Opcode::Store32_2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);
        
        assert_ne!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x81234)));
        
        while c.step() {}

        assert_eq!(0x89ABCDEF, c.reg_u32(0));
        assert_eq!(0x81234, c.reg_u32(4));

        assert_eq!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x81234)));
    }

    #[test]
    #[should_panic]
    fn store32_unaligned() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(0x0);
        rom.push(0xEF);
        rom.push(0xCD);
        rom.push(0xAB);
        rom.push(0x89);
        rom.push(Opcode::LoadImm32 as u8);
        rom.push(4);
        rom.push(0x35);
        rom.push(0x12);
        rom.push(0x08);
        rom.push(0x00);
        rom.push(Opcode::Store32_1 as u8);
        rom.push(0);
        rom.push(4);
        rom.push(Opcode::Store32_2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);
        
        assert_ne!(0x89ABCDEF, u32::from_le_bytes(*c.mem_word_mut(0x81234)));
        
        while c.step() {}

        assert_eq!(0x89ABCDEF, c.reg_u32(0));
        assert_eq!(0x81235, c.reg_u32(4));

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

        let mut c = Computer::from_raw(rom);

        c.reg_u32_set(0xC, 0x81234);

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

        let mut c = Computer::from_raw(rom);
        c.reg_u32_set(4, 0x0F0F0F0F);
        c.reg_u32_set(8, 0x40302010);

        while c.step() {}

        assert_eq!(0x4F3F2F1F, c.reg_u32(0xC));
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
                        Opcode::AddCarry8,
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
        rom.push(Opcode::Mul8 as u8);
        rom.push(0x4);
        rom.push(0x5);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw_with_print(rom, false);

        *c.reg_u8_mut(4) = a;
        *c.reg_u8_mut(5) = b;

        while c.step() {}

        assert_eq!(
            (a as u16) * (b as u16),
            (c.reg_u32(0) & 0xFFFF) as u16
        );
    }

    #[test]
    fn div8() {
        let values = [0u8, 1, 2, 3, 4, 27, 0x80, 0xFE, 0xFF];
        for a in &values {
            for b in &values {
                if *b == 0 { continue; }
                reg8(
                    Opcode::Divide8,
                    false, *a, *b,
                    a.wrapping_div(*b),
                    false,
                );
            }
        }
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
    fn addcarry32() {
        let mut rom = Vec::new();
        rom.push(Opcode::AddCarry32_1 as u8);
        rom.push(0x4);
        rom.push(0x8);
        rom.push(0xc);
        rom.push(Opcode::AddCarry32_2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);
        c.flags.remove(Flags::CARRY);
        c.reg_u32_set(4, 0x12345678);
        c.reg_u32_set(8, 0x11111111);

        while c.step() {}

        assert_eq!(0x23456789, c.reg_u32(0xC));
    }

    #[test]
    fn copy32() {
        let mut rom = Vec::new();
        rom.push(Opcode::Copy32 as u8);
        rom.push(0x4);
        rom.push(0x8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);
        c.reg_u32_set(4, 0x12345678);

        while c.step() {}

        assert_eq!(0x12345678, c.reg_u32(8));
    }

    fn run_add32_tests(carry_in: bool, in1: u32, in2: u32) {
        addcarry32_tester(carry_in, in1, in2, 0, 4, 8);
        addcarry32_tester(carry_in, in2, in1, 0, 4, 8);
        addcarry32_tester(carry_in, in1, in2, 0, 4, 0); //re-use reg1
        addcarry32_tester(carry_in, in1, in2, 0, 4, 4); //re-use reg2
        addimm32nocarry_tester(carry_in, in1, in2);
        addimm32nocarry_tester(carry_in, in2, in1);
    }

    fn addcarry32_tester(carry_in: bool, in1: u32, in2: u32, reg1: u8, reg2: u8, reg_sum: u8) {
        let sum = in1 as u64 + in2 as u64 + carry_in as u64;
        let carry_out = sum > u32::max_value() as u64;
        let sum = (sum & 0xFFFFFFFF) as u32;

        println!(
            "add32nocarryin_tester test case cin:{} + r{:02x}={:08x} + r{:02x}={:08x} -> r{:02x}={:08x}",
            carry_in, reg1, in1, reg2, in2, reg_sum, sum
        );

        let mut rom = Vec::new();

        rom.push(Opcode::AddCarry32_1 as u8);
        rom.push(reg1);
        rom.push(reg2);
        rom.push(reg_sum);
        rom.push(Opcode::AddCarry32_2 as u8);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);

        c.reg_u32_set(reg1, in1);
        c.reg_u32_set(reg2, in2);

        if carry_in {
            c.flags |= Flags::CARRY;
        } else {
            c.flags.remove(Flags::CARRY);
        }

        while c.step() {}

        if reg1 != reg_sum {
            assert_eq!(in1, c.reg_u32(reg1));
        }
        if reg2 != reg_sum {
            assert_eq!(in2, c.reg_u32(reg2));
        }
        assert_eq!(sum, c.reg_u32(reg_sum), "Expected:0x{:x} Actual:0x{:x}", sum, c.reg_u32(reg_sum));
        assert_eq!(carry_out, c.flags.contains(Flags::CARRY));
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

        let mut c = Computer::from_raw(rom);

        c.reg_u32_set(0, in1);

        if carry_in {
            c.flags |= Flags::CARRY;
        }  else {
            c.flags.remove(Flags::CARRY);
        }

        while c.step() {}

        assert_eq!(sum, c.reg_u32(0), "Expected:0x{:x} Actual:0x{:x}", sum, c.reg_u32(0));
    }

    #[test]
    fn add_no_carry() {
        run_add32_tests(false, 1, 2);
        run_add32_tests(false, 0x01010101, 0x02020202);
        run_add32_tests(false, 0x01010101, 0xFEFEFEFE);
    }

    #[test]
    fn add_internal_carry() {
        run_add32_tests(false, 0xFF, 1);
        run_add32_tests(false, 0xFFFF, 1);
        run_add32_tests(false, 0xFFFFFF, 1);
        run_add32_tests(false, 0xFFFFFFFF, 1);
    }

    #[test]
    fn add_incoming_carry() {
        run_add32_tests(true, 1, 2);
        run_add32_tests(true, 0xFF, 0);
    }


    fn run_u32_pairs<F: Fn(u32, u32) -> ()>(f : F) {
        use itertools::Itertools;
        let values = &[0x0, 0x1, 0xFF];
        let byte_values: Vec<u32> = values
            .iter()
            .cartesian_product(values)
            .cartesian_product(values)
            .cartesian_product(values)
            .map(|(((a1, a2), a3), a4)| u32::from_le_bytes([*a1, *a2, *a3, *a4]))
            .collect();

        for a in &byte_values {
            for b in &byte_values {
                f(*a,*b);
            }
        }
    }

    fn run_add_cases<F: Fn(bool, u32, u32) -> ()>(f : F) {
        for carry_in in &[false, true] {
            let carry_in = *carry_in;
            run_u32_pairs(|in1, in2| {
                if in1 >= in2 {
                    f(carry_in, in1, in2);
                }
            });
        }
    }

    #[test]
    fn addcarry32_coverage() {
        addcarry32_tester(true, 0x01000000, 0xff, 0, 4, 8);
        run_add_cases(|carry_in, in1, in2| {
            addcarry32_tester(carry_in, in1, in2, 0, 4, 8);
            addcarry32_tester(carry_in, in2, in1, 0, 4, 8);
            addcarry32_tester(carry_in, in1, in2, 0, 4, 0); //re-use reg1
            addcarry32_tester(carry_in, in1, in2, 0, 4, 4); //re-use reg2
            // addcarry32_tester(carry_in, in1, in2, 4, 4, 4); //re-use both
        });
    }

    #[test]
    fn addimm32nocarryin_coverage() {
        run_add_cases(|carry_in, in1, in2| {
            addimm32nocarry_tester(carry_in, in1, in2);
            addimm32nocarry_tester(carry_in, in2, in1);
        });
    }

    #[test]
    fn alu_info() {
        let get_info = |info: SpecialMicroHelperInfo| {
            let mut rom = Vec::new();
            rom.push(Opcode::GetAluInfo as u8);
            rom.push(0x1);
            rom.push(0x2);
            rom.push(Opcode::Halt as u8);

            let mut c = Computer::from_raw(rom);
            *c.reg_u8_mut(1) = info as u8;

            while c.step() {}
            c.reg_u8(2)
        };

        assert_eq!(alu::MAJOR_VERSION, get_info(SpecialMicroHelperInfo::VersionMajor));
        assert_eq!(alu::MINOR_VERSION, get_info(SpecialMicroHelperInfo::VersionMinor));
        assert_eq!(alu::PATCH_VERSION, get_info(SpecialMicroHelperInfo::VersionPatch));

        let hash = alu::ALU_HASH.to_le_bytes();
        assert_eq!(hash[0], get_info(SpecialMicroHelperInfo::Hash0));
        assert_eq!(hash[1], get_info(SpecialMicroHelperInfo::Hash1));
        assert_eq!(hash[2], get_info(SpecialMicroHelperInfo::Hash2));
        assert_eq!(hash[3], get_info(SpecialMicroHelperInfo::Hash3));
    }

    #[test]
    fn ucode_info() {
        let mut rom = Vec::new();
        rom.push(Opcode::GetUcodeInfo as u8);
        rom.push(0x1);
        rom.push(0x2);
        rom.push(0x3);
        rom.push(0x4);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::from_raw(rom);
        while c.step() {}

        assert_eq!(ucode::MAJOR_VERSION, c.reg_u8(1));
        assert_eq!(ucode::MINOR_VERSION, c.reg_u8(2));
        assert_eq!(ucode::PATCH_VERSION, c.reg_u8(3));

        assert_eq!(*ucode::UCODE_HASH, c.reg_u32(4));
    }
}
