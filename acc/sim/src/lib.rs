extern crate packed_struct;
use packed_struct::prelude::*;

extern crate itertools;

use strum::IntoEnumIterator;

use alu::*;
use common::*;
use std::fmt::Debug;
use ucode::*;

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
    alu_lut: &'a [u8],
    ucode_rom: &'a [u8],
    regs: [u8; 4],
    addr: [u8; 4],
    pc: [u8; 4],
    pcr: [u8; 4],
    upc: u8,
    alu: u8,
    flags: Flags,
    ir0: u8,
    in1: u8,
}

impl<'a> Debug for Computer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upc:{:02x}", self.upc)?;
        write!(f, " pc:{:08x}", u32::from_le_bytes(self.pc))?;
        write!(f, " pcr:{:08x}", u32::from_le_bytes(self.pcr))?;
        write!(f, " regs:{:08x}", u32::from_le_bytes(self.regs))?;
        write!(f, " addr:{:08x}", u32::from_le_bytes(self.addr))?;
        write!(f, " ir0:{:02x}", self.ir0)?;
        write!(f, " {:?}", self.flags)?;
        Ok(())
    }
}

impl<'a> Computer<'a> {
    pub fn new(rom: Vec<u8>) -> Computer<'a> {
        let c = Computer {
            rom,
            ram: vec![0u8; 1 << MEM_BITS],
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
        };

        assert_eq!(c.alu_lut.len(), 1 << MEM_BITS);
        assert_eq!(c.ucode_rom.len(), 1 << MEM_BITS);
        c
    }

    fn mem_mut(&mut self, addr_bus: u32) -> &mut u8 {
        let addr_bus = addr_bus & 0x0FFFFF;
        match addr_bus as usize {
            a if ROM_MIN <= a && a <= ROM_MAX => &mut self.rom[a - ROM_MIN],
            a if RAM_MIN <= a && a <= RAM_MAX => &mut self.ram[a - RAM_MIN],
            _ => panic!(format!("Invalid address: {:08x}", addr_bus)),
        }
    }

    pub fn step(&mut self) -> bool {
        println!("\n{:?}", &self);

        let urom_entry = MicroEntry {
            flags: self.flags.bits().into(),
            instruction: self.ir0,
        };
        let mut urom_addr = u16::from_le_bytes(urom_entry.pack_lsb()) as usize;
        urom_addr <<= 7;
        urom_addr += self.upc as usize;

        let opcode = Opcode::iter().nth(urom_entry.instruction as usize);
        println!(
            "urom_addr {:05x} = {:?} {:?} + {:02x}",
            urom_addr, urom_entry, opcode, self.upc
        );

        let urom_op =
            MicroOp::unpack(&[self.ucode_rom[urom_addr], self.ucode_rom[urom_addr + 1]]).unwrap();
        println!("urom_op: {:?}", urom_op);

        let addr_bus = u32::from_le_bytes(match urom_op.address_bus_out {
            AddressBusOutputLevel::Addr => self.addr,
            AddressBusOutputLevel::Pc => self.pc,
        });

        println!("addr_bus: {:08x}", addr_bus);

        let data_bus = match urom_op.data_bus_out {
            DataBusOutputLevel::Alu => Some(self.alu),
            DataBusOutputLevel::Halt => return false,
            DataBusOutputLevel::Imm => Some(((*urom_op.immediate << 4) >> 4) as u8),
            DataBusOutputLevel::Mem => {
                Some(*self.mem_mut(addr_bus))
            }
            DataBusOutputLevel::Next => {
                self.upc = 0;
                return true;
            }
            DataBusOutputLevel::Pc => {
                self.pc = self.pcr;
                None
            }
            DataBusOutputLevel::TtyIn => unimplemented!(),
            DataBusOutputLevel::W => Some(self.regs[0]),
            DataBusOutputLevel::X => Some(self.regs[1]),
            DataBusOutputLevel::Y => Some(self.regs[2]),
            DataBusOutputLevel::Z => Some(self.regs[3]),
            DataBusOutputLevel::Reserved7 => return false,
        };

        if let Some(data_bus) = data_bus {
            println!("data_bus: {:02x}", data_bus);
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
                print!(
                    "lut_entry:{:05x}={:?} => ",
                    lut_entry_index, lut_entry
                );
                let lut_output = self.alu_lut[lut_entry_index];
                println!("{:02x}", lut_output);
                self.alu = lut_output;
            }
            DataBusLoadEdge::Flags => {
                self.flags = Flags::from_bits_truncate(data_bus.unwrap());
            }
            DataBusLoadEdge::In1 => self.in1 = data_bus.unwrap(),
            DataBusLoadEdge::IR0 => self.ir0 = data_bus.unwrap(),
            DataBusLoadEdge::Mem => {
                *self.mem_mut(addr_bus) = data_bus.unwrap();
            }
            DataBusLoadEdge::PcInc => {
                self.pc = (u32::from_le_bytes(self.pc) + 1).to_le_bytes();
            }
            DataBusLoadEdge::PcR => {
                self.pcr = self.addr;
            }
            DataBusLoadEdge::TtyIn => unimplemented!(),
            DataBusLoadEdge::TtyOut => print!("{}", data_bus.unwrap() as char),
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

        let mut c = Computer::new(rom);

        while c.step() {}
    }

    #[test]
    fn loadimm() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm as u8);
        rom.push(1);
        rom.push(2);
        rom.push(3);
        rom.push(4);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(c.regs[0], 1);
        assert_eq!(c.regs[1], 2);
        assert_eq!(c.regs[2], 3);
        assert_eq!(c.regs[3], 4);
    }

    #[test]
    fn or() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm as u8);
        rom.push(0xF0);
        rom.push(0x0F);
        rom.push(0xF0);
        rom.push(0x0F);
        rom.push(Opcode::Or as u8);
        rom.push(0x0F);
        rom.push(0xF0);
        rom.push(0x0F);
        rom.push(0xF0);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(c.regs[0], 0xFF);
        assert_eq!(c.regs[1], 0xFF);
        assert_eq!(c.regs[2], 0xFF);
        assert_eq!(c.regs[3], 0xFF);
    }

    #[test]
    fn regsor() {
        let mut rom = Vec::new();
        rom.push(Opcode::RegsOr as u8);
        rom.push(0x8);
        rom.push(0x0);
        rom.push(0x4);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
        c.ram.as_mut_slice()[0..4].copy_from_slice(&u32::to_le_bytes(0x0F0F0F0F));
        c.ram.as_mut_slice()[4..8].copy_from_slice(&u32::to_le_bytes(0x40302010));

        while c.step() {}

        assert_eq!(0x4F3F2F1F, u32::from_le_bytes(c.ram[8..0xc].try_into().unwrap()));
    }

    #[test]
    fn fetch_to_reg_ram() {
        let mut rom = Vec::new();
        rom.push(Opcode::FetchToReg as u8);
        rom.push(0x80);
        rom.push(0x70);
        rom.push(0x09);
        rom.push(0x04);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
        c.ram.as_mut_slice()[0x017080..0x017084].copy_from_slice(&u32::to_le_bytes(0xDEADBEEF));

        while c.step() {}

        assert_eq!(0xDEADBEEF, u32::from_le_bytes(c.ram[0x04..0x08].try_into().unwrap()));
    }

    #[test]
    fn fetch_to_reg_rom() {
        let mut rom = Vec::new();
        rom.push(Opcode::FetchToReg as u8);
        rom.push(0xCC);
        rom.push(0xCC);
        rom.push(0xCC);
        rom.push(0x04);
        rom.push(Opcode::Halt as u8);
        while rom.len() % 4 != 0 { rom.push(0); }
        let addr = rom.len();
        rom.push(0xEF);
        rom.push(0xBE);
        rom.push(0xAD);
        rom.push(0xDE);
        
        rom[1] = addr.to_le_bytes()[0];
        rom[2] = addr.to_le_bytes()[1];
        rom[3] = addr.to_le_bytes()[2];

        let mut c = Computer::new(rom);

        while c.step() {}

        assert_eq!(0xDEADBEEF, u32::from_le_bytes(c.ram[0x04..0x08].try_into().unwrap()));
    }

    fn add_tester(carry_in: bool, in1: u32, in2: u32, sum: u32, carry_out: bool) {
        add_tester_internal(carry_in, in1, in2, sum, carry_out);
        add_tester_internal(carry_in, in2, in1, sum, carry_out);
    }

    fn add_tester_internal(carry_in: bool, in1: u32, in2: u32, sum: u32, carry_out: bool) {
        println!(
            "test case {:?} + {:08x} + {:08x} -> {:08x} + {:?}",
            carry_in, in1, in2, sum, carry_out
        );

        let mut rom = Vec::new();

        rom.push(Opcode::LoadImm as u8);
        for b in &in1.to_le_bytes() {
            rom.push(*b);
        }

        rom.push(Opcode::Add as u8);
        for b in &in2.to_le_bytes() {
            rom.push(*b);
        }
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);

        if carry_in {
            c.flags |= Flags::CARRY;
        }

        while c.step() {}

        assert_eq!(sum, u32::from_le_bytes(c.regs));
        assert_eq!(carry_out, c.flags.contains(Flags::CARRY));
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
    fn coverage() {
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
                    let sum = a as u64 + b as u64 + *carry_in as u64;
                    let carry_out = sum > u32::max_value() as u64;
                    let sum = (sum & 0xFFFFFFFF) as u32;
                    add_tester_internal(*carry_in, a, b, sum, carry_out);
                }
            }
        }
    }
}
