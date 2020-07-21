extern crate packed_struct;
use packed_struct::prelude::*;

use strum::IntoEnumIterator;

use alu::*;
use common::*;
use ucode::*;
use std::fmt::Debug;

pub struct Computer {
    rom: Vec<u8>,
    ram: Vec<u8>,
    alu_lut: Vec<u8>,
    ucode_rom: Vec<u8>,
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

impl Debug for Computer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upc:{:02x}", self.upc)?;
        write!(f, " pc:{:08x}", u32::from_le_bytes(self.pc))?;
        write!(f, " pcr:{:08x}", u32::from_le_bytes(self.pcr))?;
        write!(f, " regs:{:08x}", u32::from_le_bytes(self.regs))?;
        write!(f, " addr:{:08x}", u32::from_le_bytes(self.addr))?;
        write!(f, " ir0:{:02x}", self.ir0)?;
        Ok(())
    }
    
}

impl Computer {
    pub fn new(rom: Vec<u8>) -> Computer {
        let c = Computer {
            rom,
            ram: vec![0u8; 1 << 19],
            alu_lut: alu::ALU.clone(),
            ucode_rom: ucode::UCODE.clone(),
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

        assert_eq!(c.alu_lut.len(), 1 << 19);
        assert_eq!(c.ucode_rom.len(), 1 << 19);
        c
    }

    pub fn step(&mut self) -> bool {
        println!("\n{:?}", &self);

        let urom_entry = MicroEntry {
            flags: self.flags.bits().into(),
            instruction: self.ir0
        };
        let mut urom_addr = u16::from_le_bytes(urom_entry.pack_lsb()) as usize;
        urom_addr <<= 7;
        urom_addr += self.upc as usize;

        let opcode = Opcode::iter().nth(urom_entry.instruction as usize);
        println!("urom_addr {:04x} = {:?} {:?} + {:02x}", 
            urom_addr, urom_entry, opcode, self.upc);

        let urom_op = MicroOp::unpack(&[
            self.ucode_rom[urom_addr], 
            self.ucode_rom[urom_addr+1]]).unwrap();
        println!("urom_op: {:?}", urom_op);
        
        let addr_bus = u32::from_le_bytes(match urom_op.address_bus_out {
            AddressBusOutputLevel::Addr => self.regs,
            AddressBusOutputLevel::Pc => self.pc
        }) as usize;

        println!("addr_bus: {:04x}", addr_bus);

        let data_bus = match urom_op.data_bus_out {
            DataBusOutputLevel::Alu => Some(self.alu),
            DataBusOutputLevel::Halt => return false,
            DataBusOutputLevel::Imm => Some(*urom_op.immediate as u8),
            DataBusOutputLevel::Mem => if addr_bus & 0x80000000 != 0 {
                Some(self.ram[addr_bus])
            } else {
                Some(self.rom[addr_bus & 0x7FFFFFFF])
            },
            DataBusOutputLevel::Next => {
                self.upc = 0;
                return true;
            },
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
                let lut_entry_bytes = lut_entry.pack_lsb();
                let lut_entry_bytes = [
                    lut_entry_bytes[0],
                    lut_entry_bytes[1],
                    lut_entry_bytes[2],
                    0];
                let lut_entry_index = u32::from_le_bytes(lut_entry_bytes) as usize;
                print!("lut_entry:{:04x}={:?}={:?} => ",
                    lut_entry_index,
                    lut_entry_bytes,
                    lut_entry);
                let lut_output = self.alu_lut[lut_entry_index];
                println!("{:02x}", lut_output);
                self.alu = lut_output;
            },
            DataBusLoadEdge::Flags => {
                self.flags = Flags::from_bits_truncate(data_bus.unwrap());
            }
            DataBusLoadEdge::In1 => self.in1 = data_bus.unwrap(),
            DataBusLoadEdge::IR0 => self.ir0 = data_bus.unwrap(),
            DataBusLoadEdge::Mem => {
                self.ram[addr_bus] = data_bus.unwrap();
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
            
        while c.step() { }

        assert_eq!(c.regs[0], 0xFF);
        assert_eq!(c.regs[1], 0xFF);
        assert_eq!(c.regs[2], 0xFF);
        assert_eq!(c.regs[3], 0xFF);
    }

    #[test]
    fn add_nocarry() {
        let mut rom = Vec::new();
        rom.push(Opcode::LoadImm as u8);
        rom.push(1);
        rom.push(0);
        rom.push(0);
        rom.push(0);
        rom.push(Opcode::Add as u8);
        rom.push(2);
        rom.push(0);
        rom.push(0);
        rom.push(0);
        rom.push(Opcode::Halt as u8);

        let mut c = Computer::new(rom);
            
        while c.step() { }

        assert_eq!(c.regs[0], 3);
        assert_eq!(c.regs[1], 0);
        assert_eq!(c.regs[2], 0);
        assert_eq!(c.regs[3], 0);
    }
}