extern crate packed_struct;
use packed_struct::prelude::*;

use alu::*;
use common::*;
use ucode::*;

pub struct Computer {
    rom: Vec<u8>,
    ram: Vec<u8>,
    alu_lut: Vec<u8>,
    urom: Vec<u8>,
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

impl Computer {
    pub fn step(&mut self) -> bool {
        let urom_entry = MicroEntry {
            flags: self.flags.bits().into(),
            instruction: self.ir0
        };
        let mut urom_addr = u16::from_le_bytes(urom_entry.pack()) as usize;
        urom_addr <<= 7;
        urom_addr += self.upc as usize;

        let urom_op = MicroOp::unpack(&[self.urom[urom_addr], self.urom[urom_addr+1]]).unwrap();
        
        let addr_bus = u32::from_le_bytes(match urom_op.address_bus_out {
            AddressBusOutputLevel::Addr => self.regs,
            AddressBusOutputLevel::Pc => self.pc
        }) as usize;

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
                self.upc += 1;
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
                self.alu = self.alu_lut[u32::from_le_bytes(lut_entry.pack()) as usize];
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

        true
    }
}