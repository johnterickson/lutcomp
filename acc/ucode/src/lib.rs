extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::{EnumCount, IntoEnumIterator};

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;
use std::cell::RefCell;

use lazy_static::lazy_static;
lazy_static! {
    pub static ref UCODE: Vec<u8> = ucode(false);
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum DataBusLoadEdge {
    PcInc = 0,
    IR0 = 1,
    In1 = 2,
    Alu = 3,
    Mem = 4,
    TtyIn = 5,
    PcR = 6,
    Flags = 7,
    W = 8,
    X = 9,
    Y = 10,
    Z = 11,
    Addr0 = 12,
    Addr1 = 13,
    Addr2 = 14,
    TtyOut = 15,
}

impl DataBusLoadEdge {
    pub fn wxyz(i: usize) -> DataBusLoadEdge {
        match i {
            0 => DataBusLoadEdge::W,
            1 => DataBusLoadEdge::X,
            2 => DataBusLoadEdge::Y,
            3 => DataBusLoadEdge::Z,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum AddressBusOutputLevel {
    Addr = 0,
    Pc = 1,
}

#[derive(Clone, Copy, Display, Debug, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum DataBusOutputLevel {
    Next = 0,
    Halt = 1,
    Imm = 2,
    Alu = 3,
    Mem = 4,
    TtyIn = 5,
    Pc = 6,
    Reserved7 = 7,
    W = 8,
    X = 9,
    Y = 10,
    Z = 11,
}

impl DataBusOutputLevel {
    pub fn wxyz(i: usize) -> DataBusOutputLevel {
        match i {
            0 => DataBusOutputLevel::W,
            1 => DataBusOutputLevel::X,
            2 => DataBusOutputLevel::Y,
            3 => DataBusOutputLevel::Z,
            _ => panic!(),
        }
    }   
}

const MAX_UOP_BYTES: usize = 128;
const MAX_UOPS: usize = MAX_UOP_BYTES / 2;

#[derive(Clone, Copy, Debug)]
#[derive(PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct MicroOp {
    #[packed_field(bits = "0..=3", ty = "enum")]
    pub data_bus_out: DataBusOutputLevel,
    #[packed_field(bits = "4..=6", ty = "enum")]
    pub alu_opcode: AluOpcode,
    #[packed_field(bits = "7", ty = "enum")]
    pub address_bus_out: AddressBusOutputLevel,
    #[packed_field(bits = "8..=11", ty = "enum")]
    pub data_bus_load: DataBusLoadEdge,
    #[packed_field(bits = "12..=15")]
    pub immediate: Integer<u8, packed_bits::Bits4>,
}

impl MicroOp {
    fn new(
        address_bus_out: Option<AddressBusOutputLevel>,
        data_out: DataBusOutputLevel,
        alu_opcode: Option<AluOpcode>,
        data_bus_load: DataBusLoadEdge,
        immediate: Option<u8>,
    ) -> MicroOp {
        assert_eq!(data_bus_load == DataBusLoadEdge::Mem || data_out == DataBusOutputLevel::Mem, address_bus_out.is_some());
        assert_eq!(data_bus_load == DataBusLoadEdge::Alu, alu_opcode.is_some());
        assert_eq!(data_out == DataBusOutputLevel::Imm, immediate.is_some());

        MicroOp {
            data_bus_out: data_out,
            alu_opcode: alu_opcode.unwrap_or(AluOpcode::AddLoNoCarry),
            address_bus_out: address_bus_out.unwrap_or(AddressBusOutputLevel::Addr),
            data_bus_load,
            immediate: immediate.unwrap_or_default().into(),
        }
    }

    fn emit(&self) -> (u8, u8) {
        let bytes = self.pack();
        (bytes[0], bytes[1])
    }

    fn print(&self) {
        println!("{:02x} {:02x}", self.emit().1, self.emit().0);
    }
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct MicroEntry {
    #[packed_field(bits = "0..=7")]
    pub instruction: u8,
    #[packed_field(bits = "8..=11")]
    pub flags: Integer<u8, packed_bits::Bits4>,
}

impl MicroEntry {
    pub fn pack_lsb(&self) -> [u8; 2] {
        let bytes = self.pack();
        [bytes[1], bytes[0]]
    }

    pub fn unpack_lsb(bytes: &[u8; 2]) -> MicroEntry {
        let bytes = [bytes[1], bytes[0]];
        MicroEntry::unpack(&bytes).unwrap()
    }
}

pub fn ucode(print: bool) -> Vec<u8> {
    if print {
        println!("v2.0 raw");
    }

    let halt = MicroOp::new(
        None,
        DataBusOutputLevel::Halt,
        None,
        DataBusLoadEdge::IR0, //same value as Halt
        None,
    );

    let pc_inc = MicroOp::new(
        None,
        DataBusOutputLevel::W,
        None,
        DataBusLoadEdge::PcInc,
        None,
    );

    let mut vec_out = Vec::new();
    let out = RefCell::new(&mut vec_out);

    // let mut uops = Vec::new();
    for encoded_inst in 0u16..(1 << 12) {
        let bytes: &[u8; 2] = &encoded_inst.to_le_bytes();
        let inst = MicroEntry::unpack_lsb(bytes);
        let flags = Flags::from_bits_truncate(*inst.flags);
        let opcode = Opcode::iter().nth(inst.instruction as usize);
        let base_address = encoded_inst as usize * MAX_UOPS * 2;

        if print {
            println!("#");
            println!("#");
            println!(
                "# addr:{:05x} inst:{:02x}={:?} flags:[{:?}] opcode:{:?}",
                base_address, encoded_inst, &inst, &flags, &opcode
            );
            println!("#");
        }

        let wxyz_outs = &[
            DataBusOutputLevel::W,
            DataBusOutputLevel::X,
            DataBusOutputLevel::Y,
            DataBusOutputLevel::Z,
        ];

        let wxyz_loads = &[
            DataBusLoadEdge::W,
            DataBusLoadEdge::X,
            DataBusLoadEdge::Y,
            DataBusLoadEdge::Z,
        ];

        let addr_loads = &[
            DataBusLoadEdge::Addr0,
            DataBusLoadEdge::Addr1,
            DataBusLoadEdge::Addr2,
        ];

        assert_eq!(
            Opcode::iter().last().unwrap() as u8,
            Opcode::count() as u8 - 1,
        );

        let inc_pc = RefCell::new(true);
        let uop_count = RefCell::new(0);

        let add_op = |u: MicroOp| {
            if print {
                println!(
                    "#  addr:{:05x} uop:{:?}",
                    base_address + *uop_count.borrow() * 2,
                    &u
                );
                u.print();
            }
            let mut out = out.borrow_mut();
            out.push(u.emit().0);
            out.push(u.emit().1);
            *uop_count.borrow_mut() += 1;
        };

        let jmp_abs = || {
            for addr_edge in addr_loads {
                add_op(pc_inc);
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    *addr_edge,
                    None,
                ));
            }

            *inc_pc.borrow_mut() = false;
        };

        let start_of_ram = || {
            add_op(MicroOp::new(
                None,
                DataBusOutputLevel::Imm,
                None,
                DataBusLoadEdge::Addr2,
                Some(0x8),
            ));
            add_op(MicroOp::new(
                None,
                DataBusOutputLevel::Imm,
                None,
                DataBusLoadEdge::Addr1,
                Some(0),
            ));
        };

        let read_reg = || {
            for i in 0..=3 {
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Addr),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::wxyz(i as usize),
                    None,
                ));

                if i != 3 {
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        Some(AluOpcode::AddLoNoCarry),
                        DataBusLoadEdge::Alu,
                        Some(1),
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                }
            }
        };

        let parallel = |op: AluOpcode| {
            for i in 0..=3 {
                add_op(pc_inc);
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    wxyz_outs[i],
                    Some(op),
                    DataBusLoadEdge::Alu,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Alu,
                    None,
                    wxyz_loads[i],
                    None,
                ));
            }
        };

        if print {
            println!("# common prelude");
        }
        add_op(MicroOp::new(
            Some(AddressBusOutputLevel::Pc),
            DataBusOutputLevel::Mem,
            None,
            DataBusLoadEdge::IR0,
            None,
        ));
        add_op(MicroOp::new(
            Some(AddressBusOutputLevel::Pc),
            DataBusOutputLevel::Mem,
            None,
            DataBusLoadEdge::IR0,
            None,
        ));

        match opcode {
            Some(Opcode::Jmp) => {
                jmp_abs();
            }
            Some(Opcode::Jz) => {
                if flags.contains(Flags::ZERO) {
                    jmp_abs();
                }
            }
            Some(Opcode::LoadImm) => {
                for edge in wxyz_loads {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        *edge,
                        None,
                    ));
                }
            }
            Some(Opcode::Load) => {
                for (i, addr_edge) in addr_loads.iter().enumerate() {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        *addr_edge,
                        None,
                    ));
                    if i == 0 {
                        add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                    }
                }
                for (i, edge) in wxyz_loads.iter().enumerate() {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        *edge,
                        None,
                    ));
                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }
            }
            Some(Opcode::Or) => {
                parallel(AluOpcode::Or);
            }
            Some(Opcode::Xor) => {
                parallel(AluOpcode::Xor);
            }
            Some(Opcode::And) => {
                parallel(AluOpcode::And);
            }
            Some(Opcode::Add) => {
                for i in 0..=3 {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        None,
                        wxyz_outs[i],
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addlo(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        wxyz_loads[i],
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addhi(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Flags,
                        None,
                    ));
                }
            }
            Some(Opcode::Halt) => {
                add_op(halt);
            }
            Some(Opcode::RegsOr) => {

                for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z] {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        *edge,
                        None,
                    ));
                }

                start_of_ram();

                
                for i in 0..=3 {
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::X,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Y,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::Or),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Z,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Mem,
                        None,
                    ));
    
                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            None,
                            DataBusLoadEdge::In1,
                            Some(1),
                        ));
                        for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                            add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::wxyz(*r as usize),
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                None,
                            ));
                            add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::wxyz(*r as usize),
                                None,
                            ));
                        }
                    }
                }
            }
            Some(Opcode::RegsAddPart1) => {

                for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z] {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        *edge,
                        None,
                    ));
                }

                start_of_ram();

                
                for i in 0..=2 {
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::X,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Y,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addlo(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::W,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addhi(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Flags,
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Z,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::W,
                        None,
                        DataBusLoadEdge::Mem,
                        None,
                    ));
    
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        None,
                        DataBusLoadEdge::In1,
                        Some(1),
                    ));
                    for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::wxyz(*r as usize),
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::wxyz(*r as usize),
                            None,
                        ));
                    }
                }
            }
            Some(Opcode::RegsAddPart2) => {
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::X,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Addr),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Y,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Addr),
                    DataBusOutputLevel::Mem,
                    Some(AluOpcode::addlo(flags)),
                    DataBusLoadEdge::Alu,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Alu,
                    None,
                    DataBusLoadEdge::W,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Addr),
                    DataBusOutputLevel::Mem,
                    Some(AluOpcode::addhi(flags)),
                    DataBusLoadEdge::Alu,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Alu,
                    None,
                    DataBusLoadEdge::Flags,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Z,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Addr),
                    DataBusOutputLevel::W,
                    None,
                    DataBusLoadEdge::Mem,
                    None,
                ));
            }
            Some(Opcode::FetchRegToReg) => {

                // set top bytes of address
                start_of_ram();

                // set low byte of address based on register input
                add_op(pc_inc);
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));

                // capture address stored in the input register
                for i in 0..=2 {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::wxyz(i as usize),
                        None,
                    ));

                    if i != 2 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }

                // set that address
                for i in 0..=2 {
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::wxyz(i as usize),
                        None,
                        addr_loads[i],
                        None,
                    ));

                    if i == 0 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::wxyz(i as usize),
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                    }
                }

                // read the value from memory
                for (i, edge) in wxyz_loads.iter().enumerate() {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        *edge,
                        None,
                    ));
                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }

                // set top bytes of address
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Imm,
                    None,
                    DataBusLoadEdge::Addr2,
                    Some(0x8),
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Imm,
                    None,
                    DataBusLoadEdge::Addr1,
                    Some(0),
                ));

                // set low byte of address based on register output
                add_op(pc_inc);
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));

                // write value to memory-register
                for (i, out) in wxyz_outs.iter().enumerate() {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        *out,
                        None,
                        DataBusLoadEdge::Mem,
                        None,
                    ));
                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }
            }
            Some(Opcode::FetchAbsToReg) => {
                for data_bus_load in &[DataBusLoadEdge::Addr0, DataBusLoadEdge::Addr1, DataBusLoadEdge::Addr2] {
                    add_op(pc_inc);
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        *data_bus_load,
                        None,
                    ));

                    if *data_bus_load == DataBusLoadEdge::Addr0 {
                        add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                    }
                }

                for i in 0..=3 {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::wxyz(i as usize),
                        None,
                    ));

                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }

                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Imm,
                    None,
                    DataBusLoadEdge::Addr2,
                    Some(0x8),
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Imm,
                    None,
                    DataBusLoadEdge::Addr1,
                    Some(0),
                ));
                add_op(pc_inc);
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));
                add_op(MicroOp::new(
                    Some(AddressBusOutputLevel::Pc),
                    DataBusOutputLevel::Mem,
                    None,
                    DataBusLoadEdge::Addr0,
                    None,
                ));
                for i in 0..=3 {
                    add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::wxyz(i as usize),
                        None,
                        DataBusLoadEdge::Mem,
                        None,
                    ));

                    if i != 3 {
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            Some(AluOpcode::AddLoNoCarry),
                            DataBusLoadEdge::Alu,
                            Some(1),
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                    }
                }
            }
            Some(Opcode::Multiply) => {
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Y,
                    None,
                    DataBusLoadEdge::In1,
                    None,
                ));
                add_op(MicroOp::new(
                    None,
                    DataBusOutputLevel::Imm,
                    Some(AluOpcode::Special),
                    DataBusLoadEdge::Alu,
                    Some(0),
                ));
            }
            _ => {}
        }

        if print {
            println!("# common exit");
        }
        if *inc_pc.borrow() {
            add_op(pc_inc);
        }
        add_op(MicroOp::new(
            None,
            DataBusOutputLevel::Next,
            None,
            DataBusLoadEdge::W, // doesn't matter
            None,
        ));

        let uop_count = *uop_count.borrow();
        assert!(uop_count < MAX_UOPS);

        let filler_bytes = 2 * (MAX_UOPS - uop_count);
        let halt = halt.emit();
        assert_eq!(halt.0, halt.1);
        if print {
            println!("{}*{:02x}", filler_bytes, halt.0);
        }
        for _ in 0..filler_bytes {
            out.borrow_mut().push(halt.0);
        }
    }

    vec_out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack() {
        let entry = MicroEntry {
            flags: 0xF.into(),
            instruction: 0xCC,
        };

        assert_eq!([0xCC, 0xF], entry.pack_lsb());
    }
}
