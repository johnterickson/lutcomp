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

// fn to_output_level(r: &Register) -> OutputLevel {
//     match r {
//         Register::A => OutputLevel::A,
//         Register::B => OutputLevel::B,
//         Register::C => OutputLevel::C,
//         Register::Pc => OutputLevel::Pc,
//         Register::SerialIn => OutputLevel::TTYin,
//         Register::SerialOut => OutputLevel::Halt,
//         Register::Reserved6 => OutputLevel::Halt,
//         Register::Reserved7 => OutputLevel::Halt,
//     }
// }

// fn to_load_edge(r: &Register) -> LoadEdge {
//     match r {
//         Register::A => LoadEdge::A,
//         Register::B => LoadEdge::B,
//         Register::C => LoadEdge::C,
//         Register::Pc => panic!(),
//         Register::SerialIn => LoadEdge::TTYin,
//         Register::SerialOut => LoadEdge::TTYout,
//         _ => unimplemented!(),
//     }
// }

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
    pub immediate: Integer<i8, packed_bits::Bits4>,
}

impl MicroOp {
    fn new(
        address_bus_out: Option<AddressBusOutputLevel>,
        data_out: DataBusOutputLevel,
        alu_opcode: Option<AluOpcode>,
        data_bus_load: DataBusLoadEdge,
        immediate: Option<i8>,
    ) -> MicroOp {
        assert_eq!(data_bus_load == DataBusLoadEdge::Alu, alu_opcode.is_some());

        MicroOp {
            data_bus_out: data_out,
            alu_opcode: alu_opcode.unwrap_or(AluOpcode::AddLo),
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
        DataBusOutputLevel::Imm,
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
                "# addr:{:04x} inst:{:02x}={:?} flags:[{:?}] opcode:{:?}",
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
                    "#  addr:{:04x} uop:{:?}",
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
                            Some(AluOpcode::AddLo),
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

                    // Apply carry
                    let carry_value = if flags.contains(Flags::CARRY) { 1 } else { 0 };
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        Some(AluOpcode::AddLo),
                        DataBusLoadEdge::Alu,
                        Some(carry_value),
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        wxyz_loads[i],
                        None,
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        Some(AluOpcode::AddHi),
                        DataBusLoadEdge::Alu,
                        Some(carry_value),
                    ));
                    add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Flags,
                        None,
                    ));

                    // if carry bit is set at this point, it
                    // also means that wxyz[i] is zero and that
                    // 1) pc[1+i] + wxyz[i] cannot overflow
                    // 2) wxyz[i] := pc[i+1]

                    if flags.contains(Flags::CARRY) {
                        let op = MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::wxyz(i),
                            None,
                        );
                        add_op(op);
                        add_op(op);
                        add_op(op);
                        add_op(op);
                        add_op(op);
                    } else {
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
                            Some(AluOpcode::AddLo),
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
                            Some(AluOpcode::AddHi),
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
            }
            Some(Opcode::Halt) => {
                add_op(halt);
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
