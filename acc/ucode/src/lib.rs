extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::IntoEnumIterator;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;
use std::cell::RefCell;

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
        println!("{:02x} {:02x}", self.emit().0, self.emit().1);
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

pub fn ucode() {
    println!("v2.0 raw");

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

    // let mut uops = Vec::new();
    for encoded_inst in 0u16..(1 << 12) {
        let bytes: &[u8; 2] = &encoded_inst.to_be_bytes();
        let inst = MicroEntry::unpack(bytes).expect(&format!(
            "Could not decode {:02x}={:?}.",
            encoded_inst, bytes
        ));

        let flags = Flags::from_bits_truncate(*inst.flags);
        let opcode = Opcode::iter().nth(inst.instruction as usize);

        let base_address = encoded_inst as usize * MAX_UOPS * 2;

        let inc_pc = RefCell::new(true);
        let uop_count = RefCell::new(0);

        let add_op = |u: MicroOp| {
            println!(
                "# @{:04x} [{:?}] {:?} {:?}",
                base_address, &flags, &opcode, &u
            );
            u.print();
            *uop_count.borrow_mut() += 1;
        };

        let jmp_abs = || {
            add_op(pc_inc);
            add_op(MicroOp::new(
                Some(AddressBusOutputLevel::Pc),
                DataBusOutputLevel::Mem,
                None,
                DataBusLoadEdge::Addr0,
                None,
            ));
            add_op(pc_inc);
            add_op(MicroOp::new(
                Some(AddressBusOutputLevel::Pc),
                DataBusOutputLevel::Mem,
                None,
                DataBusLoadEdge::Addr1,
                None,
            ));
            add_op(pc_inc);
            add_op(MicroOp::new(
                Some(AddressBusOutputLevel::Pc),
                DataBusOutputLevel::Mem,
                None,
                DataBusLoadEdge::Addr2,
                None,
            ));
            add_op(MicroOp::new(
                Some(AddressBusOutputLevel::Addr),
                DataBusOutputLevel::Pc,
                None,
                DataBusLoadEdge::PcR,
                None,
            ));

            *inc_pc.borrow_mut() = false;
        };

        println!("# common prelude");
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
            _ => {}
        }

        if *inc_pc.borrow() {
            println!("# common exit");
            add_op(pc_inc);
        }

        let uop_count = *uop_count.borrow();
        assert!(uop_count < MAX_UOPS);

        let halt = halt.emit();
        assert_eq!(halt.0, halt.1);
        println!("{}*{:02x}", 2 * (MAX_UOPS - uop_count), halt.0);
    }
}
