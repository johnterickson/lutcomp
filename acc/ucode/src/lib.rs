extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::{EnumCount, IntoEnumIterator};

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

use lazy_static::lazy_static;
use std::convert::TryInto;
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

enum Load {
    Direct(DataBusLoadEdge),
    Mem(AddressBusOutputLevel),
    Alu(AluOpcode),
}

enum Output {
    Direct(DataBusOutputLevel),
    Imm(u8),
    Mem(AddressBusOutputLevel),
}

impl MicroOp {
    fn copy(out: Output, load: Load) -> MicroOp {
        let load_addr = match load {
            Load::Mem(a) => Some(a),
            _ => None,
        };
        let out_addr = match out {
            Output::Mem(a) => Some(a),
            _ => None,
        };

        let address_bus_out = match (out_addr, load_addr) {
            (Some(load_addr), Some(out_addr)) => {
                assert_eq!(load_addr, out_addr);
                Some(load_addr)
            }
            (Some(load_addr), None) => Some(load_addr),
            (None, Some(out_addr)) => Some(out_addr),
            (None, None) => None,
        };

        let (alu_opcode, data_bus_load) = match load {
            Load::Direct(e) => (None, e),
            Load::Alu(o) => (Some(o), DataBusLoadEdge::Alu),
            Load::Mem(_) => (None, DataBusLoadEdge::Mem),
        };

        let (data_out, immediate) = match out {
            Output::Direct(o) => (o, None),
            Output::Imm(i) => (DataBusOutputLevel::Imm, Some(i)),
            Output::Mem(_) => (DataBusOutputLevel::Mem, None),
        };

        MicroOp::new(
            address_bus_out,
            data_out,
            alu_opcode,
            data_bus_load,
            immediate,
        )
    }

    fn new(
        address_bus_out: Option<AddressBusOutputLevel>,
        data_out: DataBusOutputLevel,
        alu_opcode: Option<AluOpcode>,
        data_bus_load: DataBusLoadEdge,
        immediate: Option<u8>,
    ) -> MicroOp {
        assert_eq!(
            data_bus_load == DataBusLoadEdge::Mem || data_out == DataBusOutputLevel::Mem,
            address_bus_out.is_some()
        );
        assert_eq!(data_bus_load == DataBusLoadEdge::Alu, alu_opcode.is_some());
        assert_eq!(data_out == DataBusOutputLevel::Imm, immediate.is_some());

        let default_addr_bus = if DataBusOutputLevel::Halt == data_out {
            AddressBusOutputLevel::Addr
        } else {
            AddressBusOutputLevel::Pc
        };

        let address_bus_out = address_bus_out.unwrap_or(default_addr_bus);

        match data_bus_load {
            DataBusLoadEdge::Addr0 | DataBusLoadEdge::Addr1 | DataBusLoadEdge::Addr2 => {
                if address_bus_out == AddressBusOutputLevel::Addr {
                    assert_ne!(data_out, DataBusOutputLevel::Mem, "reading from memory and changing address is unstable");
                }
            },
            _ => {}
        }

        MicroOp {
            data_bus_out: data_out,
            alu_opcode: alu_opcode.unwrap_or(AluOpcode::AddLoNoCarry),
            address_bus_out,
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

// macro_rules! add {
//     ($u:expr) => {
//         if print {
//             println!(
//                 "#  addr:{:05x} uop:{:?} {}:{}",
//                 base_address + *uop_count.borrow() * 2,
//                 &$u,
//                 file!(), line!()
//             );
//             $u.print();
//         }
//         let mut out = out.borrow_mut();
//         out.push($u.emit().0);
//         out.push($u.emit().1);
//         *uop_count.borrow_mut() += 1;
//     };
// }

struct Ucode {
    vec_out: Vec<u8>,
    print: bool,

    base_address: usize,
    uop_count: usize,
    inc_pc: bool,
}

impl Ucode {
    fn new(print: bool) -> Ucode {
        Ucode {
            vec_out: Vec::new(),
            print,
            base_address: 0,
            uop_count: 0,
            inc_pc: true,
        }
    }

    const ADDR_LOADS: [DataBusLoadEdge; 3] =
        [
            DataBusLoadEdge::Addr0,
            DataBusLoadEdge::Addr1,
            DataBusLoadEdge::Addr2,
        ];

        const WXYZ_OUTS: [DataBusOutputLevel; 4] =
        [
            DataBusOutputLevel::W,
                DataBusOutputLevel::X,
                DataBusOutputLevel::Y,
                DataBusOutputLevel::Z,
        ];

    const WXYZ_LOADS: [DataBusLoadEdge; 4] =
        [
            DataBusLoadEdge::W,
            DataBusLoadEdge::X,
            DataBusLoadEdge::Y,
            DataBusLoadEdge::Z,
        ];

    fn pc_inc(&mut self) {
        self.add_op(MicroOp::new(
            None,
            DataBusOutputLevel::W,
            None,
            DataBusLoadEdge::PcInc,
            None,
        ));
    }

    fn add_op(&mut self, u: MicroOp) {
        if self.print {
            println!(
                "#  addr:{:05x} uop:{:?}",
                self.base_address + self.uop_count * 2,
                &u
            );
            u.print();
        }
        self.vec_out.push(u.emit().0);
        self.vec_out.push(u.emit().1);
        self.uop_count += 1;
    }

    fn add(&mut self, out: Output, load: Load) {
        self.add_op(MicroOp::copy(out, load));
    }

    fn start_of_ram(&mut self) {
        self.add(Output::Imm(0x8), Load::Direct(DataBusLoadEdge::Addr2));
        self.add(Output::Imm(0x0), Load::Direct(DataBusLoadEdge::Addr1));
    }

    fn jmp_abs(&mut self) {
        for addr_edge in &Ucode::ADDR_LOADS {
            self.pc_inc();
            self.add_op(MicroOp::new(
                Some(AddressBusOutputLevel::Pc),
                DataBusOutputLevel::Mem,
                None,
                *addr_edge,
                None,
            ));
        }

        self.inc_pc = false;
    }

    fn build(&mut self) -> Vec<u8> {
        if self.print {
            println!("v2.0 raw");
        }

        let halt = MicroOp::new(
            None,
            DataBusOutputLevel::Halt,
            None,
            DataBusLoadEdge::IR0, //same value as Halt
            None,
        );

        // let mut uops = Vec::new();
        for encoded_inst in 0u16..(1 << 12) {
            let bytes: &[u8; 2] = &encoded_inst.to_le_bytes();
            let inst = MicroEntry::unpack_lsb(bytes);
            let flags = Flags::from_bits_truncate(*inst.flags);
            let opcode = Opcode::iter().filter(|o| *o as u8 == inst.instruction).next();

            self.base_address = encoded_inst as usize * MAX_UOPS * 2;
            self.uop_count = 0;
            self.inc_pc = true;

            if self.print {
                println!("#");
                println!("#");
                println!(
                    "# addr:{:05x} inst:{:02x}={:?} flags:[{:?}] opcode:{:?}",
                    self.base_address, encoded_inst, &inst, &flags, &opcode
                );
                println!("#");
            }

            if self.print {
                println!("# common prelude");
            }
            self.add(
                Output::Mem(AddressBusOutputLevel::Pc),
                Load::Direct(DataBusLoadEdge::IR0),
            );
            self.add(
                Output::Mem(AddressBusOutputLevel::Pc),
                Load::Direct(DataBusLoadEdge::IR0),
            );

            match opcode {
                Some(Opcode::Jmp) => {
                    self.jmp_abs();
                }
                Some(Opcode::Jz) => {
                    if flags.contains(Flags::ZERO) {
                        self.jmp_abs();
                    }
                }
                Some(Opcode::LoadImm8) => {
                    self.start_of_ram();
                    self.pc_inc();
                    self.add(
                        Output::Mem(AddressBusOutputLevel::Pc),
                        Load::Direct(DataBusLoadEdge::Addr0),
                    );
                    self.pc_inc();

                    self.add(
                        Output::Mem(AddressBusOutputLevel::Pc),
                        Load::Direct(DataBusLoadEdge::W),
                    );
                    self.add(
                        Output::Direct(DataBusOutputLevel::W),
                        Load::Mem(AddressBusOutputLevel::Addr),
                    );
                }
                Some(Opcode::LoadImm32) => {
                    self.start_of_ram();
                    self.pc_inc();
                    self.add(
                        Output::Mem(AddressBusOutputLevel::Pc),
                        Load::Direct(DataBusLoadEdge::Addr0),
                    );
                    self.add(
                        Output::Mem(AddressBusOutputLevel::Pc),
                        Load::Direct(DataBusLoadEdge::In1),
                    );

                    for i in 0..=3 {
                        self.pc_inc();
                        self.add(
                            Output::Mem(AddressBusOutputLevel::Pc),
                            Load::Direct(DataBusLoadEdge::W),
                        );
                        self.add(
                            Output::Direct(DataBusOutputLevel::W),
                            Load::Mem(AddressBusOutputLevel::Addr),
                        );

                        if i != 3 {
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(
                                Output::Direct(DataBusOutputLevel::Alu),
                                Load::Direct(DataBusLoadEdge::In1),
                            );
                            self.add(
                                Output::Direct(DataBusOutputLevel::Alu),
                                Load::Direct(DataBusLoadEdge::Addr0),
                            );
                        }
                    }
                }
                Some(Opcode::Load8) => {
                    self.start_of_ram();
                    self.pc_inc();

                    // copy address into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        self.add(Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // copy value into W
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));

                    // store value
                    self.start_of_ram();
                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));
                },
                Some(Opcode::Store8) => {
                    self.start_of_ram();
                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    // copy value into Z
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));

                    // copy address into wxy
                    self.pc_inc();
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        self.add(Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // store value to memory
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Store32Part1) => {
                    self.start_of_ram();
                    
                    // store regA in Z
                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Z));

                    // store regB's addr in WXY
                    self.pc_inc();
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    for i in 0..=1 {
                        self.start_of_ram();
                        self.add(Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        self.add(Output::Imm(0), Load::Alu(AluOpcode::AddLoNoCarry));

                        // load up store address
                        for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                            self.add(Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                        }

                        self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            self.add(Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                            self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                        }
                    }
                }
                Some(Opcode::Store32Part2) => {
                    for i in 2..=3 {
                        self.start_of_ram();
                        self.add(Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        self.add(Output::Imm(0), Load::Alu(AluOpcode::AddLoNoCarry));

                        // load up store address
                        for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                            self.add(Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                        }

                        self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            self.add(Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                            self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                        }
                    }
                }
                Some(Opcode::Load32) => {
                    self.start_of_ram();
                    self.pc_inc();

                    // copy address into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().enumerate() {
                        if i == 0 {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }
                    
                    // copy address in wxy to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        self.add(Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // copy addr0 to Z
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::Z));

                    // copy value into WXYZ
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().enumerate() {
                        if i != 0 {
                            self.add(Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // store value
                    self.start_of_ram();
                    self.pc_inc();
                    for (i, reg) in Ucode::WXYZ_OUTS.iter().enumerate() {
                        if i == 0 {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(i as u8), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }

                        self.add(Output::Direct(*reg), Load::Mem(AddressBusOutputLevel::Addr));
                    }
                }
                Some(Opcode::Mul8Part1) => {
                    self.start_of_ram();
                    self.pc_inc();
                    // 16-bit "A" in WX
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));
                    self.add(Output::Imm(0), Load::Direct(DataBusLoadEdge::X));

                    // 8-bit "B" in Y
                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Y));
                }
                Some(Opcode::Mul8Part2) => {
                    self.inc_pc = false; // handle this manually

                    self.add(Output::Direct(DataBusOutputLevel::Y), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                    if flags.contains(Flags::ZERO) {
                        self.pc_inc();
                        self.add(Output::Direct(DataBusOutputLevel::Next), Load::Direct(DataBusLoadEdge::W));
                    } else {
                        // filler
                        self.add(Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr0));
                        self.add(Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr0));
                    }
                    
                    //   Z = if b & 1 { 0xFF } else { 0x00 }

                    // point memory to product0, three times to 
                    self.add(Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr0));

                    // store mask in Z
                    self.add(Output::Direct(DataBusOutputLevel::Y), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(SpecialMicroHelper::AllBitsIfOdd as u8), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                    // mask W with Z
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                    // add product0 masked W
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::AddHiNoCarry));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::AddLoNoCarry));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // point memory to product1
                    self.add(Output::Imm(1), Load::Direct(DataBusLoadEdge::Addr0));

                    // mask X with Z
                    self.add(Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                    // add product1 masked X
                    self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::addlo(flags)));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                    
                    //   need to left-shift [WX] by 1

                    // create shift command and store in Z: 01==Shift 00=Rotate
                    self.add(Output::Imm(0b0100), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(1), Load::Alu(AluOpcode::Or));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                    // rotate X by 1
                    self.add(Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::X));

                    // rotate W by 1
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));

                    // create ~1 mask and store in Z
                    self.add(Output::Imm(0x1), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                    // clear out lowest bit from X
                    self.add(Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::X));

                    // grab lowest bit from W and OR into X
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(1), Load::Alu(AluOpcode::And));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::X), Load::Alu(AluOpcode::Or));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::X));

                    // clear out lowest bit from W
                    self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));

                    //   need to right-shift Y by 1
                    self.add(Output::Direct(DataBusOutputLevel::Y), Load::Direct(DataBusLoadEdge::In1));
                    self.add(Output::Imm(SpecialMicroHelper::RightShiftByOne as u8), Load::Alu(AluOpcode::Special));
                    self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Y));
                }
                // Some(Opcode::ShiftLeftSubByteRegImm) => {

                //     // W holds register number
                //     self.pc_inc();
                //     self.add_copy(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                //     self.add_copy(Output::Imm(0b0101), Load::Direct(DataBusLoadEdge::In1));
                //     self.add_copy(Output::Imm(SpecialSpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));

                //     // PC points to shift amount
                //     self.pc_inc();
                //     self.add_copy(Output::Imm(0b111), Load::Direct(DataBusLoadEdge::In1));
                //     self.add_copy(Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::And));

                //     // read first byte to X
                //     self.start_of_ram();
                //     self.add_copy(Output::Mem(AddressBusOutputLevel::W), Load::Direct(DataBusLoadEdge::Addr0));
                //     self.add_copy(Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::X));

                //     todo!();
                // }
                Some(Opcode::AddRegImm) => {
                    self.start_of_ram();

                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                    for i in 0..=3 {
                        self.pc_inc();
                        self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::addlo(flags)));
                        self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::X));
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::addhi(flags)));
                        self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                        self.add(Output::Direct(DataBusOutputLevel::X), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }
                }
                Some(Opcode::OrRegImm) => {
                    self.start_of_ram();

                    self.pc_inc();
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                    for i in 0..=3 {
                        self.pc_inc();
                        self.add(Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                        self.add(Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::Or));
                        self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            self.add(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            self.add(Output::Imm(1), Load::Alu(AluOpcode::AddLoNoCarry));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            self.add(Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }
                }
                Some(Opcode::Halt) => {
                    self.add_op(halt);
                }
                Some(Opcode::Or32) => {
                    self.start_of_ram();

                    for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z] {
                        self.pc_inc();
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            *edge,
                            None,
                        ));
                    }

                    for i in 0..=3 {
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::X,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Y,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            Some(AluOpcode::Or),
                            DataBusLoadEdge::Alu,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Z,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Mem,
                            None,
                        ));

                        if i != 3 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                None,
                                DataBusLoadEdge::In1,
                                Some(1),
                            ));
                            for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                                self.add_op(MicroOp::new(
                                    None,
                                    DataBusOutputLevel::wxyz(*r as usize),
                                    Some(AluOpcode::AddLoNoCarry),
                                    DataBusLoadEdge::Alu,
                                    None,
                                ));
                                self.add_op(MicroOp::new(
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
                Some(Opcode::RegsAdd32Part1) => {
                    for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z] {
                        self.pc_inc();
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            *edge,
                            None,
                        ));
                    }

                    self.start_of_ram();

                    for _ in 0..=2 {
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::X,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::In1,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Y,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            Some(AluOpcode::addlo(flags)),
                            DataBusLoadEdge::Alu,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::W,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            Some(AluOpcode::addhi(flags)),
                            DataBusLoadEdge::Alu,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Alu,
                            None,
                            DataBusLoadEdge::Flags,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Z,
                            None,
                            DataBusLoadEdge::Addr0,
                            None,
                        ));
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::W,
                            None,
                            DataBusLoadEdge::Mem,
                            None,
                        ));

                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::Imm,
                            None,
                            DataBusLoadEdge::In1,
                            Some(1),
                        ));
                        for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::wxyz(*r as usize),
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                None,
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::wxyz(*r as usize),
                                None,
                            ));
                        }
                    }
                }
                Some(Opcode::RegsAdd32Part2) => {
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::X,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Y,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addlo(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::W,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::Mem,
                        Some(AluOpcode::addhi(flags)),
                        DataBusLoadEdge::Alu,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Alu,
                        None,
                        DataBusLoadEdge::Flags,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Z,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Addr),
                        DataBusOutputLevel::W,
                        None,
                        DataBusLoadEdge::Mem,
                        None,
                    ));
                }
                Some(Opcode::FetchRegToReg) => {
                    // set top bytes of address
                    self.start_of_ram();

                    // set low byte of address based on register input
                    self.pc_inc();
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));

                    // capture address stored in the input register
                    for i in 0..=2 {
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::wxyz(i as usize),
                            None,
                        ));

                        if i != 2 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                Some(1),
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                            self.add_op(MicroOp::new(
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
                        self.add_op(MicroOp::new(
                            None,
                            DataBusOutputLevel::wxyz(i as usize),
                            None,
                            Ucode::ADDR_LOADS[i],
                            None,
                        ));

                        if i == 0 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::wxyz(i as usize),
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                        }
                    }

                    // read the value from memory
                    for (i, edge) in Ucode::WXYZ_LOADS.iter().enumerate() {
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            None,
                            *edge,
                            None,
                        ));
                        if i != 3 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                Some(1),
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::Addr0,
                                None,
                            ));
                        }
                    }

                    // set top bytes of address
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        None,
                        DataBusLoadEdge::Addr2,
                        Some(0x8),
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        None,
                        DataBusLoadEdge::Addr1,
                        Some(0),
                    ));

                    // set low byte of address based on register output
                    self.pc_inc();
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));

                    // write value to memory-register
                    for (i, out) in Ucode::WXYZ_OUTS.iter().enumerate() {
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            *out,
                            None,
                            DataBusLoadEdge::Mem,
                            None,
                        ));
                        if i != 3 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                Some(1),
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                            self.add_op(MicroOp::new(
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
                    for data_bus_load in &[
                        DataBusLoadEdge::Addr0,
                        DataBusLoadEdge::Addr1,
                        DataBusLoadEdge::Addr2,
                    ] {
                        self.pc_inc();
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Pc),
                            DataBusOutputLevel::Mem,
                            None,
                            *data_bus_load,
                            None,
                        ));

                        if *data_bus_load == DataBusLoadEdge::Addr0 {
                            self.add_op(MicroOp::new(
                                Some(AddressBusOutputLevel::Pc),
                                DataBusOutputLevel::Mem,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                        }
                    }

                    for i in 0..=3 {
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::Mem,
                            None,
                            DataBusLoadEdge::wxyz(i as usize),
                            None,
                        ));

                        if i != 3 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                Some(1),
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::Addr0,
                                None,
                            ));
                        }
                    }

                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        None,
                        DataBusLoadEdge::Addr2,
                        Some(0x8),
                    ));
                    self.add_op(MicroOp::new(
                        None,
                        DataBusOutputLevel::Imm,
                        None,
                        DataBusLoadEdge::Addr1,
                        Some(0),
                    ));
                    self.pc_inc();
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::In1,
                        None,
                    ));
                    self.add_op(MicroOp::new(
                        Some(AddressBusOutputLevel::Pc),
                        DataBusOutputLevel::Mem,
                        None,
                        DataBusLoadEdge::Addr0,
                        None,
                    ));
                    for i in 0..=3 {
                        self.add_op(MicroOp::new(
                            Some(AddressBusOutputLevel::Addr),
                            DataBusOutputLevel::wxyz(i as usize),
                            None,
                            DataBusLoadEdge::Mem,
                            None,
                        ));

                        if i != 3 {
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Imm,
                                Some(AluOpcode::AddLoNoCarry),
                                DataBusLoadEdge::Alu,
                                Some(1),
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::In1,
                                None,
                            ));
                            self.add_op(MicroOp::new(
                                None,
                                DataBusOutputLevel::Alu,
                                None,
                                DataBusLoadEdge::Addr0,
                                None,
                            ));
                        }
                    }
                }
                _ => {
                    self.add_op(halt);
                }
            }

            if self.print {
                println!("# common exit");
            }
            if self.inc_pc {
                self.pc_inc();
            }
            self.add_op(MicroOp::new(
                None,
                DataBusOutputLevel::Next,
                None,
                DataBusLoadEdge::W, // doesn't matter
                None,
            ));

            let uop_count = self.uop_count;
            assert!(uop_count < MAX_UOPS);

            let filler_bytes = 2 * (MAX_UOPS - uop_count);
            let halt = halt.emit();
            assert_eq!(halt.0, halt.1);
            if self.print {
                println!("{}*{:02x}", filler_bytes, halt.0);
            }
            for _ in 0..filler_bytes {
                self.vec_out.push(halt.0);
            }
        }

        self.vec_out.clone()
    }
}

pub fn ucode(print: bool) -> Vec<u8> {
    let mut ucode = Ucode::new(print);
    ucode.build()
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
