extern crate strum;
#[macro_use]
extern crate strum_macros;

use std::{collections::hash_map::DefaultHasher, hash::Hasher, convert::TryInto, borrow::Cow};

use strum::IntoEnumIterator;

extern crate packed_struct;
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

use lazy_static::lazy_static;
lazy_static! {
    static ref UCODE_AND_HASH: (Vec<(u8, &'static str, u32)>, u32) = ucode(false);
    pub static ref UCODE_HASH: u32 = UCODE_AND_HASH.1;
    pub static ref UCODE: Vec<(u8, &'static str, u32)> = UCODE_AND_HASH.0.clone();
}

pub const MAJOR_VERSION: u8 = 1;
pub const MINOR_VERSION: u8 = 1;
pub const PATCH_VERSION: u8 = 1;

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
    None = 6,
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
    PcSPE = 6,
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
    pub immediate: Integer<u8, packed_bits::Bits::<4>>,
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
    fn create(out: Output, load: Load) -> MicroOp {
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

        MicroOp::new(address_bus_out, data_out, alu_opcode, data_bus_load, immediate)
    }

    fn new(address_bus_out: Option<AddressBusOutputLevel>, data_bus_out: DataBusOutputLevel, alu_opcode: Option<AluOpcode>, data_bus_load: DataBusLoadEdge, immediate: Option<u8>) -> MicroOp {
        if data_bus_out != DataBusOutputLevel::PcSPE {
            assert_eq!(data_bus_load == DataBusLoadEdge::Mem || data_bus_out == DataBusOutputLevel::Mem, address_bus_out.is_some());
        }
        assert_eq!(data_bus_load == DataBusLoadEdge::Alu, alu_opcode.is_some());
        assert_eq!(data_bus_out == DataBusOutputLevel::Imm, immediate.is_some());

        if data_bus_out == DataBusOutputLevel::Next {
            assert_eq!(None, address_bus_out);
            assert_eq!(None, alu_opcode);
            assert_eq!(DataBusLoadEdge::None, data_bus_load);
            assert_eq!(None, immediate);
        }

        if data_bus_load == DataBusLoadEdge::None {
            assert_eq!(DataBusOutputLevel::Next, data_bus_out);
            assert_eq!(None, address_bus_out);
            assert_eq!(None, alu_opcode);
            assert_eq!(None, immediate);
        }

        let default_addr_bus = if DataBusOutputLevel::Halt == data_bus_out {
            AddressBusOutputLevel::Addr
        } else {
            AddressBusOutputLevel::Pc
        };

        let address_bus_out = address_bus_out.unwrap_or(default_addr_bus);

        match data_bus_load {
            DataBusLoadEdge::Addr0 | DataBusLoadEdge::Addr1 | DataBusLoadEdge::Addr2 => {
                if address_bus_out == AddressBusOutputLevel::Addr {
                    assert_ne!(data_bus_out, DataBusOutputLevel::Mem, "reading from memory and changing address is unstable");
                }
            }
            _ => {}
        }

        let alu_opcode = alu_opcode.unwrap_or(AluOpcode::AddLo);
        let immediate = immediate.unwrap_or_default().into();
        MicroOp {
            data_bus_out,
            alu_opcode,
            address_bus_out,
            data_bus_load,
            immediate,
        }
    }

    fn emit(&self) -> (u8, u8) {
        let bytes = self.pack().unwrap();
        (bytes[0], bytes[1])
    }

    pub fn print(&self) {
        println!("{:02x} {:02x}", self.emit().1, self.emit().0);
    }
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct MicroEntry {
    #[packed_field(bits = "0..=7")]
    pub instruction: u8,
    #[packed_field(bits = "8..=11")]
    pub flags: Integer<u8, packed_bits::Bits::<4>>,
}

impl MicroEntry {
    pub fn pack_lsb(&self) -> [u8; 2] {
        let bytes = self.pack().unwrap();
        [bytes[1], bytes[0]]
    }

    pub fn unpack_lsb(bytes: &[u8; 2]) -> MicroEntry {
        let bytes = [bytes[1], bytes[0]];
        MicroEntry::unpack(&bytes).unwrap()
    }
}

pub struct Ucode {
    vec_out: Vec<(u8,&'static str, u32)>,
    print: bool,

    base_address: usize,
    uop_count: usize,
    inc_pc: bool,
}

macro_rules! add {
    ($self:expr, $out:expr, $load:expr) => {
        $self.add_with_source($out,$load, file!(), line!());
    };
}

macro_rules! pc_inc {
    ($self:expr) => {
        $self.pc_inc(file!(), line!());
    };
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

    const ADDR_LOADS: [DataBusLoadEdge; 3] = [DataBusLoadEdge::Addr0, DataBusLoadEdge::Addr1, DataBusLoadEdge::Addr2];

    const WXYZ_OUTS: [DataBusOutputLevel; 4] = [DataBusOutputLevel::W, DataBusOutputLevel::X, DataBusOutputLevel::Y, DataBusOutputLevel::Z];

    const WXYZ_LOADS: [DataBusLoadEdge; 4] = [DataBusLoadEdge::W, DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z];

    fn pc_inc(&mut self, file: &'static str, line: u32) {
        self.add_op(
            MicroOp::create(Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::PcInc)),
            file, line);
    }

    fn add_op(&mut self, u: MicroOp, file: &'static str, line: u32) {
        if self.print {
            let mut file = Cow::Borrowed(file);
            if file.contains('\\') {
                file = Cow::Owned(file.chars().map(|c| if c == '\\' {'/'} else {c}).collect());
            }
            println!("#  addr:{:05x} uop:{:?} source:{}:{}", 
                self.base_address + self.uop_count * 2, &u, file, line);
            u.print();
        }
        let bytes = u.emit();
        self.vec_out.push((bytes.0, file, line));
        self.vec_out.push((bytes.1, file, line));
        self.uop_count += 1;
    }

    fn add_with_source(&mut self, out: Output, load: Load, file: &'static str, line: u32) {
        self.add_op(MicroOp::create(out, load), file, line);
    }

    fn start_of_ram(&mut self) {
        let ram_addr = (common::RAM_MIN as u32).to_lsb_bytes();
        add!(self, Output::Imm(ram_addr[2]), Load::Direct(DataBusLoadEdge::Addr2));
        add!(self, Output::Imm(ram_addr[1]), Load::Direct(DataBusLoadEdge::Addr1));
    }

    fn load_pc_from_address_regs(&mut self) {
        self.add_op(
            MicroOp::new(
                Some(AddressBusOutputLevel::Addr),
                DataBusOutputLevel::PcSPE,
                None,
                DataBusLoadEdge::PcInc,
                None,
            ), 
            file!(),
            line!());

        self.inc_pc = false;
    }

    fn jmp_abs(&mut self) {
        for addr_edge in &Ucode::ADDR_LOADS {
            pc_inc!(self);
            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(*addr_edge));
        }
        self.load_pc_from_address_regs();
    }

    fn flags_from(&mut self, out: Output) {
        add!(self, out, Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
    }

    fn some_flags_from(&mut self, out: Output, current_flags: Flags, flags_to_capture: Flags) {
        // capture the zero and neg bits
        add!(self, out, Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
        self.some_flags_from_preceding_addhi(current_flags, flags_to_capture);
    }

    fn some_flags_from_preceding_addhi(&mut self, current_flags: Flags, flags_to_capture: Flags) {
        let bits = flags_to_capture.bits();
        // capture the zero and neg bits
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(bits), Load::Alu(AluOpcode::And));

        // or it into the flags
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(current_flags.bits() & !bits), Load::Alu(AluOpcode::Or));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
    }

    fn parallel_op_8(&mut self, alu_op: AluOpcode) {
        self.start_of_ram();

        pc_inc!(self);
        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

        pc_inc!(self);
        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(alu_op));

        pc_inc!(self);
        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

        self.flags_from(Output::Direct(DataBusOutputLevel::Alu));
    }


    fn parallel_op_32(&mut self, alu_op: AluOpcode) {
        self.start_of_ram();

        for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y] {
            pc_inc!(self);
            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(*edge));
        }

        pc_inc!(self);

        for i in 0..=3 {
            add!(self, Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::Addr0));
            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
            add!(self, Output::Direct(DataBusOutputLevel::Y), Load::Direct(DataBusLoadEdge::Addr0));
            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(alu_op));
            if i == 0 {
                add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
            } else {
                add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
            }
            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

            if i != 3 {
                add!(self, Output::Imm(1), Load::Direct(DataBusLoadEdge::In1));

                for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                    if i == 0 && *r == RwRegister::Z {
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::AddLo));
                    } else {
                        add!(self, Output::Direct(DataBusOutputLevel::wxyz(*r as usize)), Load::Alu(AluOpcode::AddLo));
                    }
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::wxyz(*r as usize)));
                }
            }
        }
    }

    fn build(&mut self) -> (Vec<(u8, &'static str, u32)>,u32) {
        if self.print {
            println!("v2.0 raw");
        }

        let mut hash_nibble_indices = Vec::new();

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

            let noop = MicroOp::create(Output::Imm(flags.bits()), Load::Direct(DataBusLoadEdge::Flags));

            if self.print {
                println!("#");
                println!("#");
                println!("# addr:{:05x} inst:{:02x}={:?} flags:[{:?}] opcode:{:?}", self.base_address, encoded_inst, &inst, &flags, &opcode);
                println!("#");
            }

            if self.print {
                println!("# common prelude");
            }
            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::IR0));

            match opcode {
                Some(Opcode::Init) => {
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Flags));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::W));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::X));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Y));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Z));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr1));
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::Addr2));
                },
                Some(Opcode::ClearCarry) => {
                    add!(self, Output::Imm((flags & !Flags::CARRY).bits()), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::JmpImm) => {
                    self.jmp_abs();
                }
                Some(Opcode::JmpReg) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    for (i,load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load));

                        if i != 2 {
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    for (out, load) in Ucode::WXYZ_OUTS.iter().take(3).zip(&Ucode::ADDR_LOADS) {
                        add!(self, Output::Direct(*out), Load::Direct(*load));
                    }

                    self.load_pc_from_address_regs();
                }
                Some(Opcode::JmpMem) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    // capture address of address in WXY
                    for (i,load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load));

                        if i != 2 {
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    // load address of address into addrs
                    for (out, load) in Ucode::WXYZ_OUTS.iter().take(3).zip(&Ucode::ADDR_LOADS) {
                        add!(self, Output::Direct(*out), Load::Direct(*load));
                    }

                    // load address into wxy
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    for (i,load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load));

                        if i != 2 {
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    // load address into addrs
                    for (out, load) in Ucode::WXYZ_OUTS.iter().take(3).zip(&Ucode::ADDR_LOADS) {
                        add!(self, Output::Direct(*out), Load::Direct(*load));
                    }

                    self.load_pc_from_address_regs();
                }
                Some(Opcode::JcImm) => {
                    if flags.contains(Flags::CARRY) {
                        self.jmp_abs();
                    } else {
                        pc_inc!(self);
                        pc_inc!(self);
                        pc_inc!(self);
                    }
                }
                Some(Opcode::JzImm) => {
                    if flags.contains(Flags::ZERO) {
                        self.jmp_abs();
                    } else {
                        pc_inc!(self);
                        pc_inc!(self);
                        pc_inc!(self);
                    }
                }
                Some(Opcode::JnImm) => {
                    if flags.contains(Flags::NEG) {
                        self.jmp_abs();
                    } else {
                        pc_inc!(self);
                        pc_inc!(self);
                        pc_inc!(self);
                    }
                }
                Some(Opcode::LoadImm8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    pc_inc!(self);

                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Invert8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));

                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                    self.some_flags_from(Output::Direct(DataBusOutputLevel::Alu), flags, 
                        Flags::ZERO | Flags::NEG);
                }
                Some(Opcode::Negate8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    
                    add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::LoadImm32) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));

                    for i in 0..=3 {
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));
                        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }
                }
                Some(Opcode::Copy32) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::X));

                    add!(self, Output::Imm(1), Load::Direct(DataBusLoadEdge::In1));

                    for i in 0..=3 {
                        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));
                        add!(self, Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            add!(self, Output::Direct(DataBusOutputLevel::X), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::X));
                        }
                    }
                }
                Some(Opcode::StoreImm32) => {
                    self.start_of_ram();
                    pc_inc!(self);

                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));

                    // copy address into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    add!(self, Output::Imm(1), Load::Direct(DataBusLoadEdge::In1));

                    for i in 0..=3 {
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Z));
                        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                        }
                    }
                }
                Some(Opcode::Load8) => {
                    self.start_of_ram();
                    pc_inc!(self);

                    // copy address into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to address regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // copy value into W
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));

                    // store value
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Store8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    // copy value into Z
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));

                    // copy address into wxy
                    pc_inc!(self);
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // store value to memory
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));
                }

                Some(Opcode::Push8) => {
                    self.start_of_ram();

                    // load -1 into In1
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    
                    // add 0xFFFFFF to REG_SP (and store a copy in WXY)
                    for i in 0..=3 {
                        add!(self, Output::Imm(REG_SP + i), Load::Direct(DataBusLoadEdge::Addr0));

                        if i !=0 && flags.contains(Flags::CARRY) {
                            // here we are adding 0xFF + 0x01 so we just copy.
                            self.add_op(noop, file!(), line!());
                            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(Ucode::WXYZ_LOADS[i as usize]));
                        } else {
                            // we are adding just 0xFF
                            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(Ucode::WXYZ_LOADS[i as usize]));
                        }
                        
                        if i != 3 {
                            let addhi = if i == 0 { AluOpcode::AddHiNoCarry } else { AluOpcode::addhi(flags) };
                            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(addhi));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                        }

                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i as usize]), Load::Mem(AddressBusOutputLevel::Addr));
                    }

                    pc_inc!(self);

                    // copy value into Z
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // store value to memory
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Pop8) => {
                    self.start_of_ram();

                    // copy sp into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Imm(REG_SP + (i as u8)), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // copy popped value into Z
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));

                    // store into reg
                    pc_inc!(self);
                    self.start_of_ram();
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));


                    // increment sp

                    for i in 0..=2 {
                        add!(self, Output::Imm(REG_SP + (i as u8)), Load::Direct(DataBusLoadEdge::Addr0));

                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                        let addend = if i == 0 { 1 } else { 0 };
                        
                        let addhi = if i == 0 { AluOpcode::AddHiNoCarry } else { AluOpcode::addhi(flags) };
                        add!(self, Output::Imm(addend), Load::Alu(addhi));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                        let carry_inc = if flags.contains(Flags::CARRY_PENDING) { 1 } else { 0 };
                        add!(self, Output::Imm(addend + carry_inc), Load::Alu(AluOpcode::AddLo));

                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                    }

                }
                Some(Opcode::Copy8) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    // copy value into Z
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Z));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::AndImm8) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::OrImm8) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::XorImm8) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    
                    pc_inc!(self);

                    // a | b -> W
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));

                    // ~ (a & b)
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                    // a ^ b == (a | b) & ~( a & b)
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::And8) => self.parallel_op_8(AluOpcode::And),
                Some(Opcode::Or8) => self.parallel_op_8(AluOpcode::Or),
                Some(Opcode::Xor8) => {
                    self.start_of_ram();

                    // a ^ b == (a | b) & ~( a & b)

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::X));


                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    // a | b -> regC
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(DataBusOutputLevel::X), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // a & b -> alu
                    add!(self, Output::Direct(DataBusOutputLevel::X), Load::Alu(AluOpcode::And));

                    // ~(a & b) -> alu -> in1
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                    // a ^ b == (a | b) & ~( a & b)
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    self.flags_from(Output::Direct(DataBusOutputLevel::Alu));
                }
                Some(Opcode::Shift8) => {  // shift(ShiftCommand, (amount)regA, (value) regB, (dest) regC)
                    pc_inc!(self); // now pointing to ShiftCommand
                    self.start_of_ram();

                    // create ALU ShiftArgs cmd in Z
                    // first the upper nibble
                    // 4..=5 mode
                    // 6..=7 SpecialOpcode::Shift

                    // pc holds ShiftCommand
                    // 0..=1 mode
                    // 2..=2 dir
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0x3), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm((SpecialOpcode::Shift as u8) << 2), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));
                    // Z now has upper nibble of ShiftArgs

                    // copy dir flag to  ZERO flag
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0x4), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0x0), Load::Alu(AluOpcode::AddHiNoCarry));
                    self.some_flags_from_preceding_addhi(flags, Flags::ZERO);

                    pc_inc!(self); // now pointing to reg # containing amount
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    if flags.contains(Flags::ZERO) {
                        // shift left so noop
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());
                    } else {
                        // shift right so negate amount
                        add!(self, Output::Imm(SpecialMicroHelper::Negate as u8), Load::Alu(AluOpcode::Special));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    }

                    // get 4 LSB of amount to make final ShiftArgs in Z
                    add!(self, Output::Imm(0x0F), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));


                    pc_inc!(self); // now pointing to reg # containing value to be shifted
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    // compute shift
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::Special));

                    pc_inc!(self); // now pointing to reg # for result
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // update flags
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0x0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::AddCarry8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::addhi(flags)));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                    let inc = if flags.contains(Flags::CARRY_PENDING) { 1 } else { 0 };
                    add!(self, Output::Imm(inc), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Cmp8) | Some(Opcode::Cmp8IfZero) => {
                    let perform_cmp8 = (opcode == Some(Opcode::Cmp8)) || flags.contains(Flags::ZERO);
                    if  perform_cmp8 {
                        self.start_of_ram();

                        // grab B
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        
                        // invert it
                        add!(self, Output::Imm(SpecialMicroHelper::Invert as u8), Load::Alu(AluOpcode::Special));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

                        // add 1 so W = ~B + 1 (and CarryFlag -> Z)
                        add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                        add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddHiNoCarry));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(Flags::CARRY.bits()), Load::Alu(AluOpcode::And));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                        // step to A and load in into In1
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        
                        // add A and W
                        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddHiNoCarry));

                        // OR in the CARRY flag
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::Or));

                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                    } else {
                        pc_inc!(self); //self.start_of_ram();
                        pc_inc!(self);
                        pc_inc!(self); //pc_inc!(self);
                        add!(self, Output::Direct(DataBusOutputLevel::Next), Load::Direct(DataBusLoadEdge::None));
                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!()); //pc_inc!(self);
                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!());
                        self.add_op(noop, file!(), line!());

                        self.add_op(noop, file!(), line!());
                    }
                }
                Some(Opcode::Divide8) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));
                    
                    // quotient in W
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::Divide));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));

                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));

                    // capture the zero bit
                    add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddHiNoCarry));
                    self.some_flags_from_preceding_addhi(flags, Flags::ZERO);
                },
                Some(Opcode::Add8NoCarry) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // capture the zero and neg bit
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddHiNoCarry));
                    self.some_flags_from_preceding_addhi(flags, Flags::ZERO | Flags::NEG);
                },
                Some(Opcode::Add8NoCarryIn) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // capture the zero bit
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                },
                Some(Opcode::TtyIn) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Direct(DataBusOutputLevel::TtyIn), Load::Mem(AddressBusOutputLevel::Addr));

                    // ack it
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::TtyIn));
                }
                Some(Opcode::TtyOut) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::TtyOut));
                }
                Some(Opcode::Store32_1) => {
                    self.start_of_ram();

                    // store regA in Z
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Z));

                    // store regB's addr in WXY
                    pc_inc!(self);
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        if i == 0 {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    for _ in 0..=1 {
                        self.start_of_ram();
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddLo));

                        // load up store address
                        for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                            add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                        }

                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                    }
                }
                Some(Opcode::Store32_2) => {
                    for i in 2..=3 {
                        self.start_of_ram();
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddLo));

                        // load up store address
                        for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                            add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                        }

                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                        }
                    }
                }
                Some(Opcode::Load32) => {
                    self.start_of_ram();
                    pc_inc!(self);

                    // copy address into wxy
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().enumerate() {
                        if i == 0 {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // copy address in wxy to addres regs
                    for (i, load_edge) in Ucode::ADDR_LOADS.iter().enumerate() {
                        add!(self, Output::Direct(Ucode::WXYZ_OUTS[i]), Load::Direct(*load_edge));
                    }

                    // copy addr0 to Z
                    add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::Z));

                    // copy value into WXYZ
                    for (i, load_edge) in Ucode::WXYZ_LOADS.iter().enumerate() {
                        if i != 0 {
                            add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load_edge));
                    }

                    // store value
                    self.start_of_ram();
                    pc_inc!(self);
                    for (i, reg) in Ucode::WXYZ_OUTS.iter().enumerate() {
                        if i == 0 {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                        } else {
                            add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(i as u8), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }

                        add!(self, Output::Direct(*reg), Load::Mem(AddressBusOutputLevel::Addr));
                    }
                }
                Some(Opcode::Mul8_8) => {
                    self.start_of_ram();
                    
                    // Load x & y
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::X));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Y));

                    // determine order of X & Y
                    // first comupte X / Y
                    add!(self, Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(DataBusOutputLevel::Y), Load::Alu(AluOpcode::Divide));

                    // now compute flags for (0 + (X / Y))
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                    let (in1, in2) = if flags.contains(Flags::ZERO) {
                        // X/Y == 0 --> X < Y
                        (DataBusOutputLevel::Y, DataBusOutputLevel::X)
                        
                    } else {
                        // when Y == 0: X >= Y.  Div(X, 0)=0xFF.  0xFF != 0
                        // when Y != 0: Div(X, Y) != 0 implies X >= Y
                        (DataBusOutputLevel::X, DataBusOutputLevel::Y)
                    };

                    add!(self, Output::Direct(in1), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(in2), Load::Alu(AluOpcode::MulLoHi));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::Mul8_16) => {
                    self.start_of_ram();
                    
                    // Load x & y
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::X));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Y));

                    // Load RegC into Z
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Z));

                    // determine order of X & Y
                    // first comupte X / Y
                    add!(self, Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(DataBusOutputLevel::Y), Load::Alu(AluOpcode::Divide));

                    // now compute flags for (0 + (X / Y))
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                    let (in1, in2) = if flags.contains(Flags::ZERO) {
                        // X/Y == 0 --> X < Y
                        (DataBusOutputLevel::Y, DataBusOutputLevel::X)
                        
                    } else {
                        // when Y == 0: X >= Y.  Div(X, 0)=0xFF.  0xFF != 0
                        // when Y != 0: Div(X, Y) != 0 implies X >= Y
                        (DataBusOutputLevel::X, DataBusOutputLevel::Y)
                    };

                    add!(self, Output::Direct(in1), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(in2), Load::Alu(AluOpcode::MulLoHi));

                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // increment RegC is in Z so increment it, so that we point to the next reg
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                    // now compute the high byte
                    let (in1, in2) = if flags.contains(Flags::ZERO) {
                        // X < Y
                        (DataBusOutputLevel::X, DataBusOutputLevel::Y)
                    } else {
                        // X >= Y
                        (DataBusOutputLevel::Y, DataBusOutputLevel::X)
                    };

                    // we'll compute assuming in1 != 0, but correct for that later
                    add!(self, Output::Direct(in1), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::Decrement as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Direct(in2), Load::Alu(AluOpcode::MulLoHi));
                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // check if in1 is 0
                    add!(self, Output::Direct(in1), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                    if flags.contains(Flags::ZERO) {
                        add!(self, Output::Imm(0), Load::Mem(AddressBusOutputLevel::Addr));
                    }
                }
                Some(Opcode::AndImm32) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                    add!(self, Output::Imm(Flags::ZERO.bits()), Load::Direct(DataBusLoadEdge::Z));

                    for i in 0..=3 {
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::And));
                        // store result
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                        // also check result for zero bit
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(Flags::ZERO.bits()), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::OrImm32) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));

                    add!(self, Output::Imm(Flags::ZERO.bits()), Load::Direct(DataBusLoadEdge::Z));

                    for i in 0..=3 {
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::Or));
                        // store result
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                        // also check result for zero bit
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(0), Load::Alu(AluOpcode::AddHiNoCarry));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Alu(AluOpcode::And));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Z));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(Flags::ZERO.bits()), Load::Alu(AluOpcode::And));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                }
                Some(Opcode::Halt) | Some(Opcode::HaltRAM) => {
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::X));
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Y));
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Z));
                    self.add_op(halt, file!(), line!());
                }
                Some(Opcode::Or32) => {
                    self.parallel_op_32(AluOpcode::Or);
                }
                Some(Opcode::And32) => {
                    self.parallel_op_32(AluOpcode::And);
                }
                Some(Opcode::AddCarry32_1) => {
                    self.start_of_ram();

                    // X == reg of src1
                    // Y == reg of src2
                    // Z == reg of dest
                    for edge in &[DataBusLoadEdge::X, DataBusLoadEdge::Y, DataBusLoadEdge::Z] {
                        pc_inc!(self);
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(*edge));
                    }

                    self.write_AddCarry32(0, flags);
                    self.write_AddCarry32(1, flags);
                }
                Some(Opcode::AddCarry32_2) => {
                    self.write_AddCarry32(2, flags);
                    self.write_AddCarry32(3, flags);

                }
                Some(Opcode::AddImm32IgnoreCarry) => {

                    add!(self, Output::Imm(flags.bits()), Load::Direct(DataBusLoadEdge::Z));

                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::W));
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    for i in 0..=3 {
                        pc_inc!(self);

                        let addhi = if i == 0 { AluOpcode::AddHiNoCarry} else { AluOpcode::addhi(flags) };
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(addhi));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                        add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Alu(AluOpcode::AddLo));
                        let inc = if flags.contains(Flags::CARRY_PENDING) { 1 } else { 0 };
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(inc), Load::Alu(AluOpcode::AddLo));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                        if i != 3 {
                            add!(self, Output::Direct(DataBusOutputLevel::W), Load::Direct(DataBusLoadEdge::In1));
                            add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));
                            add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                        }
                    }

                    add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Flags));

                }
                // Some(Opcode::ShiftRight32_1) => {
                //     self.start_of_ram();
                //     pc_inc!(self);
                //     add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                //     add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::W));

                //     // regA in W
                //     // regB in X
                // }
                // Some(Opcode::ShiftRight32_2) => {
                //     self.inc_pc = false; // handle this manually

                //     // subtract 8 from W
                //     add!(self, Output::Imm(8), Load::Direct(DataBusLoadEdge::In1));
                //     add!(self, Output::Imm(SpecialMicroHelper::Negate as u8), Load::Alu(AluOpcode::Special));
                //     add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                //     add!(self, Output::Direct(DataBusOutputLevel::W), Load::Alu(AluOpcode::AddHiNoCarry));
                //     add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                //     if flags.contains(Flags::NEG) {
                //         //less than 8 to-go so 
                //         pc_inc!(self);
                //         add!(self, Output::Direct(DataBusOutputLevel::Next), Load::Direct(DataBusLoadEdge::None));
                //     } else {

                //     }
                // }
                // Some(Opcode::ShiftRight32_3) => {

                // }
                Some(Opcode::GetAluInfo) => {
                    self.start_of_ram();
                    
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::GetInfo as u8), Load::Alu(AluOpcode::Special));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::GetUcodeInfo) => {
                    self.start_of_ram();

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Imm(MAJOR_VERSION >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(MAJOR_VERSION & 0xF), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Imm(MINOR_VERSION >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(MINOR_VERSION & 0xF), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Imm(PATCH_VERSION >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(PATCH_VERSION & 0xF), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    let nib1 = self.vec_out.len();
                    add!(self, Output::Imm(1), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    hash_nibble_indices.push(self.vec_out.len());
                    hash_nibble_indices.push(nib1);
                    add!(self, Output::Imm(0), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(1), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                    let nib3 = self.vec_out.len();
                    add!(self, Output::Imm(3), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    hash_nibble_indices.push(self.vec_out.len());
                    hash_nibble_indices.push(nib3);
                    add!(self, Output::Imm(2), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(2), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                    let nib5 = self.vec_out.len();
                    add!(self, Output::Imm(5), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    hash_nibble_indices.push(self.vec_out.len());
                    hash_nibble_indices.push(nib5);
                    add!(self, Output::Imm(4), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(3), Load::Alu(AluOpcode::AddLo));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Addr0));
                    let nib7 = self.vec_out.len();
                    add!(self, Output::Imm(7), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    hash_nibble_indices.push(self.vec_out.len());
                    hash_nibble_indices.push(nib7);
                    add!(self, Output::Imm(6), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));
                }
                None => {
                    self.add_op(halt, file!(), line!());
                }
            }

            if self.print {
                println!("# common exit");
            }
            if self.inc_pc {
                pc_inc!(self);
            }
            add!(self, 
                Output::Direct(DataBusOutputLevel::Next),
                Load::Direct(DataBusLoadEdge::None)
            );

            let uop_count = self.uop_count;
            assert!(uop_count <= MAX_UOPS, "{} > {} for {:?}", uop_count, MAX_UOPS, &opcode);

            let uop_remaining = MAX_UOPS - uop_count;
            if self.print {
                println!("# Filling in remaining {} uops of {:?} with HALT", uop_remaining, opcode);
            }
            let filler_bytes = 2 * uop_remaining;
            let halt = halt.emit();
            assert_eq!(halt.0, halt.1);
            if self.print {
                println!("{}*{:02x}", filler_bytes, halt.0);
            }
            for _ in 0..filler_bytes {
                self.vec_out.push((halt.0, file!(), line!()));
            }
        }

        assert_eq!(0, hash_nibble_indices.len() % 8);

        let hash = {
            let mut hasher = DefaultHasher::new();
            let bytes: Vec<u8> = self.vec_out.iter().map(|(b,_,_)| *b).collect();
            hasher.write(&bytes);
            hasher.finish()
        };

        let hash: u32 = (hash % 0x1_0000_0000).try_into().unwrap();
        {
            let hash = hash.to_le_bytes();
            for (nib_index, byte_index) in hash_nibble_indices.iter().enumerate() {
                let nib_index = nib_index % 8;
                let nibble = hash[nib_index/2];
                let nibble = if nib_index % 2 == 0 { nibble & 0xF } else { nibble >> 4 };
                assert_eq!(nibble & 0xF0, 0);
                let b = self.vec_out.get_mut(*byte_index).unwrap();
                assert_eq!(b.0 >> 4, nib_index.try_into().unwrap(), "{:02x}", b.0);
                b.0 &= 0x0F;
                b.0 |= nibble << 4;
            }
        }

        (self.vec_out.clone(), hash)
    }

    #[allow(non_snake_case)]
    fn write_AddCarry32(&mut self, i: u8, flags: Flags) {
        // X == reg of src1
        // Y == reg of src2
        // Z == reg of dest

        add!(self, Output::Direct(DataBusOutputLevel::X), Load::Direct(DataBusLoadEdge::Addr0));
        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Direct(DataBusOutputLevel::Y), Load::Direct(DataBusLoadEdge::Addr0));

        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::addhi(flags)));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Alu(AluOpcode::AddLo));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));

        let inc = if flags.contains(Flags::CARRY_PENDING) { 1 } else { 0 };

        add!(self, Output::Imm(inc), Load::Alu(AluOpcode::AddLo));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::W));

        add!(self, Output::Direct(DataBusOutputLevel::Z), Load::Direct(DataBusLoadEdge::Addr0));
        add!(self, Output::Direct(DataBusOutputLevel::W), Load::Mem(AddressBusOutputLevel::Addr));
        if i != 3 {
            add!(self, Output::Imm(1), Load::Direct(DataBusLoadEdge::In1));
            for r in &[RwRegister::X, RwRegister::Y, RwRegister::Z] {
                add!(self, Output::Direct(DataBusOutputLevel::wxyz(*r as usize)), Load::Alu(AluOpcode::AddLo));
                add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::wxyz(*r as usize)));
            }
        }
    }
}

pub fn ucode(print: bool) -> (Vec<(u8, &'static str, u32)>, u32) {
    let mut ucode = Ucode::new(print);
    ucode.build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack() {
        let entry = MicroEntry { flags: 0xF.into(), instruction: 0xCC };

        assert_eq!([0xCC, 0xF], entry.pack_lsb());
    }

    #[test]
    fn version_bump() {
        let mut hasher = DefaultHasher::new();
        let bytes: Vec<u8> = self::UCODE.iter().map(|(b,_,_)| *b).collect();
        hasher.write(&bytes);
        let hash = hasher.finish();
        let hash = hash % 0x1_0000_0000;
        
        assert_eq!(1796100314, hash); // if you have to change this, also change the version
        assert_eq!(MAJOR_VERSION, 1);
        assert_eq!(MINOR_VERSION, 1);
        assert_eq!(PATCH_VERSION, 1);
    }
}
