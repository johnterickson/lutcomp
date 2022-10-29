extern crate strum;
#[macro_use]
extern crate strum_macros;

use std::{collections::{hash_map::DefaultHasher}, hash::Hasher, convert::TryInto, borrow::Cow};

extern crate packed_struct;
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

use lazy_static::lazy_static;
lazy_static! {
    static ref UCODE_AND_HASH: (Vec<(u16, &'static str, u32)>, u32) = ucode(false);
    pub static ref UCODE_HASH: u32 = UCODE_AND_HASH.1;
    pub static ref UCODE: Vec<(u16, &'static str, u32)> = UCODE_AND_HASH.0.clone();
}

pub const MAJOR_VERSION: u8 = 1;
pub const MINOR_VERSION: u8 = 2;
pub const PATCH_VERSION: u8 = 0;

#[derive(Clone, Copy, Display, Debug, Eq, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum DataBusLoadEdge {
    PcInc = 0,
    NoOp = 1,
    In1 = 2,
    Alu = 3,
    Mem = 4,
    IR0 = 5,
    IoXCp = 6,
    Flags = 7,
    W = 8,
    X = 9,
    Y = 10,
    Z = 11,
    Addr0 = 12,
    Addr1 = 13,
    Addr2 = 14,
    //unused = 15,
}

impl DataBusLoadEdge {
    pub fn wxyz(i: usize) -> Self {
        match i {
            0 => Self::W,
            1 => Self::X,
            2 => Self::Y,
            3 => Self::Z,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Display, Debug, Eq, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum AddressBusOutputLevel {
    Addr = 0,
    Pc = 1,
}

#[derive(Clone, Copy, Display, Debug, Eq, PartialEq)]
#[derive(EnumCount, EnumIter, EnumString)]
#[derive(PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
pub enum DataBusOutputLevel {
    Next = 0,
    Halt = 1,
    Imm = 2,
    Alu = 3,
    Mem = 4,
    IoReadyToWrite = 5,
    PcSPE = 6,
    IoXData = 7,
    W = 8,
    X = 9,
    Y = 10,
    Z = 11,
    Addr0 = 12,
    Addr1 = 13,
    Addr2 = 14,
    IoReadyToRead = 15,
}

impl DataBusOutputLevel {
    pub fn is_addr(&self) -> bool {
        matches!(self, Self::Addr0 | Self::Addr1 | Self::Addr2)
    }

    pub fn addr(i: usize) -> Self {
        match i {
            0 => Self::Addr0,
            1 => Self::Addr1,
            2 => Self::Addr2,
            _ => panic!(),
        }
    }

    pub fn wxyz(i: usize) -> Self {
        match i {
            0 => Self::W,
            1 => Self::X,
            2 => Self::Y,
            3 => Self::Z,
            _ => panic!(),
        }
    }
}

const UPC_BITS: usize = 7;
const MAX_UOPS: usize = 1 << UPC_BITS;

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
    Addr(AddressBusOutputLevel, usize),
    Mem(AddressBusOutputLevel),
}

impl MicroOp {
    fn create(out: Output, load: Load) -> MicroOp {
        let load_addr = match load {
            Load::Mem(a) => Some(a),
            _ => None,
        };

        let out_addr = match out {
            Output::Mem(a) | Output::Addr(a, _) => Some(a),
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
            Output::Direct(o) => {
                assert!(!o.is_addr());
                (o, None)
            },
            Output::Imm(i) => (DataBusOutputLevel::Imm, Some(i)),
            Output::Mem(_) => (DataBusOutputLevel::Mem, None),
            Output::Addr(_, reg) => {
                (DataBusOutputLevel::addr(reg), None)
            },
        };

        MicroOp::new(address_bus_out, data_out, alu_opcode, data_bus_load, immediate)
    }

    fn new(address_bus_out: Option<AddressBusOutputLevel>, data_bus_out: DataBusOutputLevel, alu_opcode: Option<AluOpcode>, data_bus_load: DataBusLoadEdge, immediate: Option<u8>) -> MicroOp {
        if data_bus_out != DataBusOutputLevel::PcSPE {
            assert_eq!(data_bus_load == DataBusLoadEdge::Mem || data_bus_out == DataBusOutputLevel::Mem || data_bus_out.is_addr(), address_bus_out.is_some());
        }
        assert_eq!(data_bus_load == DataBusLoadEdge::Alu, alu_opcode.is_some());
        assert_eq!(data_bus_out == DataBusOutputLevel::Imm, immediate.is_some());

        if DataBusLoadEdge::IR0 == data_bus_load {
            // because IR0 affects IOSEL, make sure it doesn't coincide with IO ops
            assert_ne!(DataBusOutputLevel::IoXData, data_bus_out);
            assert_ne!(DataBusOutputLevel::IoReadyToRead, data_bus_out);
            assert_ne!(DataBusOutputLevel::IoReadyToWrite, data_bus_out);

            // in fact, it should always be memory
            assert_eq!(DataBusOutputLevel::Mem, data_bus_out);
        }

        if data_bus_out == DataBusOutputLevel::Next {
            assert_eq!(None, address_bus_out);
            assert_eq!(None, alu_opcode);
            assert_eq!(DataBusLoadEdge::W, data_bus_load);
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
        let immediate = immediate.unwrap_or_default();
        assert!(immediate <= 0xF);
        let immediate = immediate.into();
        MicroOp {
            data_bus_out,
            alu_opcode,
            address_bus_out,
            data_bus_load,
            immediate,
        }
    }

    fn emit_bytes(&self) -> (u8, u8) {
        let bytes = self.pack().unwrap();
        (bytes[0], bytes[1])
    }

    fn emit_word(&self) -> u16 {
        u16::from_le_bytes(self.pack().unwrap())
    }

    pub fn print(&self) {
        println!("{:04x}", self.emit_word());
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
    vec_out: Vec<(u16,&'static str, u32)>,
    print: bool,
    base_address: usize,
    uop_count: usize,
    inc_pc: bool,
    possible_carry_pending: bool,
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
            possible_carry_pending: false,
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

        // checks
        match u.alu_opcode {
            AluOpcode::AddHiNoCarry | AluOpcode::AddHiCarry => { self.possible_carry_pending = true; }
            _ => {}
        }

        if self.print {
            let mut file = Cow::Borrowed(file);
            if file.contains('\\') {
                file = Cow::Owned(file.chars().map(|c| if c == '\\' {'/'} else {c}).collect());
            }
            println!("#  addr:{:05x} uop:{:?} source:{}:{}", 
                self.base_address + self.uop_count, &u, file, line);
            u.print();
        }
        let bytes = u.emit_word();
        self.vec_out.push((bytes, file, line));
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

    fn enable_interrupts(&mut self, flags: Flags) {
        add!(self, Output::Imm((Flags::CHANGE_INTERRUPTS | Flags::INTERRUPTS_ENABLED).bits() >> 4), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(flags.bits()), Load::Alu(AluOpcode::Or));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
    }

    fn disable_interrupts(&mut self, flags: Flags) {
        add!(self, Output::Imm((Flags::CHANGE_INTERRUPTS & !Flags::INTERRUPTS_ENABLED).bits() >> 4), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
        add!(self, Output::Imm(flags.bits()), Load::Alu(AluOpcode::Or));
        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
    }

    fn build(&mut self) -> (Vec<(u16, &'static str, u32)>,u32) {
        if self.print {
            println!("v2.0 raw");
        }

        let mut hash_nibble_indices = Vec::new();

        let halt = MicroOp::new(
            None,
            DataBusOutputLevel::Halt,
            None,
            DataBusLoadEdge::NoOp, //same value as Halt
            None,
        );

        // let mut instruction_lengths = BTreeMap::new();

        // let mut uops = Vec::new();
        for encoded_inst in 0u16..(1 << 12) {
            let bytes: &[u8; 2] = &encoded_inst.to_le_bytes();
            let inst = MicroEntry::unpack_lsb(bytes);
            let flags = Flags::from_bits_truncate(*inst.flags);
            let opcode = Opcode::from_primitive(inst.instruction);

            self.base_address = encoded_inst as usize * MAX_UOPS;
            self.uop_count = 0;
            self.inc_pc = true;
            self.possible_carry_pending = false;

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
                Some(Opcode::HaltNoCode) => {
                    self.add_op(halt, file!(), line!());
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
                _ if inst.instruction >= 0x80 => {

                    // store flags - including INTERRUTPS_ENABLED+CHANGE_INTERRUPTS
                    let interrupt_previous_flags_addr = common::INTERRUPT_PREVIOUS_FLAGS.to_le_bytes();
                    assert_eq!(common::INTERRUPT_PREVIOUS_FLAGS % 4, 0);
                    assert_eq!(0, interrupt_previous_flags_addr[3]);
                    assert_eq!(0, interrupt_previous_flags_addr[2] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[2]), Load::Direct(DataBusLoadEdge::Addr2));
                    assert_eq!(0, interrupt_previous_flags_addr[1] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[1]), Load::Direct(DataBusLoadEdge::Addr1));
                    assert_eq!(0, interrupt_previous_flags_addr[0] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[0]), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Imm((Flags::CHANGE_INTERRUPTS | Flags::INTERRUPTS_ENABLED).bits() >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(flags.bits()), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Mem(AddressBusOutputLevel::Addr));

                    // make sure we leave this instruction with interrupts disabled
                    add!(self, Output::Imm((Flags::CHANGE_INTERRUPTS & !Flags::INTERRUPTS_ENABLED).bits() >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

                    // save PC to WXY and compare to zero
                    add!(self, Output::Imm(Flags::ZERO.bits()), Load::Direct(DataBusLoadEdge::Flags));
                    for (i, load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        add!(self, Output::Addr(AddressBusOutputLevel::Pc, i), Load::Direct(*load));
                        add!(self, Output::Imm(0), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Addr(AddressBusOutputLevel::Pc, i), Load::Alu(AluOpcode::AddHiNoCarry));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                        add!(self, Output::Imm(flags.bits()), Load::Alu(AluOpcode::And));
                        add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));
                    }

                    // load up INTERRUPT_PC address
                    // (or jump back to zero, but with interrupts disabled this time)
                    if flags.contains(Flags::ZERO) {
                        for load in Ucode::ADDR_LOADS.iter().take(3) {
                            add!(self, Output::Imm(0), Load::Direct(*load));
                        }
                    } else {
                        let interrupt_pc_addr = common::INTERRUPT_PREVIOUS_PC.to_le_bytes();
                        assert_eq!(common::INTERRUPT_PREVIOUS_PC % 4, 0);
                        assert_eq!(interrupt_previous_flags_addr[3], interrupt_pc_addr[3]);
                        assert_eq!(interrupt_previous_flags_addr[2], interrupt_pc_addr[2]);
                        assert_eq!(interrupt_previous_flags_addr[1], interrupt_pc_addr[1]);

                        // save PC (in WXY) to *INTERRUPT_PC
                        for (i, out) in Ucode::WXYZ_OUTS.iter().take(3).enumerate() {
                            let addr0 = interrupt_pc_addr[0].checked_add(i.try_into().unwrap()).unwrap();
                            assert_eq!(0, addr0 & 0xF0);
                            add!(self, Output::Imm(addr0), Load::Direct(DataBusLoadEdge::Addr0));
                            add!(self, Output::Direct(*out), Load::Mem(AddressBusOutputLevel::Addr));
                        }

                        // load up INTERRUPT_ISR address
                        let interrupt_isr_addr = common::INTERRUPT_ISR.to_lsb_bytes();
                        assert_eq!(interrupt_pc_addr[3], interrupt_isr_addr[3]);
                        assert_eq!(interrupt_pc_addr[2], interrupt_isr_addr[2]);
                        assert_eq!(interrupt_pc_addr[1], interrupt_isr_addr[1]);

                        // load ISR address to WXY
                        for (i, load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                            let addr0 = interrupt_isr_addr[0].checked_add(i.try_into().unwrap()).unwrap();
                            assert_eq!(0, addr0 & 0xF0);
                            add!(self, Output::Imm(addr0), Load::Direct(DataBusLoadEdge::Addr0));
                            add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load));
                        }

                        // copy WXY to Addr[0..=2]
                        for (out, load) in Ucode::WXYZ_OUTS.iter().take(3).zip(&Ucode::ADDR_LOADS) {
                            add!(self, Output::Direct(*out), Load::Direct(*load));
                        }
                    }

                    self.load_pc_from_address_regs();
                }
                Some(Opcode::Noop) => { }
                Some(Opcode::Init) => {
                    add!(self, Output::Imm((Flags::CHANGE_INTERRUPTS & !Flags::INTERRUPTS_ENABLED).bits() >> 4), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(SpecialMicroHelper::SwapNibbles as u8), Load::Alu(AluOpcode::Special));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::In1));
                    add!(self, Output::Imm(Flags::INITIAL.bits()), Load::Alu(AluOpcode::Or));
                    add!(self, Output::Direct(DataBusOutputLevel::Alu), Load::Direct(DataBusLoadEdge::Flags));

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
                Some(Opcode::EnableInterrupts) => {
                    self.enable_interrupts(flags);
                },
                Some(Opcode::DisableInterrupts) => {
                    self.disable_interrupts(flags);
                },
                Some(Opcode::ReturnFromInterrupt) => {

                    // restore the previous flags + INTERRUPTS_ENABLED again
                    let interrupt_previous_flags_addr = common::INTERRUPT_PREVIOUS_FLAGS.to_le_bytes();
                    assert_eq!(common::INTERRUPT_PREVIOUS_FLAGS % 4, 0);
                    assert_eq!(0, interrupt_previous_flags_addr[3]);
                    assert_eq!(0, interrupt_previous_flags_addr[2] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[2]), Load::Direct(DataBusLoadEdge::Addr2));
                    assert_eq!(0, interrupt_previous_flags_addr[1] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[1]), Load::Direct(DataBusLoadEdge::Addr1));
                    assert_eq!(0, interrupt_previous_flags_addr[0] & 0xF0);
                    add!(self, Output::Imm(interrupt_previous_flags_addr[0]), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::Flags));

                    // restore the previous PC
                    let interrupt_pc_addr = common::INTERRUPT_PREVIOUS_PC.to_le_bytes();
                    assert_eq!(common::INTERRUPT_PREVIOUS_PC % 4, 0);
                    assert_eq!(interrupt_previous_flags_addr[3], interrupt_pc_addr[3]);
                    assert_eq!(interrupt_previous_flags_addr[2], interrupt_pc_addr[2]);
                    assert_eq!(interrupt_previous_flags_addr[1], interrupt_pc_addr[1]);

                    // save *INTERRUPT_PC in WXY 
                    for (i, load) in Ucode::WXYZ_LOADS.iter().take(3).enumerate() {
                        let addr0 = interrupt_pc_addr[0].checked_add(i.try_into().unwrap()).unwrap();
                        assert_eq!(0, addr0 & 0xF0);
                        add!(self, Output::Imm(addr0), Load::Direct(DataBusLoadEdge::Addr0));
                        add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(*load));
                    }

                    // copy WXY to Addr[0..=2]
                    for (out, load) in Ucode::WXYZ_OUTS.iter().take(3).zip(&Ucode::ADDR_LOADS) {
                        add!(self, Output::Direct(*out), Load::Direct(*load));
                    }

                    self.load_pc_from_address_regs();
                }
                Some(Opcode::IoReadyToRead) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Direct(DataBusOutputLevel::IoReadyToRead), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(Opcode::IoReadyToWrite) => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));

                    add!(self, Output::Direct(DataBusOutputLevel::IoReadyToWrite), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(o) if Opcode::In0 <= o && o <= Opcode::In7 => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Direct(DataBusOutputLevel::IoXData), Load::Mem(AddressBusOutputLevel::Addr));
                }
                Some(o) if Opcode::Out0 <= o && o <= Opcode::Out7 => {
                    self.start_of_ram();
                    pc_inc!(self);
                    add!(self, Output::Mem(AddressBusOutputLevel::Pc), Load::Direct(DataBusLoadEdge::Addr0));
                    add!(self, Output::Mem(AddressBusOutputLevel::Addr), Load::Direct(DataBusLoadEdge::IoXCp));
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
                        add!(self, Output::Imm(((flags & Flags::INTERRUPTS_ENABLED) | Flags::CARRY).bits()), Load::Alu(AluOpcode::And));
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
                        add!(self, Output::Direct(DataBusOutputLevel::Next), Load::Direct(DataBusLoadEdge::W));
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
                Some(o) => panic!("{:?} not hanlded", o),
                None => {
                    self.add_op(halt, file!(), line!());
                }
            }

            if self.print {
                println!("# common exit");
            }

            let has_another_part = match opcode {
                Some(o) => o.has_another_part(),
                None => false
            };

            if has_another_part {
                add!(self, Output::Imm((flags | Flags::MULTI_OPCODE_INSTRUCTION).bits()), Load::Direct(DataBusLoadEdge::Flags));
            } else if self.possible_carry_pending {
                add!(self, Output::Imm((flags & !Flags::MULTI_OPCODE_INSTRUCTION).bits()), Load::Direct(DataBusLoadEdge::Flags));
            }

            if self.inc_pc {
                pc_inc!(self);
            }
            add!(self, 
                Output::Direct(DataBusOutputLevel::Next),
                Load::Direct(DataBusLoadEdge::W)
            );

            let uop_count = self.uop_count;
            assert!(uop_count <= MAX_UOPS, "{} > {} for {:?}", uop_count, MAX_UOPS, &opcode);

            // match instruction_lengths.entry(inst.instruction) {
            //     std::collections::btree_map::Entry::Vacant(e) => {e.insert((flags, uop_count));},
            //     std::collections::btree_map::Entry::Occupied(e) => {
            //         assert_eq!(e.get().1, uop_count, "instruction 0x{:02x} {:?} had length {} for flags {:?}, but has length {} for flags {:?}",
            //             inst.instruction, opcode, e.get().1, e.get().0, uop_count, flags);
            //     }
            // }

            let uop_remaining = MAX_UOPS - uop_count;
            if self.print {
                println!("# Filling in remaining {} uops of {:?} with HALT", uop_remaining, opcode);
            }
            assert_eq!(halt.emit_bytes().0, halt.emit_bytes().1);
            let halt = halt.emit_word();
            if self.print {
                println!("{}*{:04x}", uop_remaining, halt);
            }
            for _ in 0..uop_remaining {
                self.vec_out.push((halt, file!(), line!()));
            }
        }

        assert_eq!(0, hash_nibble_indices.len() % 8);

        let hash = {
            let mut hasher = DefaultHasher::new();
            let bytes: Vec<u8> = self.vec_out.iter().map(|(w,_,_)| w.to_le_bytes()).flatten().collect();
            hasher.write(&bytes);
            hasher.finish()
        };

        let hash: u32 = (hash % 0x1_0000_0000).try_into().unwrap();
        {
            let hash = hash.to_le_bytes();
            for (nib_index, uop_index) in hash_nibble_indices.iter().enumerate() {
                let nib_index = nib_index % 8;
                let nibble = hash[nib_index/2];
                let nibble = if nib_index % 2 == 0 { nibble & 0xF } else { nibble >> 4 };
                assert_eq!(nibble & 0xF0, 0);
                let w = self.vec_out.get_mut(*uop_index).unwrap();
                assert_eq!((w.0 >> 4) & 0xF, nib_index.try_into().unwrap(), "{:02x}", w.0);
                w.0 &= 0xFF0F;
                let nibble: u16 = nibble.into();
                w.0 |= nibble << 4;
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

pub fn ucode(print: bool) -> (Vec<(u16, &'static str, u32)>, u32) {
    let mut ucode = Ucode::new(print);
    ucode.build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack() {
        assert_eq!(
            [   Opcode::Halt as u8, 
                Flags::ARITHMETIC.bits().into()], 
            (MicroEntry { 
                flags: Flags::ARITHMETIC.bits().into(), 
                instruction: Opcode::Halt as u8
            }).pack_lsb());

        assert_eq!(
            [   Opcode::Halt as u8 | 0x80, 
                Flags::ARITHMETIC.bits().into()], 
            (MicroEntry { 
                flags: Flags::ARITHMETIC.bits().into(), 
                instruction: Opcode::Halt as u8  | 0x80
            }).pack_lsb());
    }

    // #[test]
    // fn version_bump() {
    //     let mut hasher = DefaultHasher::new();
    //     let bytes: Vec<u8> = self::UCODE.iter().map(|(b,_,_)| *b).collect();
    //     hasher.write(&bytes);
    //     let hash = hasher.finish();
    //     let hash = hash % 0x1_0000_0000;
        
    //     assert_eq!(3367832912, hash); // if you have to change this, also change the version
    //     assert_eq!(MAJOR_VERSION, 1);
    //     assert_eq!(MINOR_VERSION, 2);
    //     assert_eq!(PATCH_VERSION, 0);
    // }
}
