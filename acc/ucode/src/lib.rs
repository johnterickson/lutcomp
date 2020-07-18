extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::IntoEnumIterator;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use common::*;

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum LoadEdge {
    Irl = 0,
    Irh = 1,
    Alu = 2,
    Noop = 3,
    Ram = 4,
    PcR = 5,
    In1 = 6,
    Addr = 7,
    A = 8,
    B = 9,
    C = 10,
    PcFromPcr = 11,
    PcClk = 12,
    TTYin = 13,
    TTYout = 14,
    Reserved15 = 15,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum OutputLevel {
    Irl = 0,
    PcClkEn = 1,
    Alu = 2,
    Noop = 3,
    Ram = 4,
    Reserved5 = 5,
    Pc = 6,
    Reserved7 = 7,
    A = 8,
    B = 9,
    C = 10,
    Reserved11 = 11,
    Reserved12 = 12,
    TTYin = 13,
    Next = 14,
    Halt = 15,
}

fn to_output_level(r: &Register) -> OutputLevel {
    match r {
        Register::A => OutputLevel::A,
        Register::B => OutputLevel::B,
        Register::C => OutputLevel::C,
        Register::Pc => OutputLevel::Pc,
        Register::SerialIn => OutputLevel::TTYin,
        Register::SerialOut => OutputLevel::Halt,
        Register::Reserved6 => OutputLevel::Halt,
        Register::Reserved7 => OutputLevel::Halt,
    }
}

fn to_load_edge(r: &Register) -> LoadEdge {
    match r {
        Register::A => LoadEdge::A,
        Register::B => LoadEdge::B,
        Register::C => LoadEdge::C,
        Register::Pc => panic!(),
        Register::SerialIn => LoadEdge::TTYin,
        Register::SerialOut => LoadEdge::TTYout,
        _ => unimplemented!(),
    }
}

const MAX_UOPS: usize = 32;

#[derive(Clone, Copy, Debug)]
struct MicroOp {
    out: OutputLevel,
    load: LoadEdge,
}

impl MicroOp {
    fn emit(&self) -> (u8,u8) {
        (self.out as u8, self.load as u8)
    }

    fn print(&self) {
        println!("# OUT {:?}, LOAD {:?}", self.out, self.load);
        println!("{:02x} {:02x}", self.emit().0, self.emit().1);
    }
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes = "2", endian = "lsb", bit_numbering = "lsb0")]
pub struct MicroEntry {
    #[packed_field(bits = "0..=7")]
    mode_specific: u8,
    #[packed_field(bits = "8..=10", ty = "enum")]
    out_reg: Register,
    #[packed_field(bits = "11..=12")]
    mode: Integer<u8, packed_bits::Bits2>,
}

pub fn ucode() {
    println!("v2.0 raw");

    let halt = MicroOp {
        out: OutputLevel::Halt,
        load: LoadEdge::Next,
    };

    // let mut uops = Vec::new();
    for encoded_inst in 0u16..(1 << 13) {
        let bytes: &[u8; 2] = &encoded_inst.to_be_bytes();
        let inst = MicroEntry::unpack(bytes).expect(&format!(
            "Could not decode {:02x}={:?}.",
            encoded_inst, bytes
        ));

        let base_address = encoded_inst as usize * MAX_UOPS * 2;

        println!("# @{:04x} {:?} {:?}", base_address, &inst, &bytes);

        let mode = InstructionModeDiscriminants::iter()
            .nth(*inst.mode as usize)
            .unwrap();
        assert_eq!(
            *inst.mode,
            match mode {
                InstructionModeDiscriminants::Imm8 => 0,
                InstructionModeDiscriminants::Regs => 1,
                InstructionModeDiscriminants::Imm4 => 2,
                InstructionModeDiscriminants::MemOutRegs => 3,
            }
        );

        let mut uop_count = 0;

        println!("# common prelude");
        for u in &[
            MicroOp {
                out: OutputLevel::Pc,
                load: LoadEdge::Irl,
            },
            MicroOp {
                out: OutputLevel::PcClkEn,
                load: LoadEdge::PcClk,
            },
            MicroOp {
                out: OutputLevel::Pc,
                load: LoadEdge::Irh,
            },
        ] {
            u.print();
            uop_count += 1;
        }

        println!("# prep second input and drive alu");
        let mut in_ops = Vec::new();
        match mode {
            InstructionModeDiscriminants::Imm8 => {
                in_ops.push(MicroOp {
                    out: OutputLevel::A,
                    load: LoadEdge::In1,
                });
                in_ops.push(MicroOp {
                    out: OutputLevel::Irl,
                    load: LoadEdge::Alu,
                });
            }
            InstructionModeDiscriminants::Imm4 => {
                let mode2 = PackedInstructionMode2::unpack(&[inst.mode_specific]).unwrap();
                let in1_level = to_output_level(&mode2.in1_reg);
                if mode2.in1_indirect {
                    in_ops.push(MicroOp {
                        out: in1_level,
                        load: LoadEdge::Addr,
                    });
                    in_ops.push(MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::In1,
                    });
                } else {
                    in_ops.push(MicroOp {
                        out: in1_level,
                        load: LoadEdge::In1,
                    });
                }

                in_ops.push(halt); //todo
            }
            InstructionModeDiscriminants::Regs | InstructionModeDiscriminants::MemOutRegs => {
                let mode1 = PackedInstructionMode1or3::unpack(&[inst.mode_specific]).unwrap();
                {
                    let in1_level = to_output_level(&mode1.in1_reg);
                    if mode1.in1_indirect {
                        in_ops.push(MicroOp {
                            out: in1_level,
                            load: LoadEdge::Addr,
                        });
                        in_ops.push(MicroOp {
                            out: OutputLevel::Ram,
                            load: LoadEdge::In1,
                        });
                    } else {
                        in_ops.push(MicroOp {
                            out: in1_level,
                            load: LoadEdge::In1,
                        });
                    }
                }
                {
                    let in2_level = to_output_level(&mode1.in2_reg);
                    if mode1.in1_indirect {
                        in_ops.push(MicroOp {
                            out: in2_level,
                            load: LoadEdge::Addr,
                        });
                        in_ops.push(MicroOp {
                            out: OutputLevel::Ram,
                            load: LoadEdge::Alu,
                        });
                    } else {
                        in_ops.push(MicroOp {
                            out: in2_level,
                            load: LoadEdge::Alu,
                        });
                    }
                }
            }
        };
        for u in in_ops {
            u.print();
            uop_count += 1;
        }

        println!("# store output");
        let mut out_ops = Vec::new();
        match mode {
            InstructionModeDiscriminants::MemOutRegs => {
                out_ops.push(MicroOp {
                    out: to_output_level(&inst.out_reg),
                    load: LoadEdge::Addr,
                });
                out_ops.push(MicroOp {
                    out: OutputLevel::Alu,
                    load: LoadEdge::Ram,
                });
            }
            _ => match inst.out_reg {
                Register::Pc => {
                    out_ops.push(MicroOp {
                        out: OutputLevel::Alu,
                        load: LoadEdge::PcR,
                    });
                    out_ops.push(MicroOp {
                        out: OutputLevel::Noop,
                        load: LoadEdge::PcFromPcr,
                    });
                }
                Register::Reserved6 | Register::Reserved7 => {
                    out_ops.push(halt);
                }
                r => {
                    out_ops.push(MicroOp {
                        out: OutputLevel::Alu,
                        load: to_load_edge(&r),
                    });
                    out_ops.push(MicroOp {
                        out: OutputLevel::PcClkEn,
                        load: LoadEdge::PcClk,
                    });
                }
            },
        };
        for u in out_ops {
            u.print();
            uop_count += 1;
        }

        println!("# common exit");
        for u in &[MicroOp {
            out: OutputLevel::Next,
            load: LoadEdge::Next,
        }] {
            u.print();
            uop_count += 1;
        }

        assert!(uop_count < MAX_UOPS);

        let halt = halt.emit();
        assert_eq!(halt.0, halt.1);
        println!("{}*{:02x}", 2*(MAX_UOPS - uop_count), halt.0);
    }
}