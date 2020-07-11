extern crate strum;
#[macro_use]
extern crate strum_macros;
use strum::IntoEnumIterator;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
use packed_struct::prelude::*;

use std::collections::BTreeMap;
use std::{io::BufWriter, str::FromStr};

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString, PartialEq, PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum Opcode {
    Copy = 0,
    Add = 1,
    Or = 2,
    Xor = 3,
    And = 4,
    Rotate  = 5,
    AddIfZero = 6,
    Reserved7 = 7,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString, PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum SecondInputSelect {
    Reserved0 = 0,
    Imm = 1,
    MemImm = 2,
    MemMemImm = 3,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString, PrimitiveEnum_u8)]
#[strum(serialize_all = "lowercase")]
enum OutputSelect {
    Acc = 0,
    Reserved1 = 1,
    MemImm = 2,
    MemMemImm = 3,
}

#[derive(Debug, PackedStruct)]
#[packed_struct(size_bytes="1", endian="lsb", bit_numbering = "msb0")]
pub struct Instruction {

    #[packed_field(bits = "5..=7", ty = "enum")]
    op: Opcode,
    #[packed_field(bits = "3..=4", ty = "enum")]
    out: OutputSelect,
    #[packed_field(bits = "1..=2", ty = "enum")]
    in2: SecondInputSelect,
    #[packed_field(bits = "0")]
    reserved: bool
}



#[derive(Debug, PackedStruct)]
#[packed_struct(endian="lsb", bit_numbering = "msb0")]
pub struct LutEntry {
    #[packed_field(bits = "0..=7")]
    in2: u8,
    #[packed_field(bits = "8..=15")]
    in1: u8,
    #[packed_field(bits = "16..=18", ty = "enum")]
    op: Opcode,
    #[packed_field(bits = "19..=23")]
    unused: Integer<u8, packed_bits::Bits5>,
}

fn alu() {
    println!("v2.0 raw");

    for encoded_entry in 0u32..=0x7FFFF {
        let bytes = encoded_entry.to_le_bytes();
        assert_eq!(0, bytes[3]);
        let bytes = [bytes[0], bytes[1], bytes[2]];
        let entry = LutEntry::unpack(&bytes).unwrap();
        println!("# {:05x} {:?} {:?}", encoded_entry, &bytes, &entry);
        assert_eq!(0, *entry.unused);

        let out = match entry.op {
            Opcode::Copy => entry.in2,
            Opcode::Add => entry.in1.wrapping_add(entry.in2),
            Opcode::Or => entry.in1 | entry.in2,
            Opcode::Xor => entry.in1 ^ entry.in2,
            Opcode::And => entry.in1 & entry.in2,
            Opcode::Rotate => {
                let _mode = (entry.in2 >> 6) & 0b11;
                let amount = (((entry.in2 & 0xf) << 4) as i8) >> 4;
                if amount >= 0 {
                    (entry.in1 << amount) | (entry.in1 >> (8-amount))
                } else {
                    let amount = (-1 *(amount as i16)) as u8;
                    (entry.in1 >> amount) | (entry.in1 << (8-amount))
                }
            },
            Opcode::AddIfZero => 0,
            Opcode::Reserved7 => 0,
        };

        println!("# {:02x}", out);
    }
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum LoadEdge {
    Irl,
    Irh,
    Alu,
    Tmp,
    Ram,
    Pc,
    Acc,
    Addr,
    Reserved9,
    Reserved0,
    ReservedA,
    ReservedB,
    ReservedC,
    ReservedD,
    ReservedE,
    Next,
}

#[derive(Clone, Copy, Display, Debug, EnumCount, EnumIter, EnumString)]
#[strum(serialize_all = "lowercase")]
enum OutputLevel {
    Irl,
    PcInc,
    Alu,
    Tmp,
    Ram,
    Pc,
}

const MAX_UOPS: usize = 128;

struct MicroOp {
    out: OutputLevel,
    load: LoadEdge,
}

impl MicroOp {
    fn emit(&self) -> u8 {
        ((self.out as u8) << 4) | (self.load as u8)
    }

    fn print(&self) {
        println!("# OUT {:?}, LOAD {:?}", self.out, self.load);
        println!("{:02x}", self.emit());
    }
}

fn ucode() {
    println!("v2.0 raw");

    let filler = MicroOp {
        out: OutputLevel::Tmp,
        load: LoadEdge::Next,
    };


    // let mut uops = Vec::new();
    for encoded_inst in 0..=0xFF {
        let inst = Instruction::unpack(&[encoded_inst as u8])
            .expect(&format!("Could not decode {:02x}.", encoded_inst));
        let mut uop_count = 0;

        println!("# {:?}", &inst);

        println!("# common prelude");
        for u in &[
            MicroOp {
                out: OutputLevel::Pc,
                load: LoadEdge::Irl,
            },
            MicroOp {
                out: OutputLevel::PcInc,
                load: LoadEdge::Tmp,
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
        let in_ops : &[MicroOp] = match inst.in2 {
            SecondInputSelect::Reserved0 => { &[] }
            SecondInputSelect::Imm => {
                &[
                    MicroOp {
                        out: OutputLevel::Irl,
                        load: LoadEdge::Alu,
                    }
                ]
            }
            SecondInputSelect::MemImm => {
                &[
                    MicroOp {
                        out: OutputLevel::Irl,
                        load: LoadEdge::Addr,
                    },
                    MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::Alu,
                    }
                ]
            }
            SecondInputSelect::MemMemImm => {
                &[
                    MicroOp {
                        out: OutputLevel::Irl,
                        load: LoadEdge::Addr,
                    },
                    MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::Tmp,
                    },
                    MicroOp {
                        out: OutputLevel::Tmp,
                        load: LoadEdge::Addr,
                    },
                    MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::Alu,
                    }
                ]
            }
        };
        for u in in_ops {
            u.print();
            uop_count += 1;
        }

        println!("# store output");
        let out_ops : &[MicroOp] = match inst.out {
            OutputSelect::Acc => {
                &[
                    MicroOp {
                        out: OutputLevel::Alu,
                        load: LoadEdge::Acc
                    }
                ]
            }
            OutputSelect::Reserved1 => { &[] }
            OutputSelect::MemImm => {
                &[
                    MicroOp {
                        out: OutputLevel::Irl,
                        load: LoadEdge::Addr
                    },
                    MicroOp {
                        out: OutputLevel::Alu,
                        load: LoadEdge::Ram
                    }
                ]
            },
            OutputSelect::MemMemImm => {
                &[
                    MicroOp {
                        out: OutputLevel::Irl,
                        load: LoadEdge::Addr,
                    },
                    MicroOp {
                        out: OutputLevel::Ram,
                        load: LoadEdge::Tmp,
                    },
                    MicroOp {
                        out: OutputLevel::Tmp,
                        load: LoadEdge::Addr,
                    },
                    MicroOp {
                        out: OutputLevel::Alu,
                        load: LoadEdge::Ram,
                    }
                ]
            }
        };
        for u in out_ops {
            u.print();
            uop_count += 1;
        }


        println!("# common exit");
        for u in &[
            MicroOp {
                out: OutputLevel::PcInc,
                load: LoadEdge::Tmp,
            },
            MicroOp {
                out: OutputLevel::Tmp,
                load: LoadEdge::Next,
            },
        ] {
            u.print();
            uop_count += 1;
        }

        assert!(uop_count < MAX_UOPS);
        println!("{}*{:02x}", MAX_UOPS - uop_count, filler.emit());
    }
}

fn main() {
    use std::fs::File;
    use std::io::prelude::*;

    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("ucode") => { ucode(); }
        Some("alu") => { alu(); }
        Some(unknown) => eprintln!("Unknown arg '{}'", unknown),
        None => eprintln!("no arg provided"),
    }
}
