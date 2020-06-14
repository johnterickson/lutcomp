
fn main() {
    println!("v2.0 raw");

    // let inputs = ["IMM","ADDR","PC","MEM"];
    let outputs = ["ACC","ADDR","PC","MEM"];

    let instructions = 16u8;
    for i in 0..instructions {
        for acc in 0..=255u8 {
            for in2 in 0..=255u8 {
                print!("# {:01x}{:02x}{:02x} ", i, acc, in2);

                match i {
                    0b0000..=0b0011 => {
                        let dst = i;
                        println!("COPY_[IMM,ADDR,PC,MEM]_TO_{}", outputs[dst as usize]);
                        print!("{:02x}{:02x}",  dst,  in2);
                    },
                    0b0100 => {
                        if in2 <= 0x3 {
                            let dst = in2 & 0x3;
                            println!("COPY_ACC_TO_{}", outputs[dst as usize]);
                            print!("{:02x}{:02x}",  dst,  acc);
                        } else {
                            println!("COPY_ACC_TO_ RESERVED");
                            print!("80FF");
                        }
                    }
                    0b0101 => {
                        println!("ADD");
                        let sum = (acc as u16) + (in2 as u16);
                        let carry = ((sum >> 8) & 0x1) as u8;
                        let flags = (carry << 2) | 0; // ACC
                        print!("{:02x}{:02x}", flags, (sum & 0xFF) as u8);
                    },
                    _ => {
                        println!("UNUSED");
                        print!("80FF");
                    }
                }

                println!();
            }
        }
    }
}
