use std::io::{Read, BufReader, BufRead};

pub enum HexFileLine {
    Data(Vec<HexFileData>),
    Comment(String),
}

pub enum HexFileData {
    Byte(u8),
    Run(u32,u8),
}

pub struct HexFile {
    pub lines: Vec<HexFileLine>,
}

impl HexFile {
    pub const fn header() -> &'static str {
        "v2.0 raw"
    }

    pub fn bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        for line in &self.lines {
            match line {
                HexFileLine::Comment(_) => {},
                HexFileLine::Data(data) => {
                    for data in data {
                        match data {
                            HexFileData::Byte(b) => bytes.push(*b),
                            HexFileData::Run(count, b) => {
                                for _ in 0..*count {
                                    bytes.push(*b);
                                }
                            }
                        }
                    }
                }
            }
        }

        bytes
    }

    pub fn read<R: Read>(r: R) -> Result<HexFile,std::io::Error> {
        let file = BufReader::new(r);
        let mut lines = file.lines();

        assert_eq!(HexFile::header(), lines.next().unwrap().unwrap());

        let mut parsed = Vec::new();

        for line in lines {
            let line = line?;
            let line = line.trim();

            if line.starts_with("#") {
                parsed.push(HexFileLine::Comment(line[1..].to_string()));
                continue;
            }

            let mut data = Vec::new();

            for block in line.split_whitespace() {
                let mut tokens = block.split('*');
                let first = tokens.next().unwrap();
                let second = tokens.next();

                data.push(if let Some(value) = second {
                    HexFileData::Run(
                        u32::from_str_radix(first, 10).unwrap(),
                        u8::from_str_radix(value, 16).unwrap())
                } else {
                    HexFileData::Byte(u8::from_str_radix(first, 16).unwrap())
                });
            }

            parsed.push(HexFileLine::Data(data));
        }

        Ok(HexFile {
            lines: parsed,
        })
    }
}

