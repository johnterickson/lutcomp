set -e
cargo test --release
cargo run --release --bin alu > logisim/alu.hex
cargo run --release --bin ucode > logisim/ucode.hex
