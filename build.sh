set -e
cargo test --release
cargo run --release --bin alu > logisim/alu.hex
cargo run --release --bin ucode > logisim/ucode.hex
cargo run --release --bin assemble < programs/echo.asm > logisim/echo.hex