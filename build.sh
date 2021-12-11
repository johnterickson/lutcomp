set -e
cargo test --release
cargo run --release -- alu > logisim/alu.hex
cargo run --release -- ucode > logisim/ucode.hex
cargo run --release -- assemble programs/echo.asm > logisim/echo.hex