set -e
cargo test --release
cargo run --release -- alu > logisim/alu.hex
cargo run --release -- ucode > logisim/ucode.hex
cargo run --release -- assemble programs/echo.asm > logisim/echo.hex
cargo run --release -- compile programs/bootram.j > logisim/bootram.hex
cargo run --release -- compile programs/rpn.j --image_base_address=081400 > logisim/rpn_ram.hex