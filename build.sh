set -e
cargo test --release

cargo run --release -- alu > logisim/alu.hex
cargo run --release -- ucode > logisim/ucode.hex
cargo run --release -- assemble programs/echo.asm > logisim/echo.hex

cargo run --release -- modify_circ --circuit=main --label=ALU\ LUT --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/alu.hex
cargo run --release -- modify_circ --circuit=main --label=uROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/ucode.hex
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/echo.hex

pushd logisim
./test.sh
popd

#cargo run --release -- compile programs/bootram.j > logisim/bootram.hex
#cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/bootram.hex

#cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > logisim/hello_ram.hex
#cargo run --release -- compile programs/rpn.j --image_base_address=081400 > logisim/rpn_ram.hex
