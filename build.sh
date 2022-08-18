set -e
cargo test --release

cargo run --release -- alu > logisim/alu.hex
cargo run --release -- modify_circ --circuit=main --label=ALU_LUT --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/alu.hex

cargo run --release -- ucode > logisim/ucode.hex
cargo run --release -- modify_circ --circuit=main --label=uROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/ucode.hex

cargo run --release -- assemble programs/echo.asm > logisim/echo.asm.hex
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/echo.asm.hex
cat logisim/test.txt | java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt

cargo run --release -- compile programs/echo.j > logisim/echo.j.hex
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/echo.j.hex
cat logisim/test.txt | java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt

echo "6 7 * q" | cargo run --release -- compile programs/rpn.j --sim=true > logisim/rpn.hex

# RAM stuff
cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > logisim/hello_ram.hex
(cargo run --release -- program_ram --hex_path=./logisim/hello_ram.hex | cargo run --release -- compile programs/bootram.j --sim=true) > logisim/bootram.hex

# in circuit RAM
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/bootram.hex

#slow stuff
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/rpn.hex
echo "6 7 * q" | java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt