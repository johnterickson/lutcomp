set -e
cargo test --release

cargo run --release -- alu > logisim/alu.hex
cargo run --release -- modify_circ --circuit=main --label=ALU_LUT --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/alu.hex

cargo run --release -- ucode > logisim/ucode.hex
cargo run --release -- modify_circ --circuit=main --label=uROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/ucode.hex

cargo run --release -- assemble programs/echo.asm > logisim/echo.asm.hex
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/echo.asm.hex
java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt < logisim/test.txt

cargo run --release -- compile programs/echo.j > logisim/echo.j.hex
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/echo.j.hex
java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt < logisim/test.txt

echo "6 7 * q" | cargo run --release -- compile programs/rpn.j --sim=true > logisim/rpn.hex

echo "Upload echo to RAM and run it"
cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > logisim/hello_ram.hex
cargo run --release -- program_ram --hex_path=./logisim/hello_ram.hex | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > logisim/bootram.hex

echo "Upload RPN to RAM and run it"
cargo run --release -- compile programs/rpn.j --image_base_address=080400 > logisim/rpn_ram.hex
(sleep 10 && cargo run --release -- program_ram --hex_path=./logisim/rpn_ram.hex && echo "6 7 * q") | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > logisim/bootram.hex

# echo "Upload echo to logisim RAM"
# cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/bootram.hex
# echo "and run it"
# (sleep 10 && cargo run --release -- program_ram --hex_path=./logisim/hello_ram.hex) | java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt

echo "run RPN in logisim ROM"
cargo run --release -- modify_circ --circuit=main --label=ROM --circ_path=./logisim/lutcomp.circ --hex_path=./logisim/rpn.hex
echo "6 7 * q" | java -jar ./logisim/Logisim-Ita/Compiled/Logisim-ITA.jar logisim/lutcomp.circ -tty tty,speed,halt