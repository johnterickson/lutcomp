set -e
cargo test --release

cargo run --release -- alu > logisim/alu.hex
cargo run --release -- ucode > logisim/ucode.hex

echo Run Digital tests
find logisim/*.dig
find logisim/*.dig -exec java -cp ../Digital/target/Digital.jar CLI test -verbose -circ {} \;

cargo run --release -- assemble programs/echo.asm > logisim/echo.asm.hex
cp logisim/echo.asm.hex logisim/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./logisim/lutcomp.dig < logisim/test.txt

cargo run --release -- compile programs/echo.j > logisim/echo.j.hex
cp logisim/echo.j.hex logisim/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./logisim/lutcomp.dig < logisim/test.txt

echo "6 7 * q" | cargo run --release -- compile programs/rpn.j --sim=true > logisim/rpn.hex

cp logisim/rpn.hex logisim/rom.hex
echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./logisim/lutcomp.dig

echo "Upload echo to RAM and run it"
cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > logisim/hello_ram.hex
cargo run --release -- program_ram --hex_path=./logisim/hello_ram.hex | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > logisim/bootram.hex

echo "Upload RPN to RAM and run it"
cargo run --release -- compile programs/rpn.j --image_base_address=080400 > logisim/rpn_ram.hex
(cargo run --release -- program_ram --hex_path=./logisim/rpn_ram.hex && echo "6 7 * q") | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > logisim/bootram.hex

echo "Upload echo to Digital RAM and run it"
cp logisim/bootram.hex logisim/rom.hex
(cargo run --release -- program_ram --hex_path=./logisim/hello_ram.hex) | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./logisim/lutcomp.dig

echo "run RPN in Digital ROM"
cp logisim/rpn.hex logisim/rom.hex
echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./logisim/lutcomp.dig