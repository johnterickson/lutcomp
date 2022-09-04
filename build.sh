set -e
cargo test --release

cargo run --release -- alu > circuit/alu.hex
cargo run --release -- ucode > circuit/ucode.hex

echo Run Digital tests
find circuit/*.dig
find circuit/*.dig -exec java -cp ../Digital/target/Digital.jar CLI test -verbose -circ {} \;

cargo run --release -- assemble programs/echo.asm > circuit/echo.asm.hex
cp circuit/echo.asm.hex circuit/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt

cargo run --release -- compile programs/echo.j > circuit/echo.j.hex
cp circuit/echo.j.hex circuit/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt

echo "6 7 * q" | cargo run --release -- compile programs/rpn.j --sim=true > circuit/rpn.hex

cp circuit/rpn.hex circuit/rom.hex
echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig

echo "Upload echo to RAM and run it"
cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > circuit/hello_ram.hex
cargo run --release -- program_ram --hex_path=./circuit/hello_ram.hex | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > circuit/bootram.hex

echo "Upload RPN to RAM and run it"
cargo run --release -- compile programs/rpn.j --image_base_address=080400 > circuit/rpn_ram.hex
(cargo run --release -- program_ram --hex_path=./circuit/rpn_ram.hex && echo "6 7 * q") | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > circuit/bootram.hex

echo "Upload echo to Digital RAM and run it"
cp circuit/bootram.hex circuit/rom.hex
(cargo run --release -- program_ram --hex_path=./circuit/hello_ram.hex) | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig

echo "run RPN in Digital ROM"
cp circuit/rpn.hex circuit/rom.hex
echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig