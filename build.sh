set -e
cargo test --release

cargo run --release -- alu > circuit/alu.hex
cargo run --release -- ucode > circuit/ucode.hex
cargo run --release -- ascii_to_ps2 > circuit/ascii_to_ps2.hex

cargo run --release -- assemble programs/echo.asm > circuit/echo.asm.hex
cargo run --release -- compile programs/echo.j > circuit/echo.j.hex

echo Run Digital tests
cp circuit/echo.j.hex circuit/rom.hex
find circuit/*.dig
find circuit/*.dig -exec java -cp ../Digital/target/Digital.jar CLI test -verbose -circ {} \;

cp circuit/echo.asm.hex circuit/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt

cp circuit/echo.j.hex circuit/rom.hex
java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt

echo "run RPN in simulator"
echo "6 7 * q" | cargo run --release -- compile programs/rpn.j --sim=true > circuit/rpn.hex 2> /tmp/err.log

echo "run RPN in Digital ROM"
cp circuit/rpn.hex circuit/rom.hex
output=$(echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig)
expected="$(printf 'RPN\n42')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi


echo "Upload echo to RAM and run it"
cargo run --release -- compile programs/hello_ram.j --image_base_address=080400 > circuit/hello_ram.hex
cargo run --release -- program_ram --hex_path=./circuit/hello_ram.hex | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > circuit/bootram.hex

echo "Upload RPN to RAM and run it"
cargo run --release -- compile programs/rpn.j --image_base_address=080400 > circuit/rpn_ram.hex
(cargo run --release -- program_ram --hex_path=./circuit/rpn_ram.hex && echo "6 7 * q") | cargo run --release -- compile programs/bootram.j --sim=true --block_for_stdin=true > circuit/bootram.hex

echo "Upload echo to Digital RAM and run it"
cp circuit/bootram.hex circuit/rom.hex
(cargo run --release -- program_ram --hex_path=./circuit/hello_ram.hex) | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig
