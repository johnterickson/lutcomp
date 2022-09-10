set -e -o pipefail
#set -x

cargo test --release

cargo run --release -- alu > circuit/alu.hex
cargo run --release -- ucode > circuit/ucode.hex
cargo run --release -- ascii_to_ps2 > circuit/ascii_to_ps2.hex
cargo run --release -- ps2_to_ascii > circuit/ps2_to_ascii.hex

cargo run --release -- assemble programs/echo.asm > circuit/echo.asm.hex
cargo run --release -- compile programs/echo.j > circuit/echo.j.hex

echo Run Digital tests
cp circuit/echo.j.hex circuit/rom.hex
find circuit/*.dig
find circuit/*.dig -exec java -cp ../Digital/target/Digital.jar CLI test -verbose -circ {} \;

echo "run echo assembly"
cp circuit/echo.asm.hex circuit/rom.hex
output=$(java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt)
expected="$(printf 'Hi!\n>:Yo!\n>:q')"
if [ "'$expected'" != "'$output'" ]; then
    echo "'$output' != '$expected'"
    exit 1
fi

echo "run echo compiled"
cp circuit/echo.j.hex circuit/rom.hex
output=$(java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig < circuit/test.txt)
if [ "'$expected'" != "'$output'" ]; then
    echo "'$output' != '$expected'"
    exit 1
fi

echo "run RPN in simulator"
output=$(echo "6 7 * q" | cargo run --release --quiet -- compile programs/rpn.j --sim=true 2>&1 1>circuit/rpn.hex)
expected="$(printf 'RPN\n42')"
if [ "'$expected'" != "'$output'" ]; then
    echo "'$output' != '$expected'"
    exit 1
fi

echo "run RPN in Digital ROM"
cp circuit/rpn.hex circuit/rom.hex
output=$(echo "6 7 * q" | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig)
expected="$(printf 'RPN\n42')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi

echo "Upload echo to RAM and run it"
cargo run -q --release -- compile programs/hello_ram.j --image_base_address=080400 > circuit/hello_ram.hex
output=$(cargo run -q --release -- program_ram --hex_path=./circuit/hello_ram.hex | cargo run -q --release -- compile programs/bootram.j --sim=true --block_for_stdin=true 2>&1 1>circuit/bootram.hex)
expected="$(printf 'READY\nHi_from_RAM!')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi

echo "Upload RPN to RAM and run it"
cargo run -q --release -- compile programs/rpn.j --image_base_address=080400 > circuit/rpn_ram.hex
output=$((cargo run -q --release -- program_ram --hex_path=./circuit/rpn_ram.hex && echo "6 7 * q") | cargo run -q --release -- compile programs/bootram.j --sim=true --block_for_stdin=true 2>&1 1>circuit/bootram.hex)
expected="$(printf 'READY\nRPN\n42')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi

echo "Upload echo to Digital RAM and run it"
cp circuit/bootram.hex circuit/rom.hex
output=$((cargo run -q --release -- program_ram --hex_path=./circuit/hello_ram.hex) | java -classpath ./digitalTester/consoleTester/bin:../Digital/target/Digital.jar consoleTester.ConsoleTester ./circuit/lutcomp.dig)
expected="$(printf 'READY\nHi_from_RAM!')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi