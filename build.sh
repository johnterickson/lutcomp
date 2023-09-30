set -e -o pipefail
#set -x

cargo run --release -- alu > circuit/alu.hex
cargo run --release -- ucode > circuit/ucode.hex
cargo run --release -- ascii_to_ps2 > circuit/ascii_to_ps2.hex
cargo run --release -- ps2_to_ascii > circuit/ps2_to_ascii.hex
cargo run --release -- ps2_to_ascii --style=j > programs/lib/ps2.j
cargo run --release -- cgrom > circuit/cgrom.hex

cargo test --release

cargo run --release -- assemble programs/test/echo.asm > circuit/echo.asm.hex
cargo run --release -- compile programs/test/echo.j > circuit/echo.j.hex
cargo run --release -- compile programs/test/keyboard_isr.j > circuit/keyboard_isr.hex
cargo run --release -- compile programs/test/keyboard_poll.j --image_base_address=90000 > circuit/keyboard_poll.ram.hex
cargo run --release -- compile programs/test/echo_lcd.j > circuit/echo_lcd.hex
cargo run --release -- compile programs/app/rpn.j >circuit/rpn.hex
cargo run --release -- compile programs/app/bootram.j >circuit/bootram.hex

echo "Build Digital fork"
pushd deps/Digital
mvn package -DskipTests > /dev/null
popd

echo Run Digital tests
cp circuit/echo.j.hex circuit/rom.hex
proc_count=$(cat /proc/cpuinfo | grep '^processor\s' | wc -l)
test_cmd="java -cp deps/Digital/target/Digital.jar CLI test -verbose -circ"
(find circuit/*.dig | xargs -P $proc_count -I % $test_cmd %) || \
  (find circuit/*.dig | xargs --verbose -P 1 -I % $test_cmd %)

echo "Build unit tester"
pushd digital_tester
mvn compile  > /dev/null
popd

if [[ -z "${WINDIR}" ]]; then
  class_path_separator=":"
else
  class_path_separator=";"
fi

RunTest="java -cp \"digital_tester/target/classes$(echo $class_path_separator)deps/Digital/target/Digital.jar\" com.johnterickson.App circuit/lutcomp.dig"

echo "run echo assembly"
cp circuit/echo.asm.hex circuit/rom.hex
eval "$RunTest \"expected=$(printf 'Hi!\n>:Yo!\n>:q')\" \"input=$(printf 'Yo!\nq\n')\""

echo "run echo compiled"
cp circuit/echo.j.hex circuit/rom.hex
eval "$RunTest \"expected=$(printf 'Hi!\n>:Yo!\n>:q')\" \"input=$(printf 'Yo!\nq\n')\""

echo "run RPN in simulator"
output=$(echo "6 7 + 3 - 10 * 4 / 6 d q" | cargo run -q --release -- compile programs/app/rpn.j --sim=true --block_for_stdin=true 2>&1 1>circuit/rpn.hex)
expected="$(printf 'RPN\n13\n10\n100\n25\n0:25\n1:6')"
if [ "'$expected'" != "'$output'" ]; then
    echo "'$output' != '$expected'"
    exit 1
fi

echo "run RPN in Digital ROM"
cp circuit/rpn.hex circuit/rom.hex
eval "$RunTest \"expected=$(printf 'RPN\n42\n')\" \"input=6 7 * q\""

echo "test ttyin interrupts"
cp circuit/keyboard_isr.hex circuit/rom.hex
eval "$RunTest keyboard=TTYIN expected=abc input=abcq"

echo "test PS2 interrupts"
cp circuit/keyboard_isr.hex circuit/rom.hex
eval "$RunTest keyboard=PS2-Keyboard expected=abc input=abcq"

echo "Upload echo to RAM and run it"
cargo run -q --release -- compile programs/test/hello_ram.j --image_base_address=080400 > circuit/hello_ram.hex
output=$(cargo run -q --release -- program_ram --hex_path=./circuit/hello_ram.hex | cargo run -q --release -- compile programs/app/bootram.j --sim=true --block_for_stdin=true --profile=true 2>&1 1>circuit/bootram.hex)
expected="$(printf 'READY\nHi_from_RAM!')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi

echo "Upload RPN to RAM and run it"
cargo run -q --release -- compile programs/app/rpn.j --image_base_address=080400 > circuit/rpn_ram.hex
output=$((cargo run -q --release -- program_ram --hex_path=./circuit/rpn_ram.hex && echo "6 7 * q") | cargo run -q --release -- compile programs/app/bootram.j --sim=true --block_for_stdin=true 2>&1 1>circuit/bootram.hex)
expected="$(printf 'READY\nRPN\n42')"
if [ "$expected" != "$output" ]; then
    echo "$output != $expected"
    exit 1
fi

echo "Upload echo to Digital RAM and run it"
cp circuit/bootram.hex circuit/rom.hex
(cargo run -q --release -- program_ram --hex_path=./circuit/hello_ram.hex) | eval "$RunTest \"expected=$(printf 'READY\nHi_from_RAM!')\""

echo "copying rpn to ROM for tinkering"
cp circuit/rpn.hex circuit/rom.hex
