set +e
cargo run -- ucode > ucode.hex
cargo run -- assemble tests/hi.asm tests/hi.hex
cargo run -- assemble tests/16add.asm tests/16add.hex
cargo run -- assemble tests/echo.asm tests/echo.hex
