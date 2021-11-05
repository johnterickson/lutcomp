fn main() -> u8 {
    ch: u8 := 0;
    while (ch != 10) {
        tty: u8 := 0;
        while ((tty & 128) == 0) {
            tty := ttyin;
        }
        ch := (tty & 127);
        ttyout(ch);
    }
    return 0;
}