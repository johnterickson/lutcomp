fn getchar() -> u8 {
    tty: u8 := 0;
    while ((tty & 128) == 0) {
        tty := ttyin;
    }
    return (tty & 127);
}

fn main() -> u8 {
    ttyout('H');
    ttyout('i');
    ttyout('!');
    ttyout(10);
    ch: u8 := 0;
    while (0 == 0) {
        ttyout(':');
        ttyout('>');
        ch := 0;
        while (ch != 10) {
            ch := getchar();
            ttyout(ch);

            if (ch == 'q') {
                return 0;
            }
        }
    }
    return 0;
}