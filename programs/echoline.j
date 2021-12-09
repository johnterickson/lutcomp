
fn readline(buf:&u8) {
    ch: u8 = 0;
    while (0 == 0) {
        tty: u8 = 0;
        while ((tty & 128) == 0) {
            tty = ttyin;
        }
        ch = (tty & 127);
        if (ch == 10) {
            *buf = 0;
            return;
        }
        *buf = ch;
        buf = (buf + 0x1);
    }
}

fn println(buf: &u8) {
    ch: u8 = *buf;
    while (ch != 0) {
        ttyout(ch);
        buf = (buf + 0x1);
        ch = *buf;
    }
}

fn test_echoline() -> u8 {
    buf: u8[20];
    readline(&(buf[0]));
    println(&(buf[0]));
    return 0;
}