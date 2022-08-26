
fn [inline] getchar() -> u8 {
    tty: u8 = 0;
    tty = ttyin;
    while ((tty & 128) == 0) {
        tty = ttyin;
    }
    return (tty & 127);
}

fn [inline] readline(buf:&u8) {
    while (0 == 0) {
        ch: u8 = getchar();
        if (ch == 13) {

        } else if (ch == 10) {
            *buf = 0;
            return;
        } else {
            *buf = ch;
            buf = &(buf[0x1]);
        }
    }
}

fn [inline] println(buf: &u8) {
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