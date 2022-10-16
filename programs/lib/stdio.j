
!include 'lcd.j'

fn [inline] getchar() -> u8 {
    while ((io_ready_to_read() & 1) == 0) {
    }
    return ttyin;
}

fn [inline] putc(c: u8) {
    io_write0(c);
    /* lcd_write_char(c); */
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
        putc(ch);
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