fn [inline] lcd_flush() {
    while ((io_read1() & 8) != 0) {
    }
}

fn [inline] lcd_write_char(c: u8) {
    lcd_flush();
    io_write1((128 | (c >> 4)));
    io_write1((128 | (c & 15)));
}

fn lcd_init() {
    io_write1(3);

    io_write1(2);
    io_write1(8);

    io_write1(2);
    io_write1(8);

    io_write1(0);
    io_write1(1);
}
