!include 'lcd.j'

fn [inline] delay(n: usize) {
    while (n != 0x0) {
        n = (n-0x1);
    }
}

fn main() -> u8 {
    io_write0('h');
    io_write0('i');
    io_write1(3);
    delay(0x10);

    io_write1(3);
    delay(0x10);

    io_write1(2);
    delay(0x1);

    io_write1(2);
    io_write1(8);
    delay(0x1);

    io_write1(0);
    io_write1(12);
    delay(0x1);

    io_write1(0);
    io_write1(1);
    delay(0x10);

    io_write1(0);
    io_write1(6);
    delay(0x1);

    ch: u8 = 'a';
    while (ch <= 'z') {
        lcd_write_char(ch);
        ch = (ch + 1);
    }

    ch = 'A';
    while (ch <= 'Z') {
        lcd_write_char(ch);
        ch = (ch + 1);
    }

    lcd_flush();
}