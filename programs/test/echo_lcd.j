!include 'lcd.j'

fn main() -> u8 {
    io_write1(3);

    io_write1(2);
    io_write1(8);

    io_write1(2);
    io_write1(8);

    io_write1(0);
    io_write1(1);

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