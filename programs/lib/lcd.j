!include 'delay.j'

static lcd_display: u8[80];
static lcd_current: u8;

const lcd_row_start: u8[4] = [0, 64, 20, 84];

fn lcd_write_char(c: u8) {
    io_write1((128 | (c >> 4)));
    io_write1((128 | (c & 15)));
    delay(0x02);
}

fn lcd_set_pos(pos: u8) {
    io_write1((8 | (pos >> 4)));
    io_write1((pos & 15));
    delay(0x02);
}

fn lcd_init() {
    i: u8 = 0;
    lcd_current = 0;
    while (i < 80) {
        lcd_display[i] = 0;
        i = (i + 1);
    }

    io_write1(3);
    delay(0x01);
    io_write1(3);
    delay(0x01);
    io_write1(3);
    delay(0x01);

    io_write1(2);
    delay(0x02);

    io_write1(2);
    io_write1(8);
    delay(0x02);

    io_write1(0);
    io_write1(12);
    delay(0x02);
    
    io_write1(0);
    io_write1(1);
    delay(0x02);
}
