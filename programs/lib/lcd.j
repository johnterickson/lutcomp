!include 'delay.j'

static lcd_display: u8[80];
static lcd_current: u8;

const lcd_row_start: u8[4] = [0, 64, 20, 84];

fn [inline] lcd_logical_to_physical(i: u8) -> u8 {
    row: u8 = (i / 20);
    col: u8 = (i - (row * 20));
    return (lcd_row_start[row] + col);
}

fn lcd_draw_char(i:u8, ch:u8) {
    pos: u8 = lcd_logical_to_physical(i);

    io_write1((8 | (pos >> 4)));
    io_write1((pos & 15));
    delay(0x02);

    io_write1((128 | (ch >> 4)));
    io_write1((128 | (ch & 15)));
    delay(0x02);
}

fn lcd_scroll_up() {
    lcd_current = (lcd_current - 20);

    i: u8 = 0;
    ch: u8;
    while (i < 60) {
        ch = lcd_display[(i + 20)];
        lcd_display[i] = ch;
        lcd_draw_char(i, ch);
        i = (i + 1);
    }

    # i is 60
    while (i < 80) {
        lcd_display[i] = 32;
        lcd_draw_char(i, 32);
        i = (i + 1);
    }
}

fn lcd_putc(ch: u8) {
    if (ch == 10) {
        next: u8 = (((lcd_current / 20) + 1) * 20);
        while (lcd_current < next) {
            lcd_display[lcd_current] = 32;
            lcd_current = (lcd_current + 1);
        }
    } else {
        lcd_display[lcd_current] = ch;
        lcd_draw_char(lcd_current, ch);       
        lcd_current = (lcd_current + 1);
    }
    
    if (lcd_current >= 80) {
        lcd_scroll_up();
    }
}

fn lcd_init() {
    lcd_current = 0;

    # fill with spaces
    i: u8 = 0;
    while (i < 80) {
        lcd_display[i] = 32;
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
