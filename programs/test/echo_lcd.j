!include 'lcd.j'

fn main() -> u8 {
    lcd_init();

    ch: u8 = 'a';
    while (ch <= 'z') {
        lcd_putc(ch);
        ch = (ch + 1);
    }

    ch = 'A';
    while (ch <= 'Z') {
        lcd_putc(ch);
        ch = (ch + 1);
    }
}