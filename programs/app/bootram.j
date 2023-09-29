!include 'stdio.j'
!include 'print_hex.j'
!include 'rpnlib.j'

fn main() -> u8 {
    addr: usize = 0x0;
    i: usize = 0x0;
    ch: u8 = 0;
    buf: u8[20];

    stdio_init();

    putc('R');
    putc('E');
    putc('A');
    putc('D');
    putc('Y');
    putc(10);

    lcd_enable = 0;

    p_byte: &u8 = ((0x0) AS &u8);

    while (0 == 0) {
        readline(&(buf[0]));
        ch = buf[0x0];

        if (ch == 'g') {
            static calc: RpnCalc;

            lcd_enable = 1;
            stdio_init();
            putc('R');
            putc('P');
            putc('N');
            putc(10);
            RpnCalc_init(&calc);
            RpnCalc_run(&calc);
            lcd_enable = 0;
        }
        else if (ch == 'r') {
            p_byte = ((addr) AS &u8);
            printHex(*p_byte);
            putc(10);
        }
        else if (ch == 'w') {
            p_byte = ((addr) AS &u8);
            *p_byte = parseHex(&buf[0x1]);
        }
        else if (ch == 'n') {
            addr = (addr + 1);
        }
        else if (ch == 'a') {
            printHex(addr[0x3]);
            printHex(addr[0x2]);
            printHex(addr[0x1]);
            printHex(addr[0x0]);
            putc(10);
        }
        else if (ch == 's') {
            i = 0x1;
            addr = 0x0;
            while (buf[i] != 0) {
                addr[0x3] = addr[0x2];
                addr[0x2] = addr[0x1];
                addr[0x1] = addr[0x0];
                addr[0x0] = parseHex(&(buf[i]));
                i = (i + 0x2);
            }
        }
        else if (ch == 'q') {
            return 0;
        }
        else if (ch == 'i') {
            lcd_enable = 1;
            putc('B');
            putc('o');
            putc('o');
            putc('t');
            putc('r');
            putc('a');
            putc('m');
            putc(10);
            lcd_enable = 0;
        }
        else {
            putc('H');
            putc('u');
            putc('h');
            putc('?');
            putc(ch);
            putc(10);
        }
    }

    return 0;
}