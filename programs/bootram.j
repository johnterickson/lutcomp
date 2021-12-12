!include 'echoline.j'
!include 'print_hex.j'

fn main() -> u8 {
    addr: usize = 0x0;
    i: usize = 0x0;
    ch: u8 = 0;
    buf: u8[20];

    ttyout('R');
    ttyout('E');
    ttyout('A');
    ttyout('D');
    ttyout('Y');
    ttyout(10);

    p_byte: &u8 = ((0x0) AS &u8);

    while (0 == 0) {
        /* ttyout('#'); */
        readline(&(buf[0]));
        ch = buf[0x0];

        if (ch == 'r') {
            p_byte = ((addr) AS &u8);
            printHex(*p_byte);
            ttyout(10);
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
            ttyout(10);
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
            ttyout('B');
            ttyout('o');
            ttyout('o');
            ttyout('t');
            ttyout('r');
            ttyout('a');
            ttyout('m');
            ttyout(10);
        }
        else {
            ttyout('H');
            ttyout('u');
            ttyout('h');
            ttyout('?');
            ttyout(ch);
            ttyout(10);
        }
    }

    return 0;
}