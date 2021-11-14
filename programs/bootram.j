fn getchar() -> u8 {
    tty: u8 := 0;
    while ((tty & 128) == 0) {
        tty := ttyin;
    }
    return (tty & 127);
}

!include 'print_hex.j'

fn main() -> u8 {
    addr: u8[4];
    i: u8;
    ch: u8;
    buf: u8[16];

    addr[3] := 0;
    addr[2] := 3;
    addr[1] := 2;
    addr[0] := 1;

    addr_usize: &usize := ((&addr) AS &usize);
    p_addr: &u8;

    while (0 == 0) {
        /* ttyout('#'); */
        
        ch := getchar();
        while (ch == 13) {
            ch := getchar();
        }
        buf[0] := ch;
        i := 1;

        while (ch != 0) {
            ch := getchar();
            while (ch == 13) {
                ch := getchar();
            }
            if (ch == 10) {
                ch := 0;
            }
            buf[i] := ch;
            i := (i + 1);
        }

        if (buf[0] == 'a') {
            printHex(addr[3]);
            printHex(addr[2]);
            printHex(addr[1]);
            printHex(addr[0]);
            ttyout(10);
        }

        if (buf[0] == 's') {
            i := 1;
            addr[0] := 0;
            addr[1] := 0;
            addr[2] := 0;
            while (buf[i] != 0) {
                addr[2] := addr[1];
                addr[1] := addr[0];
                addr[0] := parseHex(&buf[i]);
                i := (i + 2);
            }
        }

        if (buf[0] == 'r') {
            p_addr := ((*addr_usize) AS &u8);
            printHex(*p_addr);
            ttyout(10);
        }

        if (buf[0] == 'w') {
            p_addr := ((*addr_usize) AS &u8);
            *p_addr := parseHex(&buf[1]);
            *addr_usize := (*addr_usize + 1);
        }

        if (buf[0] == 'q') {
            return 0;
        }
    }
}