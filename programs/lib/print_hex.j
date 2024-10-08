!include 'stdio.j'

fn [inline] printHexDigit(a:u8) {
    if (a < 10) {
        putc((a + '0'));
    } else {
        a = (a - 10);
        a = (a + 'A');
        putc(a);
    }
}

fn [inline] printHex(a:u8) {
    if (a > 15) {
        b: u8 = (a / 16);
        printHexDigit(b);
        a = (a - (b*16));
    } else {
        putc('0');
    }

    printHexDigit(a);
}

fn printHexTest(a:u8, b:u8) -> u8 {
    printHex(a);
    putc(10);
    return 0;
}

fn [inline] parseHexNibble(ch: char) -> u8 {
    if (ch >= 'a') {
        return ((ch - 'a') + 10);
    }
    if (ch <= '9') {
        return (ch - '0');
    }

    return ((ch -'A') + 10);
}

fn [inline] parseHex(c: &char) -> u8 {
    h: u8 = (16 * (parseHexNibble(*c)));
    c = &c[0x1];
    h = (h + parseHexNibble(*c));
    return h;
}