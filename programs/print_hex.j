fn printHexDigit(a:u8) {
    if (a < 10) {
        ttyout((a + '0'));
    } else {
        a = (a - 10);
        a = (a + 'A');
        ttyout(a);
    }
}

fn printHex(a:u8) {
    if (a > 15) {
        b: u8 = (a / 16);
        printHexDigit(b);
        a = (a - (b*16));
    } else {
        ttyout('0');
    }

    printHexDigit(a);
}

fn printHexTest(a:u8, b:u8) -> u8 {
    printHex(a);
    ttyout(10);
    return 0;
}

fn parseHexNibble(ch: char) -> u8 {
    if (ch >= 'a') {
        return ((ch - 'a') + 10);
    }
    if (ch <= '9') {
        return (ch - '0');
    }

    return ((ch -'A') + 10);
}

fn parseHex(c: &char) -> u8 {
    h: u8 = (16 * (parseHexNibble(*c)));
    c = &c[0x1];
    h = (h + parseHexNibble(*c));
    return h;
}