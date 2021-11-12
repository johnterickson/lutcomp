!include 'divide.j'

fn printHexDigit(a:u8) {
    if (a < 10) {
        ttyout((a + '0'));
    } else {
        a := (a - 10);
        a := (a + 'A');
        ttyout(a);
    }
}

fn printHex(a:u8) {
    if (a > 15) {
        b := divide(a,16);
        printHexDigit(b);
        a := (a - (b*16));
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