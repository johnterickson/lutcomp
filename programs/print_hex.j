fn divide(x:u8, y:u8) -> u8 {
    q:u8 := 0;
    while (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    return q;
}

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
        printHex(b);
        a := (a - (b*16));
    }

    printHexDigit(a);
}

fn main(a:u8, b:u8) -> u8 {
    printHex(a);
    ttyout(10);
    return 0;
}