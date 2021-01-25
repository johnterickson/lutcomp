fn divide(x:U8, y:U8) -> U8 {
    q:U8 := 0;
    while (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    return q;
}

fn printHexDigit(a:U8) {
    if (a < 10) {
        ttyout((a + '0'));
    } else {
        a := (a - 10);
        a := (a + 'A');
        ttyout(a);
    }
}

fn printHex(a:U8) {
    if (a > 15) {
        b := divide(a,16);
        printHex(b);
        a := (a - (b*16));
    }

    printHexDigit(a);
}

fn main(a:U8, b:U8) -> U8 {
    printHex(a);
    ttyout(10);
    return 0;
}