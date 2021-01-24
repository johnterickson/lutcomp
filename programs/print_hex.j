FUNCTION divide(x:U8, y:U8) -> U8 {
    q:U8 := 0;
    WHILE (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    RETURN q;
}

FUNCTION printHexDigit(a:U8) {
    IF (a < 10) {
        TTYOUT (a + '0');
    } ELSE {
        a := (a - 10);
        a := (a + 'A');
        TTYOUT a;
    }
}

FUNCTION printHex(a:U8) {
    IF (a > 15) {
        CALL b := divide(a,16);
        CALL printHex(b);
        a := (a - (b*16));
    }

    CALL printHexDigit(a);
}

FUNCTION main(a:U8, b:U8) -> U8 {
    CALL printHex(a);
    TTYOUT 10;
    RETURN 0;
}