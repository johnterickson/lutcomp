FUNCTION divide(x:U8, y:U8) -> U8 {
    ASSIGN q:U8 := 0;
    WHILE (x @gte y) {
        ASSIGN q := (q + 1);
        ASSIGN x := (x - y);
    }
    RETURN q;
}

FUNCTION printHexDigit(a:U8) {
    IF (a < 10) {
        TTYOUT (a + '0');
    } ELSE {
        ASSIGN a := (a - 10);
        ASSIGN a := (a + 'A');
        TTYOUT a;
    }
}

FUNCTION printHex(a:U8) {
    IF (a > 15) {
        CALL b := divide(a,16);
        CALL printHex(b);
        ASSIGN a := (a - (b*16));
    }

    CALL printHexDigit(a);
}

FUNCTION main(a:U8, b:U8) -> U8 {
    CALL printHex(a);
    TTYOUT 10;
    RETURN 0;
}