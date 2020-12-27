FUNCTION divide(x,y) {
    IF (x < y) {
        RETURN 0;
    } ELSE {
        CALL z := divide((x-y),y);
        RETURN (1 + z);
    }
}

FUNCTION printHexDigit(a) {
    IF (a < 10) {
        TTYOUT (a + '0');
    } ELSE {
        ASSIGN a := (a - 10);
        ASSIGN a := (a + 'A');
        TTYOUT a;
    }
}

FUNCTION printHex(a) {
    IF (a > 15) {
        CALL b := divide(a,16);
        CALL c := printHex(b);
        ASSIGN a := (a - (b*16));
    }

    CALL b := printHexDigit(a);
}

FUNCTION main(a,b,c) {
    CALL b := printHex(a);
    TTYOUT 10;
}

