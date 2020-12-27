FUNCTION printHexDigit(a) {
    IF (a < 10) {
        TTYOUT (a + '0');
    } ELSE {
        ASSIGN a := (a - 10);
        ASSIGN a := (a + 'A');
        TTYOUT a;
    }
}

FUNCTION main(a,b,c) {
    CALL b := printHexDigit(a);
}

