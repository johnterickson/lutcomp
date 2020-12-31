FUNCTION divide(x,y) {
    ASSIGN q := 0;
    WHILE (x > y) {
        ASSIGN q := (q + 1);
        ASSIGN x := (x - y);
    }
    RETURN q;
}

FUNCTION main(a,b,c) {
    CALL c := divide(a,b);
    RETURN c;
}