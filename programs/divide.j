FUNCTION divide(x:U8, y:U8) -> U8 {
    q:U8 := 0;
    WHILE (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    RETURN q;
}

FUNCTION main(a:U8, b:U8) -> U8 {
    CALL c:U8 := divide(a,b);
    RETURN c;
}