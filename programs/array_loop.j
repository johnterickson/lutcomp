FUNCTION add(a: &U8, n: UPTR) -> U8 {
    i: UPTR := 0x0;
    sum: U8 := 0;
    WHILE (i != n) {
        sum := (sum + a[i]);
        i := (i + 1);
    }
    RETURN sum;
}

FUNCTION main(x: U8, y: U8) -> U8 {
    a: U8[2];
    a[0x0] := x;
    a[0x1] := y;
    CALL sum := add(&a[0x0], 0x2);
    RETURN sum;
}