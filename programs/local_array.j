/*
FUNCTION add(a: &U8, n: UPTR) -> U8 {
    ASSIGN i: UPTR := 0x0;
    ASSIGN sum: U8 := 0;
    WHILE (i != n) {
        ASSIGN sum := (sum + a[i]);
        ASSIGN i := (i + 1);
    }
    RETURN sum;
}

FUNCTION main(x: U8, y: U8) -> U8 {
    DECLARE a: U8[2];
    ASSIGN a[0x0] := x;
    ASSIGN a[0x1] := y;
    CALL sum := add(&a[0x0], 0x2);
    RETURN sum;
}
*/

/*
FUNCTION add(a: &U8) -> U8 {
    RETURN (a[0x0] + a[0x1]);
}

FUNCTION main(x: U8, y: U8) -> U8 {
    DECLARE a: U8[2];
    ASSIGN a[0x0] := x;
    ASSIGN a[0x1] := y;
    CALL sum := add(&a[0x0]);
    RETURN sum;
}
*/

FUNCTION main(x: U8, y: U8) -> U8 {
    DECLARE a: U8[2];
    ASSIGN a[0x0] := x;
    ASSIGN a[0x1] := y;
    RETURN (a[0x0] + a[0x1]);
}