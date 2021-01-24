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