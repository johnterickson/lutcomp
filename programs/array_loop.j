fn add(a: &U8, n: UPTR) -> U8 {
    i: UPTR := 0x0;
    sum: U8 := 0;
    while (i != n) {
        sum := (sum + a[i]);
        i := (i + 1);
    }
    return sum;
}

fn main(x: U8, y: U8) -> U8 {
    a: U8[2];
    a[0x0] := x;
    a[0x1] := y;
    sum := add(&a[0x0], 0x2);
    return sum;
}