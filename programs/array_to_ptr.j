fn add(a: &U8) -> U8 {
    return (a[0x0] + a[0x1]);
}

fn main(x: U8, y: U8) -> U8 {
    a: U8[2];
    a[0x0] := x;
    a[0x1] := y;
    sum := add(&a[0x0]);
    return sum;
}