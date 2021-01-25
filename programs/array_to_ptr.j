fn add(a: &u8) -> u8 {
    return (a[0x0] + a[0x1]);
}

fn main(x: u8, y: u8) -> u8 {
    a: u8[2];
    a[0x0] := x;
    a[0x1] := y;
    sum := add(&a[0x0]);
    return sum;
}