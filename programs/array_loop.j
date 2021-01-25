fn add(a: &u8, n: usize) -> u8 {
    i: usize := 0x0;
    sum: u8 := 0;
    while (i != n) {
        sum := (sum + a[i]);
        i := (i + 1);
    }
    return sum;
}

fn main(x: u8, y: u8) -> u8 {
    a: u8[2];
    a[0x0] := x;
    a[0x1] := y;
    sum := add(&a[0x0], 0x2);
    return sum;
}