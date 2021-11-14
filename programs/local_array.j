fn main(x: u8, y: u8) -> u8 {
    a: u8[2];
    a[0x0] = x;
    a[0x1] = y;
    return (a[0x0] + a[0x1]);
}