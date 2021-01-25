fn divide(x:u8, y:u8) -> u8 {
    q:u8 := 0;
    while (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    return q;
}

fn main(a:u8, b:u8) -> u8 {
    c:u8 := divide(a,b);
    return c;
}