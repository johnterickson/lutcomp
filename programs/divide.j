fn divide(x:U8, y:U8) -> U8 {
    q:U8 := 0;
    while (x >= y) {
        q := (q + 1);
        x := (x - y);
    }
    return q;
}

fn main(a:U8, b:U8) -> U8 {
    c:U8 := divide(a,b);
    return c;
}