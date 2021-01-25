fn main() -> U8 {
    res:U8 := plusone(6);
    return res;
}

fn plusone(x:U8) -> U8 {
    sum:U8 := (x + 1);
    return sum;
}
