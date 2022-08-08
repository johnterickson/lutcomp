fn main() -> u8 {
    res:u8 = plusone(6);
    return res;
}

fn [inline] plusone(x:u8) -> u8 {
    sum:u8 = (x + 1);
    return sum;
}
