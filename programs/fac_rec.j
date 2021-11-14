fn main(x:u8) -> u8 {
    res:u8 = fac(x);
    return res;
}

fn fac(x:u8) -> u8 {
    if (x != 0) {
        prev:u8 = fac((x-1));
        return (prev * x);
    } else {
        return 1;
    }
}
