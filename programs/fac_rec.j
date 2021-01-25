fn main(x:U8) -> U8 {
    res:U8 := fac(x);
    return res;
}

fn fac(x:U8) -> U8 {
    if (x != 0) {
        prev:U8 := fac((x-1));
        return (prev * x);
    } else {
        return 1;
    }
}
