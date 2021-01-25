fn main(x:U8) -> U8 {
    res:U8 := fac(x);
    return res;
}

fn fac(x:U8) -> U8 {
    if (x == 0) {
        return 1;
    }
    product:U8 := x;
    while (x > 2) {
        x := (x - 1);
        product := (product * x);
    }
    return product;
}
