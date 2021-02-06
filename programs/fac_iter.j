fn fac(x:u8) -> u8 {
    if (x == 0) {
        return 1;
    }
    product:u8 := x;
    while (x > 2) {
        x := (x - 1);
        product := (product * x);
    }
    return product;
}
