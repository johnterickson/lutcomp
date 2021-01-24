FUNCTION main(x:U8) -> U8 {
    CALL res:U8 := fac(x);
    RETURN res;
}

FUNCTION fac(x:U8) -> U8 {
    IF (x == 0) {
        RETURN 1;
    }
    product:U8 := x;
    WHILE (x > 2) {
        x := (x - 1);
        product := (product * x);
    }
    RETURN product;
}
