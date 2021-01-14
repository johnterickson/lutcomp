FUNCTION main(x:U8) {
    CALL res:U8 := fac(x);
    RETURN res;
}

FUNCTION fac(x:U8) {
    IF (x == 0) {
        RETURN 1;
    }
    ASSIGN product:U8 := x;
    WHILE (x > 2) {
        ASSIGN x:U8 := (x - 1);
        ASSIGN product:U8 := (product * x);
    }
    RETURN product;
}
