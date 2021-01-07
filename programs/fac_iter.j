FUNCTION main() {
    CALL res:U8 := fac(5);
    RETURN res;
}

FUNCTION fac(x:U8) {
    ASSIGN product:U8 := x;
    WHILE (x > 2) {
        ASSIGN x:U8 := (x - 1);
        ASSIGN product:U8 := (product * x);
    }
    RETURN product;
}
