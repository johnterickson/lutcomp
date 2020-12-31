FUNCTION main() {
    CALL res := fac(5);
    RETURN res;
}

FUNCTION fac(x) {
    ASSIGN product := x;
    WHILE (x > 2) {
        ASSIGN x := (x - 1);
        ASSIGN product := (product * x);
    }
    RETURN product;
}
