FUNCTION main() {
    CALL product := fac(0);
    RETURN product;
}

FUNCTION fac(x) {
    ASSIGN product := 1;
    /*
    IF (x != 0) {
        CALL product := fac((x-1));
        RETURN (x*product);
    }
    */
    RETURN product;
}
