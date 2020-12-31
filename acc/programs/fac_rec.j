FUNCTION main() {
    CALL res := fac(5);
    RETURN res;
}

FUNCTION fac(x) {
    IF (x != 0) {
        CALL prev := fac((x-1));
        RETURN (prev * x);
    } ELSE {
        RETURN 1;
    }
}
