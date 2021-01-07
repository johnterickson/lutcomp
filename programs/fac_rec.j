FUNCTION main() {
    CALL res:U8 := fac(5);
    RETURN res;
}

FUNCTION fac(x:U8) {
    IF (x != 0) {
        CALL prev:U8 := fac((x-1));
        RETURN (prev * x);
    } ELSE {
        RETURN 1;
    }
}
