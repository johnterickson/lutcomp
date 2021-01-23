FUNCTION main(x:U8) -> U8 {
    CALL res:U8 := fac(x);
    RETURN res;
}

FUNCTION fac(x:U8) -> U8 {
    IF (x != 0) {
        CALL prev:U8 := fac((x-1));
        RETURN (prev * x);
    } ELSE {
        RETURN 1;
    }
}
