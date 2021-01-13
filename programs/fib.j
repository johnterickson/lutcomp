FUNCTION main(x:U8) {
    CALL result : U8 := fib(x);
    RETURN result;
}

FUNCTION fib(n:U8) {
    IF (n != 0) {
        IF (n != 1) {
            CALL sum1:U8 := fib((n - 1));
            CALL sum2:U8 := fib((n - 2));
            RETURN (sum1 + sum2);
        }
        RETURN 1;
    }
    RETURN 0;
}