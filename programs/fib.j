FUNCTION main() {
    CALL result : U8 := fib(7);
    RETURN result;
}

FUNCTION fib(n:U8) {
    ASSIGN sum1:U8 := 0;
    ASSIGN sum2:U8 := 0;
    IF (n != 0) {
        IF (n != 1) {
            CALL sum1: U8 := fib((n - 1));
            CALL sum2:U8 := fib((n - 2));
            RETURN (sum1 + sum2);
        }
        RETURN 1;
    }
    RETURN 0;
}