FUNCTION main() {
    CALL result := fib(13);
    RETURN result;
}

FUNCTION fib(n) {
    IF (n == 0) {
        RETURN 0;
    }
    IF (n == 1) {
        RETURN 1;
    }
    CALL sum1 := fib((n - 1));
    CALL sum2 := fib((n - 2));
    RETURN (sum1 + sum2);
}