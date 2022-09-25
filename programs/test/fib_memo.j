fn fib(n:u8) -> u8 {
    static fib_memo: u8[14];

    if (n == 0) {
        while (n < 14) {
            fib_memo[n] = 0;
            n = (n+1);
        }
        return 0;
    }

    f:u8 = fib_memo[n];
    /* f:u8 = 0; */
    if (f == 0) {
        if (n == 1) {
            f = 1;
            return f;
        } else {
            sum1:u8 = fib((n - 1));
            sum2:u8 = fib((n - 2));
            f = (sum1 + sum2);
            return f;
        }
    }

    return f;
}

fn main(n:u8) -> u8 {
    fib(0);
    return fib(n);
}