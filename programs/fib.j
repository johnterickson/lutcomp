fn main(x:U8) -> U8 {
    result : U8 := fib(x);
    return result;
}

fn fib(n:U8) -> U8 {
    if (n != 0) {
        if (n != 1) {
            sum1:U8 := fib((n - 1));
            sum2:U8 := fib((n - 2));
            return (sum1 + sum2);
        }
        return 1;
    }
    return 0;
}