fn main(x:u8) -> u8 {
    result : u8 := fib(x);
    return result;
}

fn fib(n:u8) -> u8 {
    if (n != 0) {
        if (n != 1) {
            sum1:u8 := fib((n - 1));
            sum2:u8 := fib((n - 2));
            return (sum1 + sum2);
        }
        return 1;
    }
    return 0;
}