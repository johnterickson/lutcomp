fn counter(n:u8) -> u8 {
    static c: u8;

    if (n == 0) {
        c = 0;
    } else {
        c = (c+n);
    }

    return c;
}

fn main(n: u8) -> u8 {
    counter(0);
    counter(n);
    return counter(n);
}