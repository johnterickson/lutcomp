fn counter1(n:u8) -> u8 {
    static c: u8;

    if (n == 0) {
        c = 0;
    } else {
        c = (c+n);
    }

    return c;
}

fn test1(n: u8) -> u8 {
    counter1(0);
    counter1(n);
    return counter1(n);
}

static globalc: u8;

fn counter2(n:u8) -> u8 {
    if (n == 0) {
        globalc = 0;
    } else {
        globalc = (globalc+n);
    }

    return globalc;
}

fn [inline] equal(n: u8, nn: u8) -> u8 {
    globalc = n;
    if (globalc != nn) {
        return 0;
    } else {
        return 1;
    }
}

fn inlined(n: u8, nn: u8) -> u8 {
    return equal(n, nn);
}