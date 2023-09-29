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

fn test2(n: u8) -> u8 {
    counter2(0);
    counter2(n);
    return counter2(n);
}