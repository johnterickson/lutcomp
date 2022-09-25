fn cmp_usize(x:usize,y:usize) -> u8 {
    if (x < y) {
        return 255;
    }

    if (x == y) {
        return 0;
    }

    if (x > y) {
        return 1;
    }

    return 7;
}
