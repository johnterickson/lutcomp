fn divide(x:u8, y:u8) -> u8 {
    q:u8 = 0;
    while (x >= y) {
        q = (q + 1);
        x = (x - y);
    }
    return q;
}