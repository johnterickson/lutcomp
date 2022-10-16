fn delay(x:usize) {
    while (x != 0x0) {
        x = (x - 1);
    }
}

fn test_delay(x:usize) -> u8 {
    delay(x);
    return 0;
}