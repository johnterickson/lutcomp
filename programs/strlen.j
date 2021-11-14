fn strlen(str: &char) -> usize {
    len: usize = 0x0;
    while(*str != 0) {
        len = (len + 0x1);
        str = (str + 0x1);
    }
    return len;
}
