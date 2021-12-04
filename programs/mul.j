fn mul8(x:u8, y:u8) -> u8 {
    return (x*y);
}

fn mul8_16(x:u8, y:u8) -> usize {
    return (((x) AS usize)*((y) AS usize));
}

/*
fn mul8_16(a:usize, b:usize) -> usize {
    c = (a * b);
    d = (a * (b >> 8));
    e = ((a >> 8) * b);
    f = ((a >> 8) * (b >> 8));
    c = (c + (f << 16));
    c = (c + (d << 8));
    c = (c + (e << 8));
    return (c); 
}
*/