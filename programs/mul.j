fn mul8(x:u8, y:u8) -> u8 {
    return (x*y);
}

fn mul8_16(x:u8, y:u8) -> usize {
    return (((x) AS usize)*((y) AS usize));
}


fn mul16_32(a:usize, b:usize) -> usize {
    c: usize = (a * b);
    
    f: usize = ((a[1] AS usize) * (b[1] AS usize));
    f[3] = f[1];
    f[2] = f[0];
    f[1] = 0;
    f[0] = 0;
    c = (c + f);

    d: usize = (a * (b[1] AS usize));
    d[3] = d[2];
    d[2] = d[1];
    d[1] = d[0];
    d[0] = 0;
    c = (c + d);
    
    e: usize = ((a[1] AS usize) * b);
    e[3] = e[2];
    e[2] = e[1];
    e[1] = e[0];
    e[0] = 0;
    c = (c + e);

    return (c); 
}
