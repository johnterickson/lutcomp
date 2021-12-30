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
    e: usize = ((a[1] AS usize) * b);
    d = (d + e);
    d[3] = d[2];
    d[2] = d[1];
    d[1] = d[0];
    d[0] = 0;
    return (c + d);
}

fn mul32_32(a:usize, b:usize) -> usize {
    c: usize = mul16_32(a, b);

    a_hi: usize = 0x0;
    a_hi[1] = a[3];
    a_hi[0] = a[2];

    b_hi: usize = 0x0;
    b_hi[1] = b[3];
    b_hi[0] = b[2];

    d: usize = (mul16_32(a_hi, b) + mul16_32(a, b_hi));
    d[3] = d[1];
    d[2] = d[0];
    d[1] = 0;
    d[0] = 0;
    return (c + d);
}
