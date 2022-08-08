fn [inline] mul8(x:u8, y:u8) -> u8 {
    return (x*y);
}

fn [inline] mul8_16(x:u8, y:u8) -> u16 {
    return __mul8_16(x,y);
}

fn [inline] mul16_32(a:usize, b:usize) -> usize {
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

fn [inline] mul32_32(a:usize, b:usize) -> usize {
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

fn [inline] mul32_by10(a:usize) -> usize {
    a = (a + a); /* a is 2a */
    b: usize = (a + a); /* b is 4a */
    b = (b + b); /* b is 8a */
    return (b + a);
}

!include 'U64.j'

fn mul32_64(a:usize, b:usize, c: &U64) {
    a_hi: usize = 0x0;
    a_hi[1] = a[3];
    a_hi[0] = a[2];

    b_hi: usize = 0x0;
    b_hi[1] = b[3];
    b_hi[0] = b[2];

    c->lo = mul16_32(a, b);
    c->hi = mul16_32(a_hi, b_hi);

    temp64: U64;
    temp: usize;
    temp_lo: usize = 0x0;
    temp_hi: usize = 0x0;

    temp = mul16_32(a_hi, b);
    temp_lo[2] = temp[0];
    temp_lo[3] = temp[1];
    temp_hi[0] = temp[2];
    temp_hi[1] = temp[3];

    temp64.lo = temp_lo;
    temp64.hi = temp_hi;
    add_U64(c, &temp64, c);

    temp = mul16_32(a, b_hi);
    temp_lo[2] = temp[0];
    temp_lo[3] = temp[1];
    temp_hi[0] = temp[2];
    temp_hi[1] = temp[3];

    temp64.lo = temp_lo;
    temp64.hi = temp_hi;
    add_U64(c, &temp64, c);
}

fn test_mul32_64(a: usize, b: usize) -> &U64 {
    static c: U64;
    mul32_64(a,b,&c);
    return (&c);
}