!include 'mul.j'

fn shiftright1(a:usize) -> usize {
    a[0] = (a[0] >ROR> 1);
    a[1] = (a[1] >ROR> 1);
    a[2] = (a[2] >ROR> 1);
    a[3] = (a[3] >ROR> 1);
    
    a[0] = (a[0] & 127);
    a[0] = (a[0] | (a[1] & 128));
    a[1] = (a[1] & 127);
    a[1] = (a[1] | (a[2] & 128));
    a[2] = (a[2] & 127);
    a[2] = (a[2] | (a[3] & 128));
    a[3] = (a[3] & 127);

    return a;
}

fn div16(a:usize, b:usize) -> usize {
    if (a < b) {
        return 0x0;
    }

    lo: usize = 0x0;
    hi: usize = 0xFFFF;

    while (lo < hi) {
        mid: usize = shiftright1((lo + hi));
        x: usize = mul16_32(mid, b);

        if (x[2] > 0) {
            hi = (mid - 1);
            continue;
        }
        
        if (x[3] > 0) {
            hi = (mid - 1);
            continue;
        }
        
        if (x > a) {
            hi = (mid - 1);
            continue;
        }
        
        if (x == a) {
            return mid;
        }

        r: usize = (a - x);
        if (r < b) {
            return mid;
        }

        lo = (mid + 1);
    }

    return lo;
}

fn div32(a:usize, b:usize) -> usize {
    if (a < b) {
        return 0x0;
    }

    if (a < 0x100) {
        if (b < 0x100) {
            return ((a[0] / b[0]) AS usize);
        }
    }

    if (a < 0x10000) {
        if (b < 0x10000) {
            return div16(a,b);
        }
    }

    lo: usize = 0x0;
    hi: usize = 0xFFFFFFFF;

    while (lo < hi) {
        mid: usize = (shiftright1(lo) + shiftright1(hi));

        x64: U64;
        mul32_64(mid, b, &x64);

        if (x64.hi > 0x0) {
            hi = (mid - 1);
            continue;
        }

        x: usize = x64.lo;
        
        if (x > a) {
            hi = (mid - 1);
            continue;
        }
        
        if (x == a) {
            return mid;
        }

        r: usize = (a - x);
        if (r < b) {
            return mid;
        }

        lo = (mid + 1);
    }

    return lo;
}