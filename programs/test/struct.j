struct request {
    in1: usize;
    in2: usize;
    sum: usize;
}

fn add(r: &request) {
    r->sum = (r->in1 + r->in2);
}

fn test_add(x: usize, y: usize) -> usize {
    r:request;
    r.in1 = x;
    r.in2 = y;
    add(&r);
    return r.sum;
}

fn test_add2(x: usize, y: usize) -> usize {
    rs:request[2];
    r0: &request = &(rs[0x0]);
    r1: &request = &(rs[0x1]);
    r0->in1 = x;
    r0->in2 = 0x0;
    r1->in1 = 0x0;
    r1->in2 = y;
    add(r0);
    add(r1);
    return (r0->sum + r1->sum);
}
