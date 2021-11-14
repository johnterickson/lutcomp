struct  request {
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
