struct  request {
    in1: usize;
    in2: usize;
    sum: usize;
}

fn main(x: usize, y: usize) -> usize {
    r:request;
    r.in1 := x;
    r.in2 := y;
    dummy:u8 := add(&r);
    return r.sum;
}

fn add(r: &request) -> u8 {
    r->sum := (r->in1 + r->in2);
    return 0;
}

