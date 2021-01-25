struct  request {
    in1: UPTR;
    in2: UPTR;
    sum: UPTR;
}

fn main(x: UPTR, y: UPTR) -> UPTR {
    r:request;
    r.in1 := x;
    r.in2 := y;
    dummy:U8 := add(&r);
    return r.sum;
}

fn add(r: &request) -> U8 {
    r->sum := (r->in1 + r->in2);
    return 0;
}

