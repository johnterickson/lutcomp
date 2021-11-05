struct  request {
    in1: usize;
    in2: usize;
    sum: usize;
}

fn get_request() -> &request {
    static r: request;
    return &r;
}

fn test_ret_static(x: usize, y: usize) -> usize {
    r1: &request := get_request();
    r1->in1 := x;
    r1->in2 := y;
    r2: &request := get_request();
    return (r2->in1 + r2->in2);
}