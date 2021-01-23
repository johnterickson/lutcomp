STRUCT request {
    in1: UPTR;
    in2: UPTR;
    sum: UPTR;
}

FUNCTION main(x: UPTR, y: UPTR) -> UPTR {
    DECLARE r:request;
    ASSIGN r.in1 := x;
    ASSIGN r.in2 := y;
    CALL dummy:U8 := add(&r);
    RETURN r.sum;
}

FUNCTION add(r: &<request>) -> U8 {
    ASSIGN r->sum := (r->in1 + r->in2);
    RETURN 0;
}

