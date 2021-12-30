struct U64 {
    lo: usize;
    hi: usize;
}

fn add_U64(a: &U64, b: &U64, c: &U64) {
    c->hi = (a->hi + b->hi);
    c->lo = (a->lo + b->lo);
    if (c->lo < a->lo) {
        c->hi = (c->hi + 0x1);
    }
}

fn test_add_U64(a: &U64, b: &U64) -> &U64 {
    static c: U64;
    add_U64(a,b,&c);
    return (&c);
}