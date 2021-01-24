FUNCTION main(p_x: &UPTR, p_y: &UPTR) -> UPTR {
    sum: UPTR := (*p_x + *p_y);
    *p_x := sum;
    RETURN *p_x;
}

