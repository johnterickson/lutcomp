FUNCTION main(p_x: &UPTR, p_y: &UPTR) -> UPTR {
    ASSIGN sum: UPTR := (*p_x + *p_y);
    ASSIGN *p_x := sum;
    RETURN *p_x;
}

