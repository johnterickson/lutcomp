fn main(p_x: &usize, p_y: &usize) -> usize {
    sum: usize = (*p_x + *p_y);
    *p_x = sum;
    return *p_x;
}

