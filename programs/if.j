fn if_eq(x:u8, y:u8) -> u8 {
    if (x == y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_gt(x:u8, y:u8) -> u8 {
    if (x > y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_gte(x:u8, y:u8) -> u8 {
    if (x >= y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_lt(x:u8, y:u8) -> u8 {
    if (x < y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_lte(x:u8, y:u8) -> u8 {
    if (x <= y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_ne_uptr(x:usize, y:usize) -> u8 {
    if (x != y) {
        return 1;
    } else {
        return 0;
    }
}

fn if_ne(x:u8, y:u8) -> u8 {
    if (x != y) {
        return 1;
    } else {
        return 0;
    }
}