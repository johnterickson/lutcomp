fn cmp_usize(x:usize,y:usize) -> u8 {
    if (x == y) {
        return 0;
    }

    xp: &u8 := ((&x) AS &u8);
    yp: &u8 := ((&y) AS &u8);

    xp := (xp + 3);
    yp := (yp + 3);

    if ((*xp) < (*yp)) {
        return 255;
    }
    if ((*xp) > (*yp)) {
        return 1;
    }

    xp := (xp - 1);
    yp := (yp - 1);
    if ((*xp) < (*yp)) {
        return 255;
    }
    if ((*xp) > (*yp)) {
        return 1;
    }

    xp := (xp - 1);
    yp := (yp - 1);
    if ((*xp) < (*yp)) {
        return 255;
    }
    if ((*xp) > (*yp)) {
        return 1;
    }

    xp := (xp - 1);
    yp := (yp - 1);
    if ((*xp) < (*yp)) {
        return 255;
    }
    if ((*xp) > (*yp)) {
        return 1;
    }

    return 0;
}
