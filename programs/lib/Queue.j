struct [align 0x100] Queue {
    values: u8[0x100];
    first: u8;
    last: u8;
}

fn [inline] queue_init(s: &Queue) {
    s->first = 0;
    s->last = 0;
}

fn [inline] queue_pop(s: &Queue) -> u8 {
    if (s->first == s->last) {
        return 0;
    }
    
    values: &u8 = &(s->values);
    value: u8 = (values[s->first]);
    s->first = (s->first + 1);
    return value;
}

fn [inline] queue_push(s: &Queue, n: u8) {
    values: &u8 = &(s->values);
    values[s->last] = n;
    s->last = (s->last + 1);
}
