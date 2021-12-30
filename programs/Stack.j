struct Stack {
    values: usize[0x10];
    first_free: usize;
}

fn stack_init(s: &Stack) {
    s->first_free = 0x0;
}

fn stack_pop(s: &Stack) -> usize{
    s->first_free = (s->first_free - 1);
    values: &usize = &(s->values);
    return (values[s->first_free]);
}

fn stack_push(s: &Stack, n: usize) {
    values: &usize = &(s->values);
    values[s->first_free] = n;
    s->first_free = (s->first_free + 1);
}

fn stack_get(s: &Stack, n: usize) -> usize {
    values: &usize = &(s->values);
    return (values[n]);
}