struct heap_entry {
    used: usize;
    free: usize;
    next: &heap_entry;
}

fn heap_start() -> &u8 {
    static heap_bytes: u8[(1024+(3*4))];
    return &(heap_bytes[0]);
}

fn heap_init() {
    e: &heap_entry := (heap_start() AS &heap_entry);
    e->used := (0 AS usize);
    e->free := 1024;
    e->next := (0 AS &heap_entry);

}

fn heap_alloc() {
    e: &heap_entry := (heap_start() AS &heap_entry);
    while (e != (0 AS &heap_entry)) {

        e := e->next;
    }
}

fn main(n:u8) -> usize {
    return (heap_start() AS usize);
}