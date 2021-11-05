struct heap {
    current: &u8;
    bytes: u8[1024];
}

fn get_heap() -> &heap {
    static heap: heap;
    return &heap;
}

fn heap_init() -> &heap {
    heap: &heap := get_heap();
    heap->current := ((0x80001004) AS &u8); /* (((((heap) AS &u8) AS usize) + 4) AS &u8); */
    return heap;
}

fn heap_alloc(n: usize) -> &u8 {
    heap: &heap := get_heap();
    b: &u8 := heap->current;
    heap->current := (heap->current + n);
    return b;
}

fn test1() -> usize {
    heap: &heap := get_heap();
    heap_init();
    a1: &u8 := heap_alloc(0x1);
    if (((a1) AS usize) != 0x80001004) {
        return ((a1) AS usize);
    }

    return 0x0;
}