!include 'cmp_usize.j'

struct heap_entry {
    next: &heap_entry;
    len: usize;
    free: u8;
    junk: u8[3];
    data: u8[0];
}

struct heap {
    head: &heap_entry;
    bytes: u8[1024];
}

fn get_heap() -> &heap {
    static heap: heap;
    return &heap;
}

fn get_heap_head() -> &heap_entry {
    heap: &heap = get_heap();
    return heap->head;
}

fn heap_init() -> &heap {
    heap: &heap = get_heap();
    head: &heap_entry = ((&(heap->bytes)) AS &heap_entry);
    heap->head = head;
    head->next = ((0x0) AS &heap_entry);
    head->len = (1024 - 0xc);
    head->free = 1;
    return heap;
}

fn heap_is_entry_bad(head: &heap_entry, alloc: usize) -> u8 {
    if (head->free == 0) {
        return 1;
    }
    if (head->len < alloc) {
        return 1;
    }
    return 0;
}

fn heap_alloc(n: usize) -> &u8 {
    head: &heap_entry = get_heap_head();
    alloc: usize = (n + 0xc);
    while (heap_is_entry_bad(head,alloc) != 0) {
        head = head->next;
        if (head == ((0x0) AS &heap_entry)) {
            return ((0x0) AS &u8);
        }
    }

    new_addr: usize = ((head) AS usize);
    new_addr = (new_addr + 0xc);
    new_addr = (new_addr + head->len);
    new_addr = (new_addr - alloc);
    new: &heap_entry = ((new_addr) AS &heap_entry);
    new->next = head->next;
    new->len = n;
    new->free = 0;

    head->len = (head->len - alloc);
    head->next = new;

    return ((new_addr + 0xc) AS &u8);
}

fn test_get_heap_head() -> &heap_entry {
    heap_init();
    return get_heap_head();
}

fn test_heap_is_entry_bad(n: usize) -> u8 {
    heap_init();
    head: &heap_entry = get_heap_head();
    return heap_is_entry_bad(head, n);
}

fn test_heap_alloc(n: usize) -> &u8 {
    heap_init();
    return heap_alloc(n);
}