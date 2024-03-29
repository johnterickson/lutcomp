!include 'Queue.j'

static q: Queue;
const NUM: u8 = 10;

fn queue_simple(x: u8) -> u8 {
    queue_init(&q);
    queue_push(&q, x);
    return queue_pop(&q);
}

fn main() -> u8 {
    queue_init(&q);
    n: u8 = 1;
    while (n != (NUM+1)) {
        queue_push(&q, n);
        n = (n + 1);
    }
    n = NUM;
    while (n != 0) {
        if (n != queue_pop(&q)) {
            return n;
        }
        n = (n - 1);
    }

    if (0 != queue_pop(&q)) {
        return 255;
    }
}