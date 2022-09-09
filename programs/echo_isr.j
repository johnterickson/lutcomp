!include 'echoline.j'
!include 'Queue.j'

fn [inline] ttyin_queue() -> &Queue {
    static q: Queue;
    return &q;
}

fn isr() {
    static ready: usize;

    q: &Queue = ttyin_queue();

    if (0x90ABCDEF != ready) {
        queue_init(q);
        ready = 0x90ABCDEF;
    }

    queue_push(q, (ttyin & 127));
}

fn main() -> u8 {
    q: &Queue = ttyin_queue();

    ch: u8 = 0;
    while (0 == 0) {
        ch = queue_pop(q);
        if (ch == 'q') {
            return 0;
        }
        if (ch != 0) {
            ttyout(ch);
        }
    }
}