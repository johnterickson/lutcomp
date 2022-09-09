!include 'echoline.j'
!include 'Queue.j'

fn [inline] ttyin_queue() -> &Queue {
    static q: Queue;
    return &q;
}

fn isr() {
    q: &Queue = ttyin_queue();
    queue_push(q, (ttyin & 127));
}

fn main() -> u8 {
    q: &Queue = ttyin_queue();
    queue_init(q);

    enable_interrupts();

    ch: u8 = 0;
    while (0 == 0) {
        disable_interrupts();
        ch = queue_pop(q);
        enable_interrupts();
        if (ch == 'q') {
            return 0;
        }
        if (ch != 0) {
            ttyout(ch);
        }
    }
}