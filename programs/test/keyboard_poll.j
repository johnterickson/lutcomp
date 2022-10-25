!include 'ps2.j'
!include 'Queue.j'

static stdin: Queue;

struct Keyboard {
    shift_held: u8;
    release_in_progress: u8;
}

fn KeyBoard_init(k: &Keyboard) {
    k->shift_held = 0;
    k->release_in_progress = 0;
}

fn Keyboard_poll(k: &Keyboard) {
    if ((io_ready_to_read() & 4) == 0) {
        return;
    }

    code: u8 = io_read2();
    # printHex(code);
    index: usize = (0x4 * code);
    c: u8;

    if (shift_held != 0) {
        c = PS2_SCAN_CODE_TO_ASCII[(index + 0x1)];
    } else {
        c = PS2_SCAN_CODE_TO_ASCII[index];
    }

    if (release_in_progress == 0) {
        queue_push(&stdin, c);
        # ttyout('-');
        # ttyout(c);
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x2)]) {
        if (release_in_progress != 0) {
            # ttyout('-');
            # ttyout('s');
            shift_held = 0;
        } else {
            # ttyout('-');
            # ttyout('S');
            shift_held = 1;
        }
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x3)]) {
        # ttyout('-');
        # ttyout('R');
        release_in_progress = 1;
    } else {
        # ttyout('-');
        # ttyout('r');
        release_in_progress = 0;
    }
}

static keyboard: Keyboard;

fn main() -> u8 {
    queue_init(&stdin);
    KeyBoard_init(&keyboard);

    while (0 == 0) {

        if ((io_ready_to_read() & 1) != 0) {
            queue_push(&stdin, ttyin);
        }
        
        Keyboard_poll(&keyboard);

        ch = queue_pop(&stdin);

        if (ch != 0) {
            ttyout(ch);
            if (ch == 'q') {
                return 0;
            }
        }
    }
}