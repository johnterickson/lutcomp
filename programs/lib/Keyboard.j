!include 'ps2.j'

struct Keyboard {
    shift_held: u8;
    release_in_progress: u8;
}

static keyboard: Keyboard;

fn KeyBoard_init() {
    keyboard.shift_held = 0;
    keyboard.release_in_progress = 0;
}

fn Keyboard_poll() {
    if ((io_ready_to_read() & 4) == 0) {
        return;
    }

    code: u8 = io_read2();
    # printHex(code);
    index: usize = (0x4 * code);
    c: u8;

    if (keyboard.shift_held != 0) {
        c = PS2_SCAN_CODE_TO_ASCII[(index + 0x1)];
    } else {
        c = PS2_SCAN_CODE_TO_ASCII[index];
    }

    if (keyboard.release_in_progress == 0) {
        queue_push(&stdin, c);
        # ttyout('-');
        # ttyout(c);
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x2)]) {
        if (keyboard.release_in_progress != 0) {
            # ttyout('-');
            # ttyout('s');
            keyboard.shift_held = 0;
        } else {
            # ttyout('-');
            # ttyout('S');
            keyboard.shift_held = 1;
        }
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x3)]) {
        # ttyout('-');
        # ttyout('R');
        keyboard.release_in_progress = 1;
    } else {
        # ttyout('-');
        # ttyout('r');
        keyboard.release_in_progress = 0;
    }
}


