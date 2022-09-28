!include 'stdio.j'
!include 'Queue.j'
!include 'ps2.j'
!include 'print_hex.j'

struct Globals {
    stdin: Queue;
    shift_held: u8;
    release_in_progress: u8;
}

fn [inline] globals() -> &Globals {
    static g: Globals;
    return &g;
}

fn handle_ps2() {
    g = globals();
    
    ch: u8;
    code: u8;
    index: usize;

    code = io_read2();

    #printHex(code);

    index = (0x4 * code);
    if (g->shift_held != 0) {
        ch = PS2_SCAN_CODE_TO_ASCII[(index + 0x1)];
    } else {
        ch = PS2_SCAN_CODE_TO_ASCII[index];
    }

    if ((g->release_in_progress) == 0) {
        #if (ch != 0) {
            queue_push(&g->stdin, ch);
            #ttyout('-');
            #ttyout(ch);
        #}
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x2)]) {
        if (g->release_in_progress != 0) {
            #ttyout('-');
            #ttyout('s');
            g->shift_held = 0;
        } else {
            #ttyout('-');
            #ttyout('S');
            g->shift_held = 1;
        }
    }

    if (0 != PS2_SCAN_CODE_TO_ASCII[(index + 0x3)]) {
        #ttyout('-');
        #ttyout('R');
        g->release_in_progress = 1;
    } else {
        #ttyout('-');
        #ttyout('r');
        g->release_in_progress = 0;
    }

    #ttyout(32);
}

fn handle_tty() {
    g = globals();

    queue_push(&g->stdin, ttyin);
}

fn isr() {
    rtr = io_ready_to_read();

    if ((rtr & 1) != 0) {
        handle_tty();
    }

    if ((rtr & 4) != 0) {
        handle_ps2();
    }
}

fn [inline] getc() -> u8 {
    g = globals();
    disable_interrupts();
    ch: u8 = queue_pop(&g->stdin);
    enable_interrupts();
    return ch;
}

fn main() -> u8 {
    g = globals();
    g->shift_held = 0;
    g->release_in_progress = 0;
    queue_init(&g->stdin);

    enable_interrupts();

    while ('a' != getc()) {
    }

    ttyout('a');

    while (0 == 0) {

        ch = getc();

        if (ch != 0) {
            if (ch == 'q') {
                return 0;
            }

            ttyout(ch);
        }
    }
}