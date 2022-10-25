!include 'ps2.j'

fn main() -> u8 {

    shift_held: u8 = 0;
    release_in_progress: u8 = 0;
    
    while (0 == 0) {
        ch = 0;
        
        rtr: u8 = io_ready_to_read();
        

        if ((rtr & 1) != 0) {
            ch = ttyin;
        }
        else if ((rtr & 4) != 0) {
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
                ch = c;
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

            # ttyout(10);
        }

        if (ch != 0) {
            ttyout(ch);
            if (ch == 'q') {
                return 0;
            }
        }
    }
}