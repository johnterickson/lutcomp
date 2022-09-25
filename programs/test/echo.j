!include 'stdio.j'
#comment test
fn main() -> u8 {
    ttyout('H');
    ttyout('i');
    ttyout('!');
    ttyout(10);
    ch: u8 = 0;
    while (0 == 0) {
        ttyout('>');
        ttyout(':');
        ch = 0;
        while (ch != 10) {
            ch = getchar();
            ttyout(ch);

            if (ch == 'q') {
                return 0;
            }
        }
    }
}