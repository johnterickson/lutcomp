
!include 'lcd.j'
!include 'Keyboard.j'
!include 'Queue.j'

static stdin: Queue;

fn stdio_init() {
    queue_init(&stdin);
    lcd_init();
    KeyBoard_init();
}

fn [inline] getchar() -> u8 {
    ch: u8 = 0;
    
    while (ch == 0) {
        if ((io_ready_to_read() & 1) != 0) {
            queue_push(&stdin, ttyin);
        } else {
            Keyboard_poll();
        }

        ch = queue_pop(&stdin);
    }

    return ch;
}

fn [inline] putc(c: u8) {
    io_write0(c);
    lcd_putc(c);
}

fn [inline] readline(buf:&u8) {
    while (0 == 0) {
        ch: u8 = getchar();
        if (ch == 13) {

        } else if (ch == 10) {
            *buf = 0;
            return;
        } else {
            *buf = ch;
            buf = &(buf[0x1]);
        }
    }
}

fn [inline] println(buf: &u8) {
    ch: u8 = *buf;
    while (ch != 0) {
        putc(ch);
        buf = (buf + 0x1);
        ch = *buf;
    }
}

fn test_echoline() -> u8 {
    buf: u8[20];
    readline(&(buf[0]));
    println(&(buf[0]));
    return 0;
}