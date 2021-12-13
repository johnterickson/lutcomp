!include 'echoline.j'

struct Stack {
    values: u8[100];
    first_free: usize;
}

fn stack_init(s: &Stack) {
    s->first_free = 0x0;
}

fn stack_pop(s: &Stack) -> u8{
    s->first_free = (s->first_free - 1);
    values: &u8 = &(s->values);
    return (values[s->first_free]);
}

fn stack_push(s: &Stack, n: u8) {
    values: &u8 = &(s->values);
    values[s->first_free] = n;
    s->first_free = (s->first_free + 1);
}

fn stack_get(s: &Stack, n: usize) -> u8 {
    values: &u8 = &(s->values);
    return (values[n]);
}

fn getchar() -> u8 {
    tty: u8 = 0;
    while ((tty & 128) == 0) {
        tty = ttyin;
    }
    return (tty & 127);
}

fn main() -> u8 {
    s: Stack;
    stack_init(&s);

    num: u8 = 0;
    a: u8 = 0;
    b: u8 = 0;
    have_num: u8 = 0;

    while (0 == 0) {
        
        ch: u8 = getchar();

        stack_needed: u8;
        if (ch == '+') {
            stack_needed = 2;
        } else if (ch == '-') {
            stack_needed = 2;
        } else if (ch == '*') {
            stack_needed = 2;
        } else if (ch == '/') {
            stack_needed = 2;
        } else if (ch == 's') {
            stack_needed = 2;
        } else {
            stack_needed = 0;
        }

        if (stack_needed > 0) {
            if (have_num != 0) {
                stack_push(&s, num);
                num = 0;
                have_num = 0;
            }
        }

        stack_count_usize: usize = s.first_free;
        stack_count: u8 = stack_count_usize[0];

        if (stack_needed > stack_count) {
            ttyout('E');
            ttyout('R');
            ttyout('R');
            ttyout(10);
            continue;
        }

        if (stack_needed > 0) {
            a = stack_pop(&s);
        }
        if (stack_needed > 1) {
            b = stack_pop(&s);
        }
        
        if (ch == '+') {         
            sum: u8 = (b+a);
            print_dec(sum);
            ttyout(10);
            stack_push(&s, sum);
        } else if (ch == '-') {
            diff: u8 = (b-a);
            print_dec(diff);
            ttyout(10);
            stack_push(&s, diff);
        } else if (ch == '*') {
            product: u8 = (b*a);
            print_dec(product);
            ttyout(10);
            stack_push(&s, product);
        } else if (ch == '/') {
            quotient: u8 = (b / a);
            print_dec(quotient);
            ttyout(10);
            stack_push(&s, quotient);
        } else if (ch == 's') {
            stack_push(&s, a);
            stack_push(&s, b);
        } else if (ch == 'q') {
            return 0;
        } else if (ch == 10) {
            if (have_num != 0) {
                stack_push(&s, num);
                num = 0;
                have_num = 0;
            }
        } else if (ch == 32) {
            if (have_num != 0) {
                stack_push(&s, num);
                num = 0;
                have_num = 0;
            }
        }
        else if (ch == 'c') {
            stack_init(&s);
        } else if (ch == 'd') {
            i: u8 = 0;
            while (i < stack_count) {
                print_dec(i);
                ttyout(':');
                print_dec(stack_get(&s, ((i) AS usize)));
                ttyout(10);
                i = (i + 1);
            }
        } else if (ch <= '9') {
            if (ch >= '0') {
                have_num = 1;
                num = (num * 10);
                num = (num + (ch - '0'));
            }
        }
    }
}

fn print_digit(a:u8) {
    if (a < 10) {
        ttyout((a + '0'));
    } else {
        a = (a - 10);
        a = (a + 'A');
        ttyout(a);
    }
}

fn print_dec(a:u8) {
    if (a >= 100) {
        b: u8 = (a / 100);
        ttyout((b + '0'));
        a = (a - (b*100));

        b = (a / 10);
        ttyout((b + '0'));
        a = (a - (b*10));
    } else if (a >= 10) {
        b = (a / 10);
        ttyout((b + '0'));
        a = (a - (b*10));
    }

    ttyout((a + '0'));
}

fn print_dec_test(a:u8, b:u8) -> u8 {
    print_dec(a);
    return 0;
}