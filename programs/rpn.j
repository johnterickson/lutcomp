!include 'divide.j'
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
    values: &u8 = (s->values);
    return (values[s->first_free]);
}

fn stack_push(s: &Stack, n: u8) {
    values: &u8 = &(s->values);
    values[s->i] = n;
    s->first_free = (s->first_free + 1);
}

fn main() -> u8 {
    s: Stack;
    stack_init(&s);

    line: u8[10];
    while (0 == 0) {
        readline(&line[0]);

        if (line[0] == '+') {
            add1 = stack_pop(&s);
            add2 = stack_pop(&s);
            sum: u8 = (add1+add2);
            print_dec(sum);
            stack_push(&s, sum);
        }
        else
        {
            a = 0;
            p_ch: &u8 = &(line[0]);
            while ((*p_ch) != 0) {
                a = (a * 10);
                b: u8 = ((*p_ch) - '0');
                a = (a + b);
                p_ch = &(p_ch[1]);
            }
            stack_push(&s, a);
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
    if (a > 100) {
        b = divide(a, 100);
        ttyout((b + '0'));
        a = (a - (b*100));
    }

    if (a > 10) {
        b = divide(a, 10);
        ttyout((b + '0'));
        a = (a - (b*10));
    }

    ttyout((a + '0'));
}