!include 'echoline.j'

struct Stack {
    values: u8[0x10];
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

struct RpnCalc {
    stack: Stack;
    num: u8;
    have_num: u8;
}

fn RpnCalc_init(c: &RpnCalc) {
    s: &Stack = &(c->stack);
    stack_init(s);
    c->num = 0;
}

fn RpnCalc_push_pending(c: &RpnCalc) {
    if (c->have_num != 0) {
        stack_push(&(c->stack), c->num);
        c->num = 0;
        c->have_num = 0;
    }
}

fn RpnCalc_handle(c: &RpnCalc, ch: u8) -> u8 {
    stack: &Stack = &(c->stack);

    a: u8 = 0;
    b: u8 = 0;

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
        RpnCalc_push_pending(c);
    }

    stack_count_usize: usize = stack->first_free;
    stack_count: u8 = stack_count_usize[0];

    if (stack_needed > stack_count) {
        ttyout('E');
        ttyout('R');
        ttyout('R');
        ttyout(10);
        return 1;
    }

    if (stack_needed > 0) {
        a = stack_pop(stack);
    }
    if (stack_needed > 1) {
        b = stack_pop(stack);
    }
    
    if (ch == '+') {         
        sum: u8 = (b+a);
        print_dec(sum);
        ttyout(10);
        stack_push(stack, sum);
    } else if (ch == '-') {
        diff: u8 = (b-a);
        print_dec(diff);
        ttyout(10);
        stack_push(stack, diff);
    } else if (ch == '*') {
        product: u8 = (b*a);
        print_dec(product);
        ttyout(10);
        stack_push(stack, product);
    } else if (ch == '/') {
        quotient: u8 = (b / a);
        print_dec(quotient);
        ttyout(10);
        stack_push(stack, quotient);
    } else if (ch == 's') {
        stack_push(stack, a);
        stack_push(stack, b);
    } else if (ch == 'q') {
        return 0;
    } else if (ch == 10) {
        RpnCalc_push_pending(c);
    } else if (ch == 32) {
        RpnCalc_push_pending(c);
    }
    else if (ch == 'c') {
        RpnCalc_init(c);
    } else if (ch == 'd') {
        i: u8 = 0;
        while (i < stack_count) {
            print_dec(i);
            ttyout(':');
            print_dec(stack_get(stack, ((i) AS usize)));
            ttyout(10);
            i = (i + 1);
        }
    } else if (ch <= '9') {
        if (ch >= '0') {
            c->have_num = 1;
            c->num = (c->num * 10);
            c->num = (c->num + (ch - '0'));
        }
    }

    return 1;
}

fn main() -> u8 {
    calcs: RpnCalc[8];
    calc: &RpnCalc = &(calcs[0x0]);

    RpnCalc_init(calc);

    while (0 == 0) {
        ch: u8 = getchar();
        result: u8 = RpnCalc_handle(calc, ch);
        if (result == 0) {
            return 0;
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
