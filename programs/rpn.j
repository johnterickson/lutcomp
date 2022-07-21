!include 'echoline.j'
!include 'Stack.j'
!include 'mul.j'
!include 'div.j'

fn getchar() -> u8 {
    tty: u8 = 0;
    while ((tty & 128) == 0) {
        tty = ttyin;
    }
    return (tty & 127);
}

struct RpnCalc {
    stack: Stack;
    num: usize;
    have_num: u8;
}

fn RpnCalc_init(c: &RpnCalc) {
    s: &Stack = &(c->stack);
    stack_init(s);
    c->num = 0x0;
}

fn RpnCalc_push_pending(c: &RpnCalc) {
    if (c->have_num != 0) {
        stack_push(&(c->stack), c->num);
        c->num = 0x0;
        c->have_num = 0;
    }
}

fn RpnCalc_handle(c: &RpnCalc, ch: u8) -> u8 {
    stack: &Stack = &(c->stack);

    a: usize = 0;
    b: usize = 0;

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
        sum: usize = (b+a);
        print_dec32(sum);
        ttyout(10);
        stack_push(stack, sum);
    } else if (ch == '-') {
        diff: usize = (b-a);
        print_dec32(diff);
        ttyout(10);
        stack_push(stack, diff);
    } else if (ch == '*') {
        product: usize = mul32_32(b,a);
        print_dec32(product);
        ttyout(10);
        stack_push(stack, product);
    } else if (ch == '/') {
        quotient: usize = div32(b,a);
        print_dec32(quotient);
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
    } else if (ch == 'c') {
        RpnCalc_init(c);
    } else if (ch == 'd') {
        i: u8 = 0;
        while (i < stack_count) {
            print_dec8(i);
            ttyout(':');
            print_dec32(stack_get(stack, ((i) AS usize)));
            ttyout(10);
            i = (i + 1);
        }
    } else if (ch <= '9') {
        if (ch >= '0') {
            c->have_num = 1;
            c->num = mul32_by10(c->num);
            c->num = (c->num + ((ch - '0') AS usize));
        }
    }

    return 1;
}

fn main() -> u8 {
    ttyout('R');

    static calc: RpnCalc;

    ttyout('P');
    
    RpnCalc_init(&calc);

    ttyout('N');
    ttyout(10);

    while (0 == 0) {
        ch: u8 = getchar();
        result: u8 = RpnCalc_handle(&calc, ch);
        if (result == 0) {
            return 0;
        }
    }

    return 1;
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

fn print_dec32(a:usize) {
    if (a >= 0xA) {
        b: usize = div32_by10(a);
        print_dec32(b);
        a = (a - mul32_by10(b));
    }

    ttyout((a[0] + '0'));
}

fn print_dec8(a:u8) {
    if (a >= 10) {
        b: u8 = (a / 10);
        print_dec8(b);
        a = (a - (b * 10));
    }

    ttyout((a + '0'));
}

fn print_dec8_test(a:u8, b:u8) -> u8 {
    print_dec8(a);
    return 0;
}

fn print_dec32_test(a:usize, b:usize) -> u8 {
    print_dec32(a);
    return 0;
}