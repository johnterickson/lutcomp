!include 'rpnlib.j'

fn main() -> u8 {
    lcd_init();

    putc('R');

    static calc: RpnCalc;

    putc('P');
    
    RpnCalc_init(&calc);

    putc('N');
    putc(10);

    return RpnCalc_run(&calc);
}