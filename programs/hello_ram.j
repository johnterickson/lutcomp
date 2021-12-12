!image_base_address=((RAM_MIN+1024))
fn main() -> usize {
    ttyout('H');
    ttyout('i');
    ttyout('_');
    ttyout('f');
    ttyout('r');
    ttyout('o');
    ttyout('m');
    ttyout('_');
    ttyout('R');
    ttyout('A');
    ttyout('M');
    ttyout('!');
    ttyout(10);
    return 0xAABBCCDD;
}