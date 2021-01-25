fn main() -> u8 {
    sum:u8 := identity(7);
    return sum;
}

fn identity(a:u8) -> u8 {
    return a;
}