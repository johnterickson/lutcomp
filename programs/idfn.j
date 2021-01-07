FUNCTION main() {
    CALL sum:U8 := identity(7);
    RETURN sum;
}

FUNCTION identity(a:U8) {
    RETURN a;
}