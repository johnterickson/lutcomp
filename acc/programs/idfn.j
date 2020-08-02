FUNCTION main() {
    CALL sum := identity(7);
    RETURN sum;
}

FUNCTION identity(a) {
    RETURN a;
}