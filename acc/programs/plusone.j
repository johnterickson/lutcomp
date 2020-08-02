FUNCTION main() {
    CALL res := plusone(6);
    RETURN res;
}

FUNCTION plusone(x) {
    ASSIGN sum := (x + 1);
    RETURN sum;
}
