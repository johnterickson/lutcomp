FUNCTION main() {
    CALL res:U8 := plusone(6);
    RETURN res;
}

FUNCTION plusone(x:U8) {
    ASSIGN sum:U8 := (x + 1);
    RETURN sum;
}
