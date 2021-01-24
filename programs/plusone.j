FUNCTION main() -> U8 {
    CALL res:U8 := plusone(6);
    RETURN res;
}

FUNCTION plusone(x:U8) -> U8 {
    sum:U8 := (x + 1);
    RETURN sum;
}
