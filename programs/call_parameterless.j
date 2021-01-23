FUNCTION main() -> U8 {
    CALL sum:U8 := seven();
    RETURN sum;
}

FUNCTION seven() -> U8 {
    RETURN 7;
}