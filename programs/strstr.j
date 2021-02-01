!include 'strlen.j'
!include 'strncmp.j'

/*
Returns a pointer to the first occurrence of str2 in str1, or a null pointer if str2 is not part of str1.
*/
fn strstr(str1: &char, str2: &char) -> &char {
    len2 := strlen(str2);
    while(*str1 != 0) {
        
        if (0 == strncmp(str1, str2, len2)) {
            return str1;
        }

        str1 := (str1 + 0x1);
    }
    str1 := ((0x0) AS &char);
    return str1;
}
