
/* 
Compares up to num characters of the C string str1 to those of the C string str2.
This function starts comparing the first character of each string.
If they are equal to each other, it continues with the following pairs until the characters differ, 
until a terminating null-character is reached, or until num characters match in both strings, 
whichever happens first.
*/

fn strncmp(str1: &char, str2: &char, num: usize) -> u8 {
    while (num != 0x0) {
        if (*str1 != *str2) {
            if (*str1 < *str2) {
                return 255;
            } else {
                return 1;
            }
        }

        if (*str1 == 0) {
            return 0;
        }

        str1 = (str1 + 0x1);
        str2 = (str2 + 0x1);
        num = (num - 0x1);
    }
    return 0;
}