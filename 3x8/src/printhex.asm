!init_stack
# call
copy acc 7
!call :print_hex
#copy pc fe
!return
:print_hex
copy addr 't'
or mem acc 0
add acc -0a
and acc 80
addifzero pc pc 1
copy pc :print_hex_digit
copy acc 'a'
add acc -0a
copy pc :print_hex_exit
:print_hex_digit
copy acc '0'
:print_hex_exit
add acc acc mem
copy addr 01
or mem acc 0
!return