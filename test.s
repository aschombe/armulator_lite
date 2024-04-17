.global _main

.text
_main: 
    mov x0, #1
    ret

.data
hello_str: .asciz "Hello, World!\n"
hello_len: .word 14
other_data: .skip 100
arr: .byte 1, 2, 3, 4, 5
arr_len: .word 5

