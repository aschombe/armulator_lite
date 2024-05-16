.global _main

.text
_main: 
    mov x0, 0b1111
    mov x1, 0xdead
    mov x2, 0xbeef
    ret

.data
hello_str: .asciz "Hello, World!\n"
hello_len: .quad 14
arr: .byte 1, 2, 3, 4, 5
arr_len: .quad 5

