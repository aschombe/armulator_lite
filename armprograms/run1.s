.global _start

.text
_start: 
    mov x0, 1
    adr x1, hello_str 
    adr x2, hello_len 
    ldr x2, [x2, 0]
    mov x8, 64
    svc 0
    
    mov x0, 0
    mov x8, 93
    svc 0

.data
hello_str: .asciz "Hello, World!\n"
hello_len: .quad 14
test_quad: .quad 0xdeadbeef
arr: .byte 1, 2, 3, 4, 5
arr_len: .quad 5

