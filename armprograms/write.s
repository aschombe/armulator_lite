
.global _start
.text

_start:
    mov x0, 1
    adr x1, str 
    mov x2, 14
    mov x8, 64
    svc 0

    mov x0, 0
    mov x8, 93
    svc 0

.data
str: .asciz "Hello world!\n"