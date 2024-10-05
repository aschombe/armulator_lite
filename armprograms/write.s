.global _start
.extern printf

.text
_start:
    mov x0, 1
    adr x1, str 
    mov x2, 14
    mov x8, 64
    svc 0

    adr x0, format 
    mov x1, 10
    mov x2, 20
    mov x3, 30
    bl printf

    mov x0, 0
    mov x8, 93
    svc 0

.data
str: .asciz "Hello world!\n"
format: .asciz "%d, %d, %d\n"