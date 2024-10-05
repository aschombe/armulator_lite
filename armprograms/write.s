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
    adr x1, vec
    mov x2, 10
    mov x3, 20
    mov x4, 30
    bl printf

    mov x0, 0
    mov x8, 93
    svc 0

.data
str: .asciz "Hello world!\n"
format: .asciz "%s: %d, %d, %d\n"
vec: .asciz "vector is"