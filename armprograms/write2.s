.global _start
.extern printf

.text
_start:
    mov x0, 1
    adr x1, str 
    mov x2, 14
    mov x8, 64
    svc 0

    adr x18, vec1 
    adr x19, vec2 
    adr x30, result 
    mov x21, 0
    sub x21, x21, 8
    mov x17, 3

_loop:
    add x21, x21, 8
    ldr x22, [x18, x21]
    ldr x15, [x19, x21]
    
    mul x22, x22, x15 
    add x16, x16, x22 

    sub x17, x17, 1
    cmp x17, 0
    b.ne _loop

    str x16, [x20, 0]
    ldr x30, [x20, 0]

    adr x0, format 
    adr x1, vec
    mov x2, x30
    bl printf

    mov x0, 0
    mov x8, 93
    svc 0

.data
vec1: .quad 10, 20, 30
vec2: .quad 1, 2, 3
len: .quad 3
result: .quad 0
str: .asciz "Hello world!\n"
format: .asciz "%s: %d\n"
vec: .asciz "dot product is"