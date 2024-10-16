.extern printf 
.global _start 

.text
_start:
    mov x0, 1
    adr x1, s
    mov x2, 14
    mov x8, 64
    svc 0

    adr x10, vec1 
    adr x11, vec2 
    adr x12, result 
    mov x13, 0
    sub x13, x13, 8
    mov x17, 3

_loop:
    add x13, x13, 8
    ldr x14, [x10, x13]
    ldr x15, [x11, x13]
    
    mul x14, x14, x15 
    add x16, x16, x14 

    sub x17, x17, 1
    cmp x17, 0
    b.ne _loop

    str x16, [x12, 0]
    ldr x20, [x12, 0]

    adr x0, format 
    adr x1, vec
    mov x2, x20
    bl printf

    mov x0, 0
    mov x8, 93
    svc 0

.data
vec1: .quad 10, 20, 30
vec2: .quad 1, 2, 3
len: .quad 3
result: .quad 0
s: .asciz "Hello world!\n"
format: .asciz "%s: %d\n"
vec: .asciz "dot product is"
