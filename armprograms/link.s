.global hello_world

.text
hello_world:
    mov x0, 1
    mov x1, s
    adr x2, l
    ldr x2, [x2, 0]
    svc 0
    
    ret

.data
s: .asciz "Hello world!\n"
l: .quad 14