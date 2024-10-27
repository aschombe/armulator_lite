.text
.global _start

_start:
    mov x0, 0
    adr x1, buffer
    mov x2, 15
    mov x8, 63
    svc 0

    mov x0, 1
    adr x1, buffer
    mov x2, 15
    mov x8, 64
    svc 0

    mov x0, 0
    mov x8, 93 
    svc 0 

.data
    buffer: .skip 100