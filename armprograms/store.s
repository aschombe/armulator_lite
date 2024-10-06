.global _start
.extern printf

.text
_start:
    adr x0, dat
    mov x1, 20
    str x1, [x0, 0]

    ldr x2, [x0, 0]

    mov x0, 0
    mov x8, 93
    svc 0

.data
dat: .quad 10