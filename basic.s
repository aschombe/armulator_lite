.global _start

.text
proc1:
    mov x1, 1
    mov x2, 2
    add x0, x1, x2 
    ret

.text
_start:
    bl proc1
    ret 
