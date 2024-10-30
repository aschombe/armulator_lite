.global _start

.text
proc1:
    mov x19, 1
    ret

_start:
    bl proc1
    
    mov x0, 0
    mov x8, 93 
    svc 0
