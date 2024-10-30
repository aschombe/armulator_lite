.global _start

.text
proc1:
    sub sp, sp, 8
    str x19, [sp, 0]
    mov x19, 1
    ldr x19, [sp, 0]
    add sp, sp, 8
    ret

_start:
    bl proc1
    
    mov x0, 0
    mov x8, 93 
    svc 0
