.global _start

.text
proc1:
    mov x5, 1
    ret

_start:
    sub sp, sp, 8
    str x5, [sp, 0]
    bl proc1
    // ldr x5, [sp, 0]
    add sp, sp, 8
    
    mov x0, 0
    mov x8, 93 
    svc 0
