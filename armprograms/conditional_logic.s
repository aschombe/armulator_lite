.global _start

.text
proc1:
    mov x1, 1
    mov x2, 2
    add x0, x1, x2 
    cmp x0, 3
    b.ne _never
_always:
    mov x0, 0
    ret
_never:
    mov x0, 1
    ldr x0, [x0, 0]
    ret

_start:
    bl proc1
    ret 
    
.data
hello_str: .asciz "Hello, World!\n"
hello_len: .word 14
other_data: .skip 100
arr: .byte 1, 2, 3, 4, 5
arr_len: .word 5
