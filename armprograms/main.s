.global _start
.extern printf
.extern hello_world

.text
_start:
    bl hello_world

    mov x0, 0
    mov x8, 93 
    svc 0