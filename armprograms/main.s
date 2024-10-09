.global _start
.extern hello_world
.extern printf

.text
_start:
    bl hello_world

    mov x0, 0
    mov x8, 93 
    svc 0