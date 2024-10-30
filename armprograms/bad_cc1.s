.global _start

.text
_start:
    sub sp, sp, 8
  
    mov x0, 0
    mov x8, 93 
    svc 0