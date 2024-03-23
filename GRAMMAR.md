# CFG For ARM Assembly Language

### program:
| directive\_list

### directive\_list:
| directive
| directive \\n directive\_list

### directive:
| .global label
| .text insn\_list

### insn\_list:
| \\epsilon
| insn
| label: insn\_list
| insn \\n insn\_list

### insn:
| MOV reg, imm
| MOV reg, reg
| ADD reg, reg, imm
| ADD reg, reg, reg
| SUB reg, reg, imm
| SUB reg, reg, reg
| MUL reg, reg, imm
| MUL reg, reg, reg

### reg:
| x0 | x1 | x2 | x3 | x4 | x5 | x6 | x7 | x8 | x9 | x10 | x11 | x12 | x13 | x14 | x15 | x16 | x17 | x18 | x19 | x20 | x21 | x22 | x23 | x24 | x25 | x26 | x27 | x28 | sp | lr | xzr

### imm:
| =label
| #number


