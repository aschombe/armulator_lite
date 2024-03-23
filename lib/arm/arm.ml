(* types for arm-lite assembly *)
(* heavily derived from the compilers course at stevens *)

type lbl = string
type quad = int64
type imm =
    | Lit of quad
    | Lbl of lbl

type reg = 
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8
    | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16
    | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 
    | X25 | X26 | X27 | X28 | SP | LR | XZR

type operand =
    | Imm of imm
    | Reg of reg 
    | Ind1 of imm 
    | Ind2 of reg 
    | Ind3 of reg * imm

type cnd = 
    | Eq | Ne | Lt | Le | Gt | Ge 

type opcode = 
    | Mov | Adr | Ldr | Str
    | Add | Sub | Mul 
    | And | Orr | Lsl | Lsr | Asr | Not
    | Br | B of cnd | Cmp | Cbz | Cbnz
    | Bl | Ret 

type ins = opcode * operand list 

type data = 
    | Quad of quad
    | Byte of char 
    | String of string
    | Asciz of string

type asm = 
    | Text of ins list
    | Data of data list 

type block = { lbl: lbl; entry: bool; asm: asm }

type prog = block list

module Asm = struct 
    let data l ds = { lbl = l; entry = true; asm = Data ds }
    let text l is = { lbl = l; entry = false; asm = Text is }
    let glob l is = { lbl = l; entry = true; asm = Text is }
end
