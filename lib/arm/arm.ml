(* types for arm-lite assembly *)
(* heavily derived from the compilers course at stevens *)

type lbl = string
type quad = int64
type word = int32
type imm =
    | Lit of quad
    | Lbl of lbl

type reg = 
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8
    | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16
    | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 
    | X25 | X26 | X27 | X28 | SP | LR | XZR
    | W0 | W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8
    | W9 | W10 | W11 | W12 | W13 | W14 | W15 | W16
    | W17 | W18 | W19 | W20 | W21 | W22 | W23 | W24 
    | W25 | W26 | W27 | W28 | W29 | W30

type offset = 
    | Ind1 of imm (* imm *)
    | Ind2 of reg (* reg *) 
    | Ind3 of reg * imm (* [reg, imm] *) 

type operand =
    | Imm of imm
    | Reg of reg 
    | Offset of offset

type cnd = 
    | Al (* always, added this as a background thing *)
    | Eq | Ne | Lt | Le | Gt | Ge 

type opcode = 
    | Mov | Adr
    | Ldr | Ldrb | Str | Strb (* need to add support for ldrb/strb eventually, we can just add new opcodes later *)
    | Add | Sub | Mul 
    | Adds | Subs | Muls 
    | And | Orr | Lsl | Lsr | Asr | Not
    | Ands | Orrs | Lsls | Lsrs | Asrs | Nots 
    | B of cnd | Cmp | Cbz | Cbnz
    | Bl | Ret 
    | Svc

(* the above should be self explanatory starting from here *)
type insn = opcode * operand list 

(* datatype directives *)
type data = 
    | Quad of quad
    | QuadArr of quad list
    | Byte of int
    | ByteArr of int list
    | Word of word 
    | WordArr of word list 

type asm = 
    | Text of insn list
    | Data of data list 

(* a block is a sequence of instructions, with an optional label (only in the case of .data) and
   if it is an entry block (_start) or not *)
type block = { entry: bool; lbl: lbl; asm: asm }

(* top level directives *)
type tld = 
    | GloblDef of string
    | ExternSym of string
    | TextDirect of block list
    | DataDirect of block list

(* a complete program contains a .global, .text, and .data section, but we also need to support external symbols *)
type prog = tld list
