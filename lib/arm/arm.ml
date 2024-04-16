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
    | Ind1 of imm (* addr + imm *)
    | Ind2 of reg (* addr + reg *)
    | Ind3 of reg * imm (* addr + [reg, imm] *)

type cnd = 
    | Eq | Ne | Lt | Le | Gt | Ge 

type opcode = 
    | Mov | Adr
    | Ldr | Str (* need to add support for ldrb/strb eventually, we can just add new opcodes later *)
    | Add | Sub | Mul 
    | And | Orr | Lsl | Lsr | Asr | Not
    | Br | B of cnd | Cmp | Cbz | Cbnz
    | Bl | Ret 

(* the above should be self explanatory starting from here *)
type insn = opcode * operand list 

(* datatype directives *)
type data = 
    | Quad of quad
    | Byte of char 
    | String of string
    | Asciz of string

type asm = 
    | Text of insn list
    | Data of data list 

(* a block is a sequence of instructions, with an optional label (only in the case of .data) and
   if it is an entry block (_start) or not *)
type block = { entry: bool; lbl: lbl option; asm: asm }

(* top level directives *)
type tld = 
    | Globl of string
    | Extern of string
    | Text of block list
    | Data of block list

(* a complete program contains a .gloabl, .text, and .data section, but we also need to support external symbols *)
type prog = tld list

let string_of_top_level_directive = function
    | Globl _ -> ".globl"
    | Extern _ -> ".extern"
    | Text _ -> ".text"
    | Data _ -> ".data"

let string_of_data_directive = function
    | Quad _ -> ".quad"
    | Byte _ -> ".byte"
    | String _ -> ".string"
    | Asciz _ -> ".asciz"

let string_of_opcode = function
    | Mov -> "mov"
    | Adr -> "adr"
    | Ldr -> "ldr"
    | Str -> "str"
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | And -> "and"
    | Orr -> "orr"
    | Lsl -> "lsl"
    | Lsr -> "lsr"
    | Asr -> "asr"
    | Not -> "not"
    | Br -> "br"
    | B c -> "b." ^ (match c with
        | Eq -> "eq"
        | Ne -> "ne"
        | Lt -> "lt"
        | Le -> "le"
        | Gt -> "gt"
        | Ge -> "ge")
    | Cmp -> "cmp"
    | Cbz -> "cbz"
    | Cbnz -> "cbnz"
    | Bl -> "bl"
    | Ret -> "ret"

let string_of_imm = function
    | Lit i -> Int64.to_string i
    | Lbl l -> l 

let string_of_reg = function
    | X0 -> "x0" | X1 -> "x1" | X2 -> "x2" | X3 -> "x3"
    | X4 -> "x4" | X5 -> "x5" | X6 -> "x6" | X7 -> "x7"
    | X8 -> "x8" | X9 -> "x9"
    | X10 -> "x10" | X11 -> "x11" | X12 -> "x12"
    | X13 -> "x13" | X14 -> "x14" | X15 -> "x15"
    | X16 -> "x16" | X17 -> "x17" | X18 -> "x18"
    | X19 -> "x19" | X20 -> "x20" | X21 -> "x21"
    | X22 -> "x22" | X23 -> "x23" | X24 -> "x24"
    | X25 -> "x25" | X26 -> "x26" | X27 -> "x27"
    | X28 -> "x28"
    | SP -> "sp" | LR -> "lr" | XZR -> "xzr"

let string_of_operand = function
    | Imm i -> "#" ^ (string_of_imm i)
    | Reg r -> string_of_reg r
    | Ind1 i -> "[" ^ (string_of_imm i) ^ "]"
    | Ind2 r -> "[" ^ (string_of_reg r) ^ "]"
    | Ind3 (r, i) -> "[" ^ (string_of_reg r) ^ ", " ^ (string_of_imm i) ^ "]"

let string_of_insn (op, ops) =
    (string_of_opcode op) ^ " " ^ (String.concat ", " (List.map string_of_operand ops))

let string_of_data = function
    | Quad q -> ".quad " ^ (Int64.to_string q)
    | Byte c -> ".byte " ^ (Char.escaped c)
    | String s -> ".string " ^ s
    | Asciz s -> ".asciz " ^ s 

let string_of_insn_list insns = String.concat "\n" (List.map string_of_insn insns)
let string_of_data_list data = String.concat "\n" (List.map string_of_data data)

let string_of_block { entry=_; lbl; asm } =
    let lbl_str = match lbl with
        | None -> ""
        | Some l -> l ^ ":\n" in 
    lbl_str ^ (match asm with
        | Text insns -> string_of_insn_list insns
        | Data data -> string_of_data_list data)

let rec string_of_prog prog =
    match prog with
    | [] -> ""
    | Globl s :: tl -> ".globl " ^ s ^ "\n" ^ (string_of_prog tl) 
    | Extern s :: tl -> ".extern " ^ s ^ "\n" ^ (string_of_prog tl) 
    | Text blocks :: tl -> ".text\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
    | Data blocks :: tl -> ".data\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
