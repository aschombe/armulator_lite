(* types for arm-lite assembly *)
(* heavily derived from the compilers course at stevens *)

type lbl = string
type quad = int64
type reg = 
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8
    | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16
    | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 
    | X25 | X26 | X27 | X28 | SP | LR | XZR
type imm =
    | Lit of quad
    | Lbl of lbl
    | Ind1 of imm 
    | Ind2 of reg 
    | Ind3 of reg * imm

type operand = 
    | Reg of reg
    | Imm of imm
    | Label of lbl

type cnd = 
    | Eq | Ne | Lt | Le | Gt | Ge 

type opcode = 
    | Mov | Adr | Ldr | Str
    | Add | Sub | Mul 
    | And | Orr | Lsl | Lsr | Asr | Not
    | Br | B of cnd | Cmp | Cbz | Cbnz
    | Bl | Ret 

type insn = lbl option * (opcode * operand list)

type directive = 
    | Global of lbl
    | Text of insn list

type prog = directive list

let reg_of_string = function
    | "x0" -> X0 | "x1" -> X1 | "x2" -> X2 | "x3" -> X3
    | "x4" -> X4 | "x5" -> X5 | "x6" -> X6 | "x7" -> X7
    | "x8" -> X8 | "x9" -> X9 | "x10" -> X10 | "x11" -> X11 
    | "x12" -> X12 | "x13" -> X13 | "x14" -> X14 | "x15" -> X15 
    | "x16" -> X16 | "x17" -> X17 | "x18" -> X18 | "x19" -> X19 
    | "x20" -> X20 | "x21" -> X21 | "x22" -> X22 | "x23" -> X23 
    | "x24" -> X24 | "x25" -> X25 | "x26" -> X26 | "x27" -> X27 
    | "x28" -> X28 | "sp" -> SP | "lr" -> LR | "xzr" -> XZR 
    | "x29" -> SP | "x30" -> LR | "x31" -> XZR
    | _ -> failwith "invalid register"

let string_of_reg = function
    | X0 -> "x0" | X1 -> "x1" | X2 -> "x2" | X3 -> "x3"
    | X4 -> "x4" | X5 -> "x5" | X6 -> "x6" | X7 -> "x7"
    | X8 -> "x8" | X9 -> "x9" | X10 -> "x10" | X11 -> "x11"
    | X12 -> "x12" | X13 -> "x13" | X14 -> "x14" | X15 -> "x15"
    | X16 -> "x16" | X17 -> "x17" | X18 -> "x18" | X19 -> "x19"
    | X20 -> "x20" | X21 -> "x21" | X22 -> "x22" | X23 -> "x23"
    | X24 -> "x24" | X25 -> "x25" | X26 -> "x26" | X27 -> "x27"
    | X28 -> "x28" | SP -> "sp" | LR -> "lr" | XZR -> "xzr"

let string_of_operand (op: operand) : string =
    match op with
    | Reg r -> string_of_reg r
    | Imm (Lit i) -> Int64.to_string i
    | Imm (Lbl l) -> l
    | Label l -> l
    | _ -> failwith "invalid operand"

let rec string_of_oplist (opl: operand list) : string = 
    match opl with
    | [] -> ""
    | h::t -> string_of_operand h ^ ", " ^ string_of_oplist t

let string_of_opcode (op: opcode) : string =
    match op with
    | Mov -> "mov" | Adr -> "adr" | Ldr -> "ldr" | Str -> "str"
    | Add -> "add" | Sub -> "sub" | Mul -> "mul"
    | And -> "and" | Orr -> "orr" | Lsl -> "lsl" | Lsr -> "lsr" | Asr -> "asr" | Not -> "not"
    | Br -> "br" | B c -> "b" ^ (match c with | Eq -> "eq" | Ne -> "ne" | Lt -> "lt" | Le -> "le" | Gt -> "gt" | Ge -> "ge")
    | Cmp -> "cmp" | Cbz -> "cbz" | Cbnz -> "cbnz"
    | Bl -> "bl" | Ret -> "ret"

let string_of_insn ((op, opl): opcode * operand list) : string = string_of_opcode op ^ " " ^ string_of_oplist opl ^ "\n"
let string_of_linsn ((l, i): insn) : string = 
    match l with
    | None -> string_of_insn i
    | Some l -> l ^ ": " ^ string_of_insn i

let rec string_of_insns (il: insn list) : string =
    match il with 
    | [] -> ""
    | h::t -> string_of_linsn h ^ "\n" ^ string_of_insns t

let rec string_of_prog (pr: prog) : string =
    match pr with
    | [] -> ""
    | Global lbl :: ts -> ".global " ^ lbl ^ "\n" ^ string_of_prog ts
    | Text il :: ts -> ".text\n" ^ string_of_insns il ^ string_of_prog ts
