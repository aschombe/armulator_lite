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

type offset = 
    | Ind1 of imm (* imm *)
    | Ind2 of reg (* reg *) 
    | Ind3 of reg * imm (* [reg, imm] *) 

type operand =
    | Imm of imm
    | Reg of reg 
    | Offset of offset

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
    | QuadArr of quad list
    | Byte of char 
    | ByteArr of char list
    | String of string
    | Asciz of string

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
    | TextBlock of block list
    | DataBlock of block list

(* a complete program contains a .gloabl, .text, and .data section, but we also need to support external symbols *)
type prog = tld list

let string_of_top_level_directive = function
    | GloblDef _ -> ".globl"
    | ExternSym _ -> ".extern"
    | TextBlock _ -> ".text"
    | DataBlock _ -> ".data"
let ast_string_of_top_level_directive = function 
    | GloblDef _ -> "Arm.Globl"
    | ExternSym _ -> "Arm.Extern"
    | TextBlock _ -> "Arm.Text"
    | DataBlock _ -> "Arm.Data"

let string_of_data_directive = function
    | Quad _ -> ".quad"
    | Byte _ -> ".byte"
    | String _ -> ".string"
    | Asciz _ -> ".asciz"
    | _ -> failwith "not implemented"
let ast_string_of_data_directive = function
    | Quad _ -> "Arm.Quad"
    | Byte _ -> "Arm.Byte"
    | String _ -> "Arm.String"
    | Asciz _ -> "Arm.Asciz"
    | _ -> failwith "not implemented"

let string_of_opcode = function
    | Mov -> "mov" | Adr -> "adr"
    | Ldr -> "ldr" | Str -> "str"
    | Add -> "add" | Sub -> "sub" | Mul -> "mul"
    | And -> "and" | Orr -> "orr" | Lsl -> "lsl" | Lsr -> "lsr" | Asr -> "asr" | Not -> "not"
    | Br -> "br"
    | B c -> "b." ^ (match c with
        | Eq -> "eq"
        | Ne -> "ne"
        | Lt -> "lt"
        | Le -> "le"
        | Gt -> "gt"
        | Ge -> "ge")
    | Cmp -> "cmp" | Cbz -> "cbz" | Cbnz -> "cbnz" | Bl -> "bl" | Ret -> "ret"
let ast_string_of_opcode = function 
    | Mov -> "Arm.Mov" | Adr -> "Arm.Adr"
    | Ldr -> "Arm.Ldr" | Str -> "Arm.Str"
    | Add -> "Arm.Add" | Sub -> "Arm.Sub" | Mul -> "Arm.Mul"
    | And -> "Arm.And" | Orr -> "Arm.Orr" | Lsl -> "Arm.Lsl" | Lsr -> "Arm.Lsr" | Asr -> "Arm.Asr" | Not -> "Arm.Not"
    | Br -> "Arm.Br"
    | B c -> "Arm.B(" ^ (
        match c with
        | Eq -> "Arm.Eq" | Ne -> "Arm.Ne" | Lt -> "Arm.Lt" | Le -> "Arm.Le" | Gt -> "Arm.Gt" | Ge -> "Arm.Ge") ^ ")"
    | Cmp -> "Arm.Cmp" | Cbz -> "Arm.Cbz" | Cbnz -> "Arm.Cbnz" | Bl -> "Arm.Bl" | Ret -> "Arm.Ret"

let string_of_imm = function
    | Lit i -> Int64.to_string i
    | Lbl l -> l
let ast_string_of_imm = function
    | Lit i -> "Arm.Lit(" ^ Int64.to_string i ^ "L)"
    | Lbl l -> "Arm.Lbl(" ^ l ^ ")"

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
let ast_string_of_reg (r : reg) : string =
    let uppercase_reg = String.uppercase_ascii (string_of_reg r) in 
    "Arm." ^ uppercase_reg

let string_of_operand = function
    | Imm i -> string_of_imm i
    | Reg r -> string_of_reg r
    | Offset(Ind1 i) -> "[" ^ (string_of_imm i) ^ "]"
    | Offset(Ind2 r) -> "[" ^ (string_of_reg r) ^ "]"
    | Offset(Ind3 (r, i)) -> "[" ^ (string_of_reg r) ^ ", " ^ (string_of_imm i) ^ "]"
let ast_string_of_operand = function
    | Imm i -> "Arm.Imm(" ^ (ast_string_of_imm i) ^ ")"
    | Reg r -> "Arm.Reg(" ^ (ast_string_of_reg r) ^ ")"
    | Offset(Ind1 i) -> "Arm.Offset(Arm.Ind1(" ^ (ast_string_of_imm i) ^ "))"
    | Offset(Ind2 r) -> "Arm.Offset(Arm.Ind2(" ^ (ast_string_of_reg r) ^ "))"
    | Offset(Ind3 (r, i)) -> "Arm.Offset(Arm.Ind3(" ^ (ast_string_of_reg r) ^ ", " ^ (ast_string_of_imm i) ^ "))"

let string_of_insn (op, ops) =
    "\t" ^ (string_of_opcode op) ^ " " ^ (String.concat ", " (List.map string_of_operand ops))
let ast_string_of_insn (op, ops) =
    "\tArm.Insn(" ^ (ast_string_of_opcode op) ^ ", [" ^ (String.concat "; " (List.map ast_string_of_operand ops)) ^ "])"

let string_of_data = function
    | Quad q -> ".quad " ^ (Int64.to_string q)
    | Byte c -> ".byte " ^ (Char.escaped c)
    | String s -> ".string " ^ s
    | Asciz s -> ".asciz " ^ s 
    | _ -> failwith "not implemented"
let ast_string_of_data = function
    | Quad q -> "Arm.Quad(" ^ (Int64.to_string q) ^ "L)"
    | Byte c -> "Arm.Byte('" ^ (Char.escaped c) ^ "')"
    | String s -> "Arm.String(\"" ^ s ^ "\")"
    | Asciz s -> "Arm.Asciz(\"" ^ s ^ "\")"
    | _ -> failwith "not implemented"

let string_of_insn_list insns = String.concat "\n" (List.map string_of_insn insns)
let ast_string_of_insn_list insns = String.concat "\n" (List.map ast_string_of_insn insns)
let string_of_data_list data = String.concat "\n" (List.map string_of_data data)
let ast_string_of_data_list data = String.concat "\n" (List.map ast_string_of_data data)

let string_of_block { entry=_; lbl; asm } =
    lbl ^ ":\n"^ (match asm with
        | Text insns -> string_of_insn_list insns
        | Data data -> string_of_data_list data)

let ast_string_of_block { entry=entry; lbl; asm } =
    "{ entry=" ^ string_of_bool entry ^ "; lbl=Arm.Lbl(\"" ^ lbl ^ "\"); asm=" ^ (match asm with
        | Text insns -> "Arm.Text([\n" ^ (ast_string_of_insn_list insns) ^ "\n])"
        | Data data -> "Arm.Data([\n" ^ (ast_string_of_data_list data) ^ "\n])") ^ "}"

let rec string_of_prog prog =
    match prog with
    | [] -> ""
    | GloblDef s :: tl -> ".globl " ^ s ^ "\n" ^ (string_of_prog tl) 
    | ExternSym s :: tl -> ".extern " ^ s ^ "\n" ^ (string_of_prog tl) 
    | TextBlock blocks :: tl -> ".text\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
    | DataBlock blocks :: tl -> ".data\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
let rec ast_string_of_prog prog =
    match prog with
    | [] -> ""
    | GloblDef s :: tl -> "Arm.GloblDef(\"" ^ s ^ "\")\n" ^ (ast_string_of_prog tl) 
    | ExternSym s :: tl -> "Arm.ExternSym(\"" ^ s ^ "\")\n" ^ (ast_string_of_prog tl) 
    | TextBlock blocks :: tl -> "Arm.TextBlock([\n" ^ (String.concat ";\n" (List.map ast_string_of_block blocks)) ^ "\n])" ^ (ast_string_of_prog tl) 
    | DataBlock blocks :: tl -> "Arm.DataBlock([\n" ^ (String.concat ";\n" (List.map ast_string_of_block blocks)) ^ "\n])" ^ (ast_string_of_prog tl)
