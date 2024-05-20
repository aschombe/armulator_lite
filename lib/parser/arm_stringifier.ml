open Arm 

let string_of_top_level_directive = function
    | GloblDef _ -> ".globl"
    | ExternSym _ -> ".extern"
    | TextDirect _ -> ".text"
    | DataDirect _ -> ".data"
let ast_string_of_top_level_directive = function 
    | GloblDef _ -> "Arm.GloblDef"
    | ExternSym _ -> "Arm.ExternSym"
    | TextDirect _ -> "Arm.TextDirect"
    | DataDirect _ -> "Arm.DataDirect"

let string_of_data_directive = function
    | Quad _ -> ".quad"
    | Byte _ -> ".byte"
    | QuadArr _ -> ".quad" 
    | ByteArr _ -> ".byte"
    | Word _ -> ".word"
    | WordArr _ -> ".word"
let ast_string_of_data_directive = function
    | Quad _ -> "Arm.Quad"
    | Byte _ -> "Arm.Byte"
    | QuadArr _ -> "Arm.QuadArr"
    | ByteArr _ -> "Arm.ByteArr"
    | Word _ -> "Arm.Word" 
    | WordArr _ -> "Arm.WordArr" 

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
    | Svc -> "svc"
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
    | Svc -> "Arm.Svc"

let string_of_imm = function
    | Lit i -> Int64.to_string i
    | Lbl l -> l
let ast_string_of_imm = function
    | Lit i -> "Arm.Lit(" ^ Int64.to_string i ^ "L)"
    | Lbl l -> "Arm.Lbl(\"" ^ l ^ "\")"

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
    | Offset(Ind1 i) -> "Arm.Offset(Arm.Ind1(Arm.Imm(" ^ (ast_string_of_imm i) ^ ")))"
    | Offset(Ind2 r) -> "Arm.Offset(Arm.Ind2(" ^ (ast_string_of_reg r) ^ "))"
    | Offset(Ind3 (r, i)) -> "Arm.Offset(Arm.Ind3(" ^ (ast_string_of_reg r) ^ ", Arm.Imm(" ^ (ast_string_of_imm i) ^ ")))"

let string_of_insn (op, ops) =
    (string_of_opcode op) ^ " " ^ (String.concat ", " (List.map string_of_operand ops))
let ast_string_of_insn (op, ops) =
    "\t(" ^ (ast_string_of_opcode op) ^ ", [" ^ (String.concat "; " (List.map ast_string_of_operand ops)) ^ "])"

let string_of_data = function
    | Quad q -> ".quad " ^ (Int64.to_string q)
    | Byte c -> ".byte " ^ (Printf.sprintf "0x%02x" c)
    | QuadArr qs -> ".quad " ^ (String.concat ", " (List.map (fun q -> Int64.to_string q) qs))
    | ByteArr cs -> ".byte " ^ (String.concat ", " (List.map (fun co -> Printf.sprintf "0x%02x" co) cs))
    | Word w -> ".word " ^ (Int32.to_string w) 
    | WordArr ws -> ".word " ^ (String.concat ", " (List.map (fun w -> Int32.to_string w) ws))

let ast_string_of_data = function
    | Quad q -> "Arm.Quad(" ^ (Int64.to_string q) ^ "L)"
    | Byte c -> "Arm.Byte(" ^ (Printf.sprintf "'%c'" (Char.chr c)) ^ ")"
    | QuadArr qs -> "Arm.QuadArr([" ^ (String.concat "; " (List.map (fun q -> Int64.to_string q) qs)) ^ "])"
    | ByteArr cs -> "Arm.ByteArr([" ^ (String.concat "; " (List.map (fun co -> Printf.sprintf "%c" (Char.chr co)) cs)) ^ "])"
    | Word w -> "Arm.Word(" ^ (Int32.to_string w) ^ "l)" 
    | WordArr ws -> "Arm.WordArr([" ^ (String.concat "; " (List.map (fun w -> Int32.to_string w) ws)) ^ "])"

let string_of_insn_list insns = String.concat "\n" (List.map string_of_insn insns)
let ast_string_of_insn_list insns = String.concat ";\n" (List.map ast_string_of_insn insns)
let string_of_data_list data = String.concat "\n" (List.map string_of_data data)
let ast_string_of_data_list data = String.concat ";\n" (List.map ast_string_of_data data)

let string_of_block { entry=_; lbl; asm } =
    lbl ^ ":\n"^ (match asm with
        | Text insns -> string_of_insn_list insns
        | Data data -> string_of_data_list data)

let ast_string_of_block { entry=entry; lbl; asm } =
    "{ entry=" ^ string_of_bool entry ^ "; lbl=(\"" ^ lbl ^ "\"); asm=" ^ (match asm with
        | Text insns -> "Arm.Text([\n" ^ (ast_string_of_insn_list insns) ^ "\n])"
        | Data data -> "Arm.Data([\n" ^ (ast_string_of_data_list data) ^ "\n])") ^ "}"

let rec string_of_prog prog =
    match prog with
    | [] -> ""
    | GloblDef s :: tl -> ".globl " ^ s ^ "\n" ^ (string_of_prog tl) 
    | ExternSym s :: tl -> ".extern " ^ s ^ "\n" ^ (string_of_prog tl) 
    | TextDirect blocks :: tl -> ".text\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
    | DataDirect blocks :: tl -> ".data\n" ^ (String.concat "\n" (List.map string_of_block blocks)) ^ "\n" ^ (string_of_prog tl) 
let ast_string_of_prog prog =
  let rec ast_string_of_prog' prog =
    match prog with
    | [] -> ""
    | GloblDef s :: tl -> "Arm.GloblDef(\"" ^ s ^ "\");\n" ^ (ast_string_of_prog' tl) 
    | ExternSym s :: tl -> "Arm.ExternSym(\"" ^ s ^ "\");\n" ^ (ast_string_of_prog' tl) 
    | TextDirect blocks :: tl -> "Arm.TextDirect([\n" ^ (String.concat ";\n" (List.map ast_string_of_block blocks)) ^ "\n]);" ^ (ast_string_of_prog' tl) 
    | DataDirect blocks :: tl -> "Arm.DataDirect([\n" ^ (String.concat ";\n" (List.map ast_string_of_block blocks)) ^ "\n]);" ^ (ast_string_of_prog' tl)
  in 
  "[\n" ^ (ast_string_of_prog' prog) ^ "\n]"

