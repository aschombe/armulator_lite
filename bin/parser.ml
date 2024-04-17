exception Syntax_error of string 

let arm_error (ln : int) (line : string) (msg : string) : 'a =
  let msg = Printf.sprintf "\nSyntax error on line %d: %s\n\n\t%s\n\n" ln msg line in
  raise (Syntax_error msg)

let print_lines (lines : string list) : unit =
  List.iter (fun x -> print_endline x) lines

let is_empty (line : string) : bool = 
  let line = String.trim line in
  String.equal String.empty line

let is_directive (line : string) : bool = String.starts_with line ~prefix:"."
let has_label (line : string) : bool = String.contains line ':'
let is_digit (c : char) : bool = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
let is_number (n : string) : bool =  (* starts with #, or starts with a digit, 0x, 0b, 0o *)
  let tn = String.trim n in
  let starts_with_pound = String.starts_with tn ~prefix:"#" in 
  let starts_with_digit = is_digit (String.get tn 0) in 
  let starts_with_0x = String.starts_with tn ~prefix:"0x" in 
  let starts_with_0b = String.starts_with tn ~prefix:"0b" in 
  let starts_with_0o = String.starts_with tn ~prefix:"0o" in 
  starts_with_pound || starts_with_digit || starts_with_0x || starts_with_0b || starts_with_0o

let find_directives (lines : (int * string) list) (d_name : string) : (int * string) list list = 
  let rec fdh (l: (int * string) list) (d: string) (active: bool) (acc : (int * string) list list) (cur : (int * string) list) : (int * string) list list =
    match l with
    | [] -> acc @ [cur]
    | (i, h)::t -> 
      if is_directive h then
        if active then 
          fdh t d true (acc @ [cur]) []
        else if String.starts_with h ~prefix:("." ^ d) then
          fdh t d true acc [(i, h)]
        else
          fdh t d false acc cur
      else if active then
        fdh t d true acc (cur @ [(i, h)])
      else
        fdh t d false acc cur
  in 
  fdh lines d_name false [] []

let find_defs (lines : (int * string) list) (d_name : string) : (int * string) list = 
  let rec fdh (l: (int * string) list) (d: string) (acc : (int * string) list) : (int * string) list =
    match l with
    | [] -> acc
    | (i, h)::t -> 
      if is_directive h then
        if String.starts_with h ~prefix:("." ^ d) then
          let defname = List.nth (String.split_on_char ' ' h) 1 in
          fdh t d acc @ [(i, defname)]
        else
          fdh t d acc
      else
        fdh t d acc
  in fdh lines d_name []

let find_blocks (lines : (int * string) list) : (int * string) list list = 
  let rec fbh (l: (int * string) list) (active: bool) (acc : (int * string) list list) (cur : (int * string) list) : (int * string) list list =
    match l with
    | [] -> acc @ [cur]
    | (i, h)::t -> 
      if has_label h then
        if active then
          fbh t true (acc @ [cur]) [(i, h)]
        else
          fbh t true acc [(i, h)]
      else if active then
        fbh t true acc (cur @ [(i, h)])
      else
        fbh t false acc cur
  in 
  fbh lines false [] []

let parse_label ((ln, line): (int * string)) : (string * (int * string)) = 
  let line = String.trim line in
  let idx = String.index_opt line ':' in
  let lbl = match idx with
  | Some i -> String.sub line 0 i 
  | None -> arm_error ln line "Could not parse label!" in 
  let rest = match idx with
  | Some i -> String.sub line (i + 1) ((String.length line) - i - 1) 
  | None -> arm_error ln line "Could not parse label!" in
  (lbl, (ln, rest))

let opcode_of_string ((ln, insn) : (int * string)) (mnemonic : string) : Arm.opcode = 
  let partial = String.split_on_char '.' mnemonic |> List.hd in
  match partial with
  | "mov" -> Arm.Mov | "adr" -> Arm.Adr 
  | "ldr" -> Arm.Ldr | "str" -> Arm.Str 
  | "add" -> Arm.Add| "sub" -> Arm.Sub  | "mul" -> Arm.Mul 
  | "and" -> Arm.And | "orr" -> Arm.Orr  | "lsl" -> Arm.Lsl | "lsr" -> Arm.Lsr  | "asr" -> Arm.Asr | "not" -> Arm.Not
  | "b" -> 
      let cnd_code_mnemonic = List.nth (String.split_on_char '.' mnemonic) 1 in 
      let cnd_code = (match cnd_code_mnemonic with 
      | "eq" -> Arm.Eq 
      | "ne" -> Arm.Ne 
      | "lt" -> Arm.Lt
      | "le" -> Arm.Le
      | "gt" -> Arm.Gt 
      | "ge" -> Arm.Ge
      | _ -> arm_error ln insn ("Invalid condition code '" ^ cnd_code_mnemonic ^ "'")
      ) 
      in Arm.B cnd_code
  | "cmp" -> Arm.Cmp | "cbz" -> Arm.Cbz  | "cbnz" -> Arm.Cbnz 
  | "bl" -> Arm.Bl  | "ret" -> Arm.Ret 
  | _ -> arm_error ln insn ("Invalid mnemonic '" ^ mnemonic ^ "'")

let register_of_string ((ln, insn) : (int * string)) (reg : string) : Arm.reg = 
  match reg with
  | "x0" -> Arm.X0 | "x1" -> Arm.X1 | "x2" -> Arm.X2 | "x3" -> Arm.X3
  | "x4" -> Arm.X4 | "x5" -> Arm.X5 | "x6" -> Arm.X6 | "x7" -> Arm.X7 
  | "x8" -> Arm.X8 | "x9" -> Arm.X9
  | "x10" -> Arm.X10 | "x11" -> Arm.X11 | "x12" -> Arm.X12 | "x13" -> Arm.X13 
  | "x14" -> Arm.X14 | "x15" -> Arm.X15 | "x16" -> Arm.X16 | "x17" -> Arm.X17 
  | "x18" -> Arm.X18 | "x19" -> Arm.X19 | "x20" -> Arm.X20 | "x21" -> Arm.X21 
  | "x22" -> Arm.X22 | "x23" -> Arm.X23 | "x24" -> Arm.X24 | "x25" -> Arm.X25 
  | "x26" -> Arm.X26 | "x27" -> Arm.X27 | "x28" -> Arm.X28 | "x29" -> Arm.SP 
  | "x30" -> Arm.LR | "x31" -> Arm.XZR 
  | "sp" -> Arm.SP | "lr" -> Arm.LR | "xzr" -> Arm.XZR
  | _ -> arm_error ln insn ("Invalid register '" ^ reg ^ "'")
let is_not_register (r : string) : bool = 
  try 
    let _ = register_of_string (0, "") r in false 
  with _ -> true

let imm_of_string (imm : string) : Arm.imm = 
  if is_number imm then 
    Arm.Lit (Int64.of_string imm)
  else 
    Arm.Lbl imm

let offset_of_string ((ln, insn) : (int * string)) (offset : string list) : Arm.offset = 
  if List.length offset = 1 then
    (* handle imm and reg single case *)
    let imm = List.nth offset 0 in
    if is_number imm then 
      Arm.Ind1(Arm.Lit(Int64.of_string imm))
    else 
      Arm.Ind2(register_of_string (ln, insn) imm)
  else 
    let reg = List.nth (String.split_on_char '[' (List.nth offset 0)) 1 in
    let imm = List.nth (String.split_on_char ']' (List.nth offset 1)) 0 in
    Arm.Ind3(register_of_string (ln, insn) reg, Arm.Lit(Int64.of_string imm))

let operand_of_string ((ln, insn) : (int * string)) (operand : string) : Arm.operand = 
  if is_number operand || is_not_register operand then 
    Arm.Imm (imm_of_string operand)
  else 
    Arm.Reg (register_of_string (ln, insn) operand)

let operands_of_tokens ((ln, insn) : (int * string)) (args : string list) : Arm.operand list = 
  match args with
  | [] -> []
  | a1::[] -> [operand_of_string (ln, insn) a1]
  | a1::a2::[] -> [operand_of_string (ln, insn) a1; operand_of_string (ln, insn) a2]
  | a1::a2::a3::[] -> 
      if String.contains a2 '[' && String.contains a2 ']' then
        [operand_of_string (ln, insn) a1; Arm.Offset(offset_of_string (ln, insn) [a2; a3])]
      else
        [operand_of_string (ln, insn) a1; operand_of_string (ln, insn) a2; operand_of_string (ln, insn) a3]
  | _ -> raise (Invalid_argument "Invalid number of operands")

let tokenize_insn ((ln, line) : (int * string)) : ((int * string) * string list) = 
  if is_empty line then
    ((ln, line), [])
  else
    let line = String.trim line in
    let parts = String.split_on_char ' ' line in
    let no_commas = List.map (fun x -> String.split_on_char ',' x) parts |> List.flatten in
    let no_empty_strs = List.filter (fun x -> not (String.equal x "")) no_commas in 
    ((ln, line), no_empty_strs)

let parse_insn ((ln, insn) : (int * string)) (tokens : string list) : Arm.insn =
  if List.length tokens = 0 then
    arm_error ln "" "Empty instruction!"
  else
    let mnemonic = List.nth tokens 0 in
    let opcode = opcode_of_string (ln, insn) mnemonic in
    let operands = operands_of_tokens (ln, insn) (List.tl tokens) in
    (opcode, operands)

let parse_text_block (lines : (int * string) list) : Arm.block = 
  let (label, rest) = parse_label (List.hd lines) in
  let removed_lines = rest :: (List.tl lines) in
  let lbl_opt = if String.equal label "" then None else Some(label) in
  let tokenized_lines = List.map (fun insn_line -> tokenize_insn insn_line) removed_lines in
  let no_empty_lines = List.filter (fun (_, tokens) -> not (List.length tokens = 0)) tokenized_lines in
  let insns = List.map (fun ((ln, insn), tokens) -> parse_insn (ln, insn) tokens) no_empty_lines in
  let is_entry = String.equal label "_start" in
  {entry=is_entry; lbl=lbl_opt; asm=Arm.IText(insns)}

let transform_global_defs (defs : (int * string) list) : Arm.tld list = 
  List.map (fun (_, x) -> Arm.Globl(x)) defs

let transform_extern_defs (defs : (int * string) list) : Arm.tld list =
  List.map (fun (_, x) -> Arm.Extern(x)) defs

let parse_assembly (lines : (int * string) list) : Arm.prog =
  let global_defs =
    (find_defs lines "global" |> transform_global_defs) @
    (find_defs lines "globl" |> transform_global_defs)
  in
  let extern_defs = find_defs lines "extern" |> transform_extern_defs in
  let text_directives = List.concat (find_directives lines "text") in
  let text_blocks = find_blocks text_directives in 
  let text_blocks_parsed = Arm.Text(List.map (fun x -> parse_text_block x) text_blocks) in 
  extern_defs @ global_defs @ [text_blocks_parsed]


