open Str

exception Syntax_error
exception Ok

type code_line = int * string

let arm_error (ln : int) (line : string) (highlight : string) (msg : string) : 'a =
  if String.equal highlight String.empty || ln == 0 then 
    raise Ok
  else
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") line in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mon line \x1b[1;97m%d\x1b[0m:" ln in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (Syntax_error)

let print_code_lines (lines : code_line list) : unit =
  List.iter (fun (_, line) -> print_endline line) lines

(* generic parse helpers *)
let is_empty (line : string) : bool = 
  let line = String.trim line in
  String.equal String.empty line
let tokenize ((ln, line) : code_line) : (code_line * string list) = 
  if is_empty line then
    ((ln, line), [])
  else
    let line = String.trim line in
    let parts = String.split_on_char ' ' line in
    let no_commas = List.map (fun x -> String.split_on_char ',' x) parts |> List.flatten in
    let no_empty_strs = List.filter (fun x -> not (String.equal x "")) no_commas in 
    ((ln, line), no_empty_strs)
let is_tld (line : string) : bool = String.starts_with line ~prefix:"."
let is_lbl_def (line : string) : bool = String.contains line ':'
let is_digit (c : char) : bool = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
let is_number (n : string) : bool =  (* starts with #, or starts with a digit, 0x, 0b, 0o *)
  let tn = String.trim n in
  let starts_with_pound = String.starts_with tn ~prefix:"#" in 
  let starts_with_digit = is_digit (String.get tn 0) in 
  let starts_with_0x = String.starts_with tn ~prefix:"0x" in 
  let starts_with_0b = String.starts_with tn ~prefix:"0b" in 
  let starts_with_0o = String.starts_with tn ~prefix:"0o" in 
  starts_with_pound || starts_with_digit || starts_with_0x || starts_with_0b || starts_with_0o
let parse_label ((ln, line): code_line) : (string * code_line) = 
  let line = String.trim line in
  let idx = String.index_opt line ':' in
  let lbl = match idx with
  | Some i -> String.sub line 0 i 
  | None -> arm_error ln line "" "Could not parse label!" in 
  let rest = match idx with
  | Some i -> String.sub line (i + 1) ((String.length line) - i - 1) 
  | None -> arm_error ln line "" "Could not parse label!" in
  (lbl, (ln, rest))

let parse_string ((ln, line): code_line) : (string * code_line) = 
  let regex = Str.regexp {|\(["']\)(?:(?=(\\?))\2.)*?\1|} in
  let () = if not (Str.string_match regex line 0) then arm_error ln line line "Invalid string" in 
  let str = Str.matched_string line in 
  let rest = Str.string_after str 1 in 
  let rest = Str.string_before rest ((String.length rest) - 1) in 
  (rest, (ln, rest)) 

(* directive parsing *)

let find_directives (lines : code_line list) (d_name : string) : code_line list list = 
  let rec fdh (l: code_line list) (d: string) (active: bool) (acc : code_line list list) (cur : code_line list) : code_line list list =
    match l with
    | [] -> acc @ [cur]
    | (i, h)::t -> 
      if is_tld h then
        if String.starts_with h ~prefix:("." ^ d) then
          fdh t d true (acc @ [cur]) []
        else
          fdh t d false acc cur
      else if active then
        fdh t d true acc (cur @ [(i, h)])
      else
        fdh t d false acc cur
  in 
  fdh lines d_name false [] []

let find_blocks (lines : code_line list) : code_line list list = 
  let rec fbh (l: code_line list) (active: bool) (acc : code_line list list) (cur : code_line list) : code_line list list =
    match l with
    | [] -> acc @ [cur]
    | (i, h)::t -> 
      if is_lbl_def h then
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

(* definition parsing (global, extern) *)
let find_defs (lines : code_line list) (d_name : string) : code_line list = 
  let rec fdh (l: code_line list) (d: string) (acc : code_line list) : code_line list =
    match l with
    | [] -> acc
    | (i, h)::t -> 
      if is_tld h then
        if String.starts_with h ~prefix:("." ^ d) then
          let defname = List.nth (String.split_on_char ' ' h) 1 in
          fdh t d acc @ [(i, defname)]
        else
          fdh t d acc
      else
        fdh t d acc
  in fdh lines d_name []

let transform_global_defs (defs : code_line list) : Arm.tld list = 
  List.map (fun (_, x) -> Arm.GloblDef(x)) defs

let transform_extern_defs (defs : code_line list) : Arm.tld list =
  List.map (fun (_, x) -> Arm.ExternSym(x)) defs


(* instruction parsing *)
let opcode_of_string ((ln, insn) : code_line) (mnemonic : string) : Arm.opcode = 
  let pieces = String.split_on_char '.' mnemonic in
  let partial = List.nth pieces 0 in 
  let cnd_code_mnemoic = (if List.length pieces > 1 then List.nth pieces 1 else "") in
  match partial with
  | "mov" -> Arm.Mov | "adr" -> Arm.Adr 
  | "ldr" -> Arm.Ldr | "str" -> Arm.Str 
  | "add" -> Arm.Add| "sub" -> Arm.Sub  | "mul" -> Arm.Mul 
  | "and" -> Arm.And | "orr" -> Arm.Orr  | "lsl" -> Arm.Lsl | "lsr" -> Arm.Lsr  | "asr" -> Arm.Asr | "not" -> Arm.Not
  | "b" -> (
    try
      let cnd_code_mnemonic = List.nth (String.split_on_char '.' mnemonic) 1 in 
      let cnd_code = (match cnd_code_mnemonic with 
      | "eq" -> Arm.Eq 
      | "ne" -> Arm.Ne 
      | "lt" -> Arm.Lt
      | "le" -> Arm.Le
      | "gt" -> Arm.Gt 
      | "ge" -> Arm.Ge
      | _ -> raise Syntax_error) in Arm.B cnd_code
    with _ -> arm_error ln insn cnd_code_mnemoic "Invalid condition code")
  | "cmp" -> Arm.Cmp | "cbz" -> Arm.Cbz  | "cbnz" -> Arm.Cbnz 
  | "bl" -> Arm.Bl  | "ret" -> Arm.Ret 
  | _ -> arm_error ln insn mnemonic "Invalid mnemonic"

let register_of_string ((ln, insn) : code_line) (reg : string) : Arm.reg = 
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
  | _ -> arm_error ln insn reg "Invalid register"
let is_not_register (r : string) : bool = 
  try 
    let _ = register_of_string (0, "") r in false 
  with _ -> true

let imm_of_string ((ln, insn) : code_line) (imm : string) : Arm.imm = 
  if is_number imm then 
    try
      Arm.Lit (Int64.of_string imm)
    with _ -> arm_error ln insn imm "Invalid immediate"
  else 
    Arm.Lbl imm

let offset_of_string ((ln, insn) : code_line) (offset : string list) : Arm.offset = 
  if List.length offset = 1 then
    (* handle imm and reg single case *)
    let imm = List.nth offset 0 in
    if is_number imm then 
      Arm.Ind1(Arm.Lit(Int64.of_string imm))
    else 
      Arm.Ind2(register_of_string (ln, insn) imm)
  else 
    let reg = List.nth offset 0 in
    let imm = List.nth offset 1 in
    Arm.Ind3(register_of_string (ln, insn) reg, imm_of_string (ln, insn) imm)

let operand_of_string ((ln, insn) : code_line) (operand : string) : Arm.operand = 
  if is_number operand || is_not_register operand then 
    Arm.Imm (imm_of_string (ln, insn) operand)
  else 
    Arm.Reg (register_of_string (ln, insn) operand)

let operands_of_tokens ((ln, insn) : code_line) (args : string list) : Arm.operand list = 
  match args with
  | [] -> []
  | a1::[] -> [operand_of_string (ln, insn) a1]
  | a1::a2::[] -> [operand_of_string (ln, insn) a1; operand_of_string (ln, insn) a2]
  | a1::a2::a3::[] -> 
      if String.contains a2 '[' && String.contains a3 ']' then
        let fixed_a2 = List.nth (String.split_on_char '[' a2) 1 in 
        let fixed_a3 = List.nth (String.split_on_char ']' a3) 0 in
        [operand_of_string (ln, insn) a1; Arm.Offset(offset_of_string (ln, insn) [fixed_a2; fixed_a3])]
      else
        [operand_of_string (ln, insn) a1; operand_of_string (ln, insn) a2; operand_of_string (ln, insn) a3]
  | _ -> raise (Invalid_argument "Invalid number of operands")

let parse_insn ((ln, insn) : code_line) (tokens : string list) : Arm.insn =
  if List.length tokens = 0 then
    arm_error ln insn insn "Empty instruction!"
  else
    let mnemonic = List.nth tokens 0 in
    let opcode = opcode_of_string (ln, insn) mnemonic in
    let operands = operands_of_tokens (ln, insn) (List.tl tokens) in
    (opcode, operands)

let parse_text_block (lines : code_line list) : Arm.block = 
  let (label, rest) = parse_label (List.hd lines) in
  let removed_lines = rest :: (List.tl lines) in
  let tokenized_lines = List.map (fun insn_line -> tokenize insn_line) removed_lines in
  let no_empty_lines = List.filter (fun (_, tokens) -> not (List.length tokens = 0)) tokenized_lines in
  let insns = List.map (fun ((ln, insn), tokens) -> parse_insn (ln, insn) tokens) no_empty_lines in
  let is_entry = String.equal label "_start" in
  {entry=is_entry; lbl=label; asm=Arm.Text(insns)}

(* data segment parsing *)
let string_to_bytes (s : string) : int list = 
  let rec stbh (s : string) (acc : int list) (idx : int) : int list = 
    if idx = String.length s then acc
    else stbh s (acc @ [Char.code (String.get s idx)]) (idx + 1)
  in stbh s [] 0

let parse_quad ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty quad!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      Arm.Quad(Int64.of_string imm)
    else
      arm_error ln line imm "Invalid quad"

let parse_quad_arr ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty quad array!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      let quads = List.map (fun x -> Int64.of_string x) tokens in
      Arm.QuadArr(quads)
    else
      arm_error ln line imm "Invalid quad array"

let parse_ddef ((ln, line) : code_line) (tokens : string list) : Arm.data =
  failwith "Not implemented"

let parse_data_block (lines : code_line list) : Arm.block =
  let (label, rest) = parse_label (List.hd lines) in
  let removed_lines = rest :: (List.tl lines) in
  let tokenized_lines = List.map (fun insn_line -> tokenize insn_line) removed_lines in
  let no_empty_lines = List.filter (fun (_, tokens) -> not (List.length tokens = 0)) tokenized_lines in
  let data = List.map (fun ((ln, insn), tokens) -> parse_ddef (ln, insn) tokens) no_empty_lines in
  {entry=false; lbl=label; asm=Arm.Data(data)}

(* assembly parsing *)

let parse_assembly (lines : code_line list) : Arm.prog =
  let global_defs =
    (find_defs lines "global" |> transform_global_defs) @
    (find_defs lines "globl" |> transform_global_defs)
  in
  let extern_defs = find_defs lines "extern" |> transform_extern_defs in
  let text_directives = List.concat (find_directives lines "text") in
  let text_blocks = find_blocks text_directives in 
  let text_blocks_parsed = Arm.TextBlock(List.map (fun x -> parse_text_block x) text_blocks) in 
  extern_defs @ global_defs @ [text_blocks_parsed]

