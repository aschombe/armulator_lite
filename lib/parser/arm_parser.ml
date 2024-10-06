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

(* string parsing *)
(* result, errinfo, rest of string *)
let extract_string ((ln, line): code_line) : (string * code_line * string) = 
  let char_list = String.to_seq line |> List.of_seq in
  let rec esh ((ln, line): code_line) (chars: char list) (is_in_str: bool) (did_match: bool) (matched: string) (rest: string): (string * string) =
    match chars with 
    | [] -> (matched, rest)
    | h::[] -> 
        if is_in_str then
          match h with
          | '"' -> (matched, rest)
          | _ -> arm_error ln line "" "Invalid string"
        else 
          if did_match then (matched, rest ^ String.make 1 h)
          else (matched, rest)
    | h::la::t -> 
        if is_in_str then
          match h with
          | '"' -> esh (ln, line) t false true matched rest
          | '\\' -> esh (ln, line) t true false (matched ^ String.make 1 h ^ String.make 1 la) rest
          | _ -> esh (ln, line) (la::t) true false (matched ^ String.make 1 h) rest
        else 
          if h = '"' then esh (ln, line) (la::t) true false matched rest
          else esh (ln, line) (la::t) false did_match matched rest
  in
  let (m, r) = esh (ln, line) char_list false false "" "" in
  (m, (ln, line), r)

let extract_all_strings (cline : code_line) : (string * (string * code_line)) list =
  if not (String.contains (snd cline) '"') then []
  else
    let gensym (i : int) : string = Printf.sprintf "__strtok_%d" i in
    let rec eash (found : int) ((ln, line): code_line) : (string * (string * code_line)) list =
      if not (String.contains line '"') then []
      else
        let (s, cln, r) = extract_string (ln, line) in
        let sym = gensym found in
        if String.equal r "" then [(sym, (s, cln))]
        else (sym, (s, cln)) :: eash (found + 1) (ln, r)
    in eash 0 cline

let print_code_lines (lines : code_line list) : unit =
  List.iter (fun (_, line) -> print_endline line) lines

(* generic parse helpers *)
let is_empty (line : string) : bool = 
  let line = String.trim line in
  String.equal String.empty line
let string_replace str pattern replacement =
  let len = String.length pattern in
  let rec replace str start =
    try
      let idx = String.index_from str start pattern.[0] in
      if String.sub str idx len = pattern then
        let prefix = String.sub str 0 idx in
        let suffix = String.sub str (idx + len) (String.length str - idx - len) in
        replace (prefix ^ replacement ^ suffix) (idx + len)
      else
        replace str (idx + 1)
    with _ -> str
  in 
  replace str 0 

let tokenize ((ln, line) : code_line) : (code_line * string list) = 
  if is_empty line then
    ((ln, line), [])
  else
    let line = (String.split_on_char '/' (String.trim line)) |> List.hd in
    let all_str_map = extract_all_strings (ln, line) in
    let line = List.fold_left (fun acc (sym, (s, _)) -> string_replace acc s sym) line all_str_map in
    let parts = String.split_on_char ' ' line in
    let no_commas = List.map (fun x -> String.split_on_char ',' x) parts |> List.flatten in
    let no_empty_strs = List.filter (fun x -> not (String.equal x "")) no_commas in 
    let tok_replaced = List.fold_left (fun acc (sym, (s, _)) -> List.map (fun x -> string_replace x sym s) acc) no_empty_strs all_str_map in
    ((ln, line), tok_replaced)

let is_tld (line : string) : bool = String.starts_with line ~prefix:"."
let is_lbl_def (line : string) : bool = String.contains line ':'
let is_digit (c : char) : bool = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
let is_number (n : string) : bool =  (* or starts with a digit, 0x, 0b, 0o *)
  let tn = String.trim n in
  let starts_with_digit = is_digit (String.get tn 0) in 
  let starts_with_0x = String.starts_with tn ~prefix:"0x" in 
  let starts_with_0b = String.starts_with tn ~prefix:"0b" in 
  let starts_with_0o = String.starts_with tn ~prefix:"0o" in 
  starts_with_digit || starts_with_0x || starts_with_0b || starts_with_0o
let is_byte (n : string) : bool = 
  let tn = String.trim n in
  let starts_with_0x = String.starts_with tn ~prefix:"0x" in 
  let starts_with_0b = String.starts_with tn ~prefix:"0b" in 
  let starts_with_0o = String.starts_with tn ~prefix:"0o" in 
  try
    let num = Int64.of_string tn in
    (num >= Int64.of_int 0 && num <= Int64.of_int 255) || starts_with_0x || starts_with_0b || starts_with_0o
  with _ -> false
let is_char (n : string) : bool = 
  try 
    let chr = String.get n 0 in 
    Char.code chr >= 0 && Char.code chr <= 255 (* probably always true but whatever *)
  with _ -> false
    

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

(* string tokenization *)
let tokenize_str (str: string) : char list =
  let char_list = String.to_seq str |> List.of_seq in
  let rec tsh (chars: char list) (is_in_str: bool) : char
  list =
    match chars with 
    | [] -> []
    | h::[] -> 
        if is_in_str then
          match h with
          | '"' -> []
          | _ -> [h]
        else 
          [h]
    | h::la::t -> 
        if is_in_str then
          match h with
          | '"' -> tsh (la::t) false
          | '\\' ->
              let escaped_str = String.make 1 h ^ String.make 1 la in
              begin match escaped_str with 
              | "\\n" -> ['\n'] @ tsh t true 
              | "\\t" -> ['\t'] @ tsh t true 
              | "\\r" -> ['\r'] @ tsh t true 
              | "\\0" -> ['\000'] @ tsh t true 
              | _ -> tsh (la::t) true
              end 
          | _ -> [h] @ tsh (la::t) true
        else 
          if h = '"' then tsh (la::t) true
          else [h] @ tsh (la::t) false
  in tsh char_list false
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
  match partial with
  | "mov" -> Arm.Mov | "adr" -> Arm.Adr 
  | "ldr" -> Arm.Ldr | "ldrb" -> Arm.Ldrb | "str" -> Arm.Str | "strb" -> Arm.Strb 
  | "add" -> Arm.Add| "sub" -> Arm.Sub  | "mul" -> Arm.Mul 
  | "adds" -> Arm.Adds | "subs" -> Arm.Subs | "muls" -> Arm.Muls
  | "and" -> Arm.And | "orr" -> Arm.Orr  | "lsl" -> Arm.Lsl | "lsr" -> Arm.Lsr  | "asr" -> Arm.Asr | "not" -> Arm.Not
  | "ands" -> Arm.Ands | "orrs" -> Arm.Orrs | "lsls" -> Arm.Lsls | "lsrs" -> Arm.Lsrs | "asrs" -> Arm.Asrs | "nots" -> Arm.Nots
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
      | "" -> Arm.Al
      | _ -> raise Syntax_error) in Arm.B cnd_code
    with _ -> Arm.B(Arm.Al))
  | "cmp" -> Arm.Cmp | "cbz" -> Arm.Cbz  | "cbnz" -> Arm.Cbnz 
  | "bl" -> Arm.Bl  | "ret" -> Arm.Ret 
  | "svc" -> Arm.Svc
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
  | "w0" -> Arm.W0 | "w1" -> Arm.W1 | "w2" -> Arm.W2 | "w3" -> Arm.W3 
  | "w4" -> Arm.W4 | "w5" -> Arm.W5 | "w6" -> Arm.W6 | "w7" -> Arm.W7 
  | "w8" -> Arm.W8 | "w9" -> Arm.W9 
  | "w10" -> Arm.W10 | "w11" -> Arm.W11 | "w12" -> Arm.W12 | "w13" -> Arm.W13 
  | "w14" -> Arm.W14 | "w15" -> Arm.W15 | "w16" -> Arm.W16 | "w17" -> Arm.W17
  | "w18" -> Arm.W18 | "w19" -> Arm.W19 | "w20" -> Arm.W20 | "w21" -> Arm.W21 
  | "w22" -> Arm.W22 | "w23" -> Arm.W23 | "w24" -> Arm.W24 | "w25" -> Arm.W25 
  | "w26" -> Arm.W26 | "w27" -> Arm.W27 | "w28" -> Arm.W28 | "w29" -> Arm.W29 
  | "w30" -> Arm.W30 | "w31" -> Arm.W31
  | _ -> arm_error ln insn reg "Invalid lower register"

let is_not_register (r : string) : bool = 
  try 
    let _ = register_of_string (0, "") r in 
    false
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
    if is_number imm then 
      Arm.Ind3(register_of_string (ln, insn) reg, imm_of_string (ln, insn) imm)
    else 
      Arm.Ind4(register_of_string (ln, insn) reg, register_of_string (ln, insn) imm)

let operand_of_string ((ln, insn) : code_line) (operand : string) : Arm.operand = 
  if is_number operand || is_not_register operand then 
    Arm.Imm (imm_of_string (ln, insn) operand)
  else 
    Arm.Reg (register_of_string (ln, insn) operand)

let operands_of_tokens ((ln, insn) : code_line) (args : string list) : Arm.operand list = 
  match args with
  | [] -> []
  | a1::[] -> [operand_of_string (ln, insn) a1]
  | a1::a2::[] -> 
      if String.contains a2 '[' && String.contains a2 ']' then
        let fixed_a2 = List.nth (String.split_on_char '[' a2) 1 in 
        let fixed_a3 = List.nth (String.split_on_char ']' fixed_a2) 0 in
        [operand_of_string (ln, insn) a1; Arm.Offset(offset_of_string (ln, insn) [fixed_a3])]
      else
        [operand_of_string (ln, insn) a1; operand_of_string (ln, insn) a2]
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
let string_to_bytes (s : string) : char list = 
  let rec stbh (s : string) (acc : char list) (idx : int) : char list = 
    if idx = String.length s then acc
    else stbh s (acc @ [String.get s idx]) (idx + 1)
  in stbh s [] 0

let token_to_int64 ((ln, line) : code_line) (token : string) : int64 = 
  if is_number token then 
    let imm = imm_of_string (ln, line) token in 
    (match imm with 
    | Arm.Lit(i) -> i 
    | Arm.Lbl(_) -> arm_error ln line token "Invalid i64"
    )
  else 
    arm_error ln line token "Invalid i64"

let token_to_int32 ((ln, line) : code_line) (token : string) : int32  = 
  if is_number token then 
    let imm = imm_of_string (ln, line) token in 
    (match imm with 
    | Arm.Lit(i) -> Int64.to_int32 i 
    | Arm.Lbl(_) -> arm_error ln line token "Invalid i32"
    )
  else 
    arm_error ln line token "Invalid i64"

let parse_quad ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty quad!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      Arm.Quad(token_to_int64 (ln, line) imm)
    else
      arm_error ln line imm "Invalid quad"

let parse_word ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty word!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      Arm.Word(token_to_int32 (ln, line) imm)
    else
      arm_error ln line imm "Invalid word"

let parse_quad_arr ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty quad array!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      let all_valid = List.for_all (fun x -> is_number x) tokens in
      if not all_valid then
        arm_error ln line (List.find (fun x -> not (is_number x)) tokens) "Invalid quad"
      else
        let quads = List.map (fun x -> token_to_int64 (ln, line) x) tokens in
        Arm.QuadArr(quads)
    else
      arm_error ln line imm "Invalid quad array"

let parse_word_arr((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty word array!"
  else
    let imm = List.nth tokens 0 in
    if is_number imm then
      let all_valid = List.for_all (fun x -> is_number x) tokens in
      if not all_valid then
        arm_error ln line (List.find (fun x -> not (is_number x)) tokens) "Invalid word"
      else
        let words = List.map (fun x -> token_to_int32 (ln, line) x) tokens in
        Arm.WordArr(words)
    else
      arm_error ln line imm "Invalid quad array"

let parse_byte_list ((ln, line) : code_line) (tokens : string list) : int list =
  if List.length tokens = 0 then
    arm_error ln line line "Empty byte list!"
  else
    let all_valid = List.for_all (fun x -> is_byte x) tokens in
    if not all_valid then
      arm_error ln line (List.find (fun x -> not (is_byte x)) tokens) "Invalid char"
    else
      List.map int_of_string tokens

let parse_byte ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty byte!"
  else
    let imm = List.nth tokens 0 in
    if is_byte imm then
      Arm.Byte(token_to_int64 (ln, line) imm |> Int64.to_int)
    else
      arm_error ln line imm "Invalid byte"

let parse_byte_arr ((ln, line) : code_line) (tokens : string list) : Arm.data = 
  if List.length tokens = 0 then
    arm_error ln line line "Empty byte array!"
  else
    let bytes = parse_byte_list (ln, line) tokens in 
    Arm.ByteArr(bytes)

let parse_skip ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then 
    arm_error ln line line "Invalid skip!"
  else 
    let imm = List.nth tokens 0 in 
    if is_number imm then 
      let skips = List.init (token_to_int64 (ln, line) imm |> Int64.to_int) (fun _ -> 0) in 
      Arm.ByteArr(skips)
    else 
      arm_error ln line imm "Invalid number"

let parse_string ((ln, line) : code_line) (tokens : string list) : Arm.data = 
  if List.length tokens = 0 then
    arm_error ln line line "Empty string!"
  else
    let bytes = parse_byte_list (ln, line) tokens in 
    Arm.ByteArr(bytes)

let parse_asciz ((ln, line) : code_line) (tokens : string list) : Arm.data = 
  if List.length tokens = 0 then
    arm_error ln line line "Empty asciz!"
  else
    let chars = tokenize_str (List.nth tokens 0) in
    let bytes = List.map (fun x -> Char.code x) chars in
    Arm.ByteArr(bytes @ [0])

let parse_ddef ((ln, line) : code_line) (tokens : string list) : Arm.data =
  if List.length tokens = 0 then
    arm_error ln line line "Empty data definition!"
  else
    let mnemonic = List.nth tokens 0 in
    match mnemonic, ((List.length tokens) - 1) with
    | ".quad", 1 -> parse_quad (ln, line) (List.tl tokens) 
    | ".quad", _ -> parse_quad_arr (ln, line) (List.tl tokens)
    | ".dword", 1 -> parse_quad (ln, line) (List.tl tokens) 
    | ".dword", _ -> parse_quad_arr (ln, line) (List.tl tokens)
    | ".byte", 1 -> parse_byte (ln, line) (List.tl tokens) 
    | ".byte", _ -> parse_byte_arr (ln, line) (List.tl tokens) 
    | ".string", _ -> parse_asciz (ln, line) (List.tl tokens) 
    | ".asciz", _ -> parse_asciz (ln, line) (List.tl tokens) 
    | ".word", 1 -> parse_word (ln, line) (List.tl tokens) 
    | ".word", _ -> parse_word_arr (ln, line) (List.tl tokens) 
    | ".int", 1 -> parse_word (ln, line) (List.tl tokens) 
    | ".int", _ -> parse_word_arr (ln, line) (List.tl tokens) 
    | ".skip", 1 -> parse_skip (ln, line) (List.tl tokens)
    | _ -> arm_error ln line mnemonic "Invalid data definition"

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
  let text_blocks_parsed = Arm.TextDirect(List.map (fun x -> parse_text_block x) text_blocks) in 
  let data_directives = List.concat (find_directives lines "data") in 
  let data_blocks = find_blocks data_directives in 
  let data_blocks_parsed = Arm.DataDirect(List.map (fun x -> parse_data_block x) data_blocks) in 
  extern_defs @ global_defs @ [text_blocks_parsed] @ [data_blocks_parsed] 

