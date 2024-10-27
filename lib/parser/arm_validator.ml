exception Invalid_instruction
exception Unknown

let arm_error (ln : int) (line : string) (highlight : string) (msg : string) : 'a =
  if String.equal highlight String.empty || ln == 0 then 
    raise Unknown
  else
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") line in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mon line \x1b[1;97m%d\x1b[0m:" ln in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (Invalid_instruction)

let validate_arm_insn (insn: Arm.insn) : bool = 
  match insn with 
  | (Arm.Mov, [o1; o2]) ->
    (* arg 1 must be a register, arg 2 can be a register or immediate *)
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_int64 o2 in 
    v1 && v2
  | (Arm.Adr, [o1; o2]) ->
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_label o2 in 
    v1 && v2
  | (Arm.Ldr, [o1; o2])
  | (Arm.Ldrb, [o1; o2]) 
  | (Arm.Str, [o1; o2])
  | (Arm.Strb, [o1; o2]) -> 
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_load_addr o2 in 
    v1 && v2
  | (Arm.Add, [o1; o2; o3]) | (Arm.Adds, [o1; o2; o3])
  | (Arm.Sub, [o1; o2; o3]) | (Arm.Subs, [o1; o2; o3]) 
  | (Arm.And, [o1; o2; o3]) | (Arm.Ands, [o1; o2; o3])
  | (Arm.Orr, [o1; o2; o3]) | (Arm.Orrs, [o1; o2; o3])
  | (Arm.Lsl, [o1; o2; o3]) | (Arm.Lsls, [o1; o2; o3])
  | (Arm.Lsr, [o1; o2; o3]) | (Arm.Lsrs, [o1; o2; o3])
  | (Arm.Asr, [o1; o2; o3]) | (Arm.Asrs, [o1; o2; o3])
  | (Arm.Not, [o1; o2; o3]) | (Arm.Nots, [o1; o2; o3]) -> 
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_register o2 in 
    let v3 = Decoder.operand_is_int64 o3 in 
    v1 && v2 && v3
  | (Arm.Mul, [o1; o2; o3]) 
  | (Arm.Muls, [o1; o2; o3]) ->
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_register o2 in 
    let v3 = Decoder.operand_is_register o3 in 
    v1 && v2 && v3
  | (Arm.Cmp, [o1; o2]) -> 
    let v1 = Decoder.operand_is_register o1 in 
    let v2 = Decoder.operand_is_int64 o2 in 
    v1 && v2
  | (Arm.Cbz, [o1])
  | (Arm.Cbnz, [o1])
  | (Arm.B _, [o1])
  | (Arm.Bl, [o1])
  | (Arm.Svc, [o1]) -> Decoder.operand_is_int64 o1
  | (Arm.Ret, []) -> true
  | _ -> false

let validate_arm_insns (li: Arm.insn list) : bool = 
  List.fold_left (fun acc insn -> acc && validate_arm_insn insn) true li

let validate_arm_program (prog: Arm.prog) : bool = 
  let rec find_text_blocks (tlds: Arm.prog) : Arm.block list = 
    match tlds with 
    | Arm.TextDirect(lis)::t -> lis @ find_text_blocks t
    | _ -> [] 
  in let rec find_text_directives (bl: Arm.block list) : Arm.insn list = 
    match bl with 
    | { entry=_; lbl=_; asm=Text(il) }::t -> il @ find_text_directives bl @ find_text_directives t
    | _ -> []
  in
  find_text_blocks prog |> find_text_directives |> validate_arm_insns