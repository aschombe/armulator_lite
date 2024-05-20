exception Invalid_insstruction
exception Unknown
(* TODO: MAKE VALIDATOR FOR ARM INSTRUCTIONS *)
(* basically check the format of the instruction, ill add an example *)

let arm_error (ln : int) (line : string) (highlight : string) (msg : string) : 'a =
  if String.equal highlight String.empty || ln == 0 then 
    raise Unknown
  else
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") line in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mon line \x1b[1;97m%d\x1b[0m:" ln in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (Invalid_insstruction)

let validate_arm_insn ((ln, line) : (int * string)) (insn: Arm.insn) : bool = 
  match insn with 
  | (Arm.Mov, [o1; o2]) ->
      (* arg 1 must be a register, arg 2 can be a register or immediate *)
      let res1 = begin match o1 with
        | Arm.Reg _ -> true
        | _ -> arm_error ln line (Arm_stringifier.string_of_operand o1) "First operand must be a register"
      end in 
      let res2 = begin match o2 with
        | Arm.Reg _ -> true
        | Arm.Imm _ -> true
        | _ -> arm_error ln line (Arm_stringifier.string_of_operand o2) "Second operand must be a register or immediate"
      end in 
      res1 && res2
  | _ -> arm_error ln line "" "Invalid instruction format"
