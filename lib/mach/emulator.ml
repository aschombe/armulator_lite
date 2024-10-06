let data_into_reg (r: Arm.reg) (value: Arm.data) (rgs: int64 array) : unit = 
  match value with
  | Arm.Quad i -> rgs.(Mach.reg_index r) <- i 
  | Arm.Word w -> rgs.(Mach.reg_index r) <- Int64.of_int32 w 
  | Arm.Byte b -> rgs.(Mach.reg_index r) <- Int64.of_int b
  | _ -> failwith "Unexpected data type"

let reg_store (v: Arm.data) (r: Arm.reg) (rgs: int64 array) : unit = data_into_reg r v rgs

let update_flags (result: Int64_overflow.t) : Mach.flags = 
  { n = if result.value < 0L then true else false; 
    z = if result.value = 0L then true else false; 
    c = result.overflow; 
    v = result.overflow; }

let step (m: Mach.t) : Mach.t = 
  let insn = Mach.get_insn m m.pc in
  if m.opts.print_machine_state then Mach.print_machine_state m;
  if m.opts.print_machine_state then print_endline (Printf.sprintf "+%04d: %s" (Int64.to_int m.pc) (Arm_stringifier.string_of_insn insn));
  match insn with
  | (Arm.Mov, [o1; o2]) ->
    let reg = Decoder.operand_as_register m o1 in
    let v = Decoder.operand_as_int64 m o2 in 
    m.regs.(Mach.reg_index reg) <- v;
    m
  | (Arm.Adr, [o1; o2]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let label_name = Decoder.operand_as_label m o2 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    m.regs.(Mach.reg_index reg) <- Int64.sub (Int64.sub label_val m.info.mem_bot) 8L;
    m
  | (Arm.Ldr, [o1; o2]) ->
    let reg = Decoder.operand_as_register m o1 in
    let load_addr = Decoder.operand_as_load_addr m o2 in
    let data = Memory.read_bytes load_addr 8L m.mem |> Mach.int64_of_sbytes in 
    m.regs.(Mach.reg_index reg) <- data;
    m
  | (Arm.Ldrb, [o1; o2]) -> 
    let reg = Decoder.operand_as_register m o1 in
    let load_addr = Decoder.operand_as_load_addr m o2 in
    let data = Memory.read_bytes load_addr 1L m.mem |> Mach.int64_of_sbytes in 
    m.regs.(Mach.reg_index reg) <- data;
    m
  | (Arm.Str, [o1; o2]) ->
    let reg = Decoder.operand_as_int64 m o1 in
    let load_addr = Decoder.operand_as_load_addr m o2 in
    let data = Memory.write_bytes load_addr 8L (Mach.sbytes_of_int64 reg) m.mem in 
    m.mem <- data;
    m
  | (Arm.Strb, [o1; o2]) ->
    let reg = Decoder.operand_as_int64 m o1 in
    let load_addr = Decoder.operand_as_load_addr m o2 in
    let data = Memory.write_bytes load_addr 1L (Mach.sbytes_of_int64 reg) m.mem in 
    m.mem <- data;
    m
  | (Arm.Add, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.add operand1 operand2;
    m
  | (Arm.Adds, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.add operand1 operand2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Sub, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.sub operand1 operand2;
    m
  | (Arm.Subs, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.sub operand1 operand2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Mul, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.mul operand1 operand2;
    m
  | (Arm.Muls, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.mul operand1 operand2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.And, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.logand operand1 operand2;
    m
  | (Arm.Ands, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    let result : Int64_overflow.t = { value = Int64.logand operand1 operand2; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Orr, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.logor operand1 operand2;
    m
  | (Arm.Orrs, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 in 
    let result : Int64_overflow.t = { value = Int64.logor operand1 operand2; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Lsl, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    m.regs.(Mach.reg_index reg) <- Int64.shift_left operand1 operand2;
    m
  | (Arm.Lsls, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    let result : Int64_overflow.t = { value = Int64.shift_left operand1 operand2; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Lsr, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    m.regs.(Mach.reg_index reg) <- Int64.shift_right_logical operand1 operand2;
    m
  | (Arm.Lsrs, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    let result : Int64_overflow.t = { value = Int64.shift_right_logical operand1 operand2; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Asr, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    m.regs.(Mach.reg_index reg) <- Int64.shift_right operand1 operand2;
    m
  | (Arm.Asrs, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let operand2 = Decoder.operand_as_int64 m o3 |> Int64.to_int in 
    let result : Int64_overflow.t = { value = Int64.shift_right operand1 operand2; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Not, [o1; o2]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in
    m.regs.(Mach.reg_index reg) <- Int64.lognot operand1;
    m
  | (Arm.Nots, [o1; o2]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let operand1 = Decoder.operand_as_int64 m o2 in 
    let result : Int64_overflow.t = { value = Int64.lognot operand1; overflow = false; } in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Cmp, [o1; o2]) ->
    let operand1 = Decoder.operand_as_int64 m o1 in 
    let operand2 = Decoder.operand_as_int64 m o2 in
    let result = Int64_overflow.sub operand1 operand2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m
  | (Arm.B cnd, [o1]) ->
    let label_name = Decoder.operand_as_label m o1 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    let new_addr = Int64.sub (Int64.sub label_val m.info.mem_bot) 8L in (* subtract 8 because step will increment PC *)
    let do_jump = begin match cnd with 
      | Al -> true
      | Eq -> m.flags.z
      | Ne -> not m.flags.z
      | Lt -> m.flags.n
      | Le -> m.flags.n || m.flags.z
      | Gt -> not m.flags.n && not m.flags.z
      | Ge -> not m.flags.n || m.flags.z
    end in 
    m.pc <- if do_jump then new_addr else m.pc;
    m
  | (Arm.Cbz, [o1; o2]) ->
    let operand1 = Decoder.operand_as_int64 m o1 in 
    let label_name = Decoder.operand_as_label m o2 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    let new_addr = Int64.sub (Int64.sub label_val m.info.mem_bot) 8L in (* subtract 8 because step will increment PC *)
    let result = Int64_overflow.sub operand1 0L in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    if m.flags.z then (m.pc <- new_addr; m) else m
  | (Arm.Cbnz, [o1; o2]) ->
    let operand1 = Decoder.operand_as_int64 m o1 in 
    let label_name = Decoder.operand_as_label m o2 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    let new_addr = Int64.sub (Int64.sub label_val m.info.mem_bot) 8L in (* subtract 8 because step will increment PC *)
    let result = Int64_overflow.sub operand1 0L in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    if not m.flags.z then (m.pc <- new_addr; m) else m
  | (Arm.Bl, [o1]) -> 
    let label_name = Decoder.operand_as_label m o1 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    let m' = begin match (Memory.read_bytes (Int64.sub label_val m.info.mem_bot) 1L m.mem) with 
      | [ExternSym s] -> System.execute_extern_function m s
      | _ -> (
        m.regs.(Mach.reg_index Arm.LR) <- m.pc;
        m.pc <- Int64.sub (Int64.sub label_val m.info.mem_bot) 8L;
        m) 
    end in
    m'
  | (Arm.Ret, []) -> 
    m.pc <- m.regs.(Mach.reg_index Arm.LR);
    m
  | (Arm.Svc, [o1]) ->
    let _ = Decoder.operand_as_int64 m o1 in (* call for error checking, o1 is ignored by the cpu *)
    System.execute_syscall m
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_insn insn) "Unexpected instruction"

let run (m: Mach.t) : unit = 
  let rec loop (m: Mach.t) : unit =
    let m' = step m in 
    if Int64.equal m'.pc m'.info.exit_val || Int64.equal m'.regs.(Mach.reg_index Arm.SP) m'.info.exit_val then (print_endline "__emulator_stop\n"; Mach.print_machine_state m) else
      (m'.pc <- (Int64.add m'.pc 8L); loop m')
  in print_endline "\n__emulator_start"; loop m 

let unwrap_str s =
  match s with
  | Some(v) -> v 
  | None -> ""

let debug (m: Mach.t) : unit = 
  let dbg_step (m: Mach.t) : Mach.t = 
    let m' = step m in 
    if Int64.equal m'.pc m'.info.exit_val || Int64.equal m'.regs.(Mach.reg_index Arm.SP) m'.info.exit_val then (print_endline "__emulator_stop\n"; Mach.print_machine_state m'; m') else
      (m'.pc <- (Int64.add m'.pc 8L); m') in 
  let rec loop (m: Mach.t) (steps: Mach.t list) : unit =
    Printf.printf "(dbg) %!";
    let broken = In_channel.input_line stdin |> unwrap_str |> String.split_on_char ' ' in 
    let command = List.hd broken in 
    let args = List.tl broken in
    match command with 
    | "s" | "step" -> begin
      let copied = Mach.copy m in
      let m' = dbg_step m in loop m' (copied :: steps)
    end
    | "bs" | "backstep" -> begin
      if List.length steps > 0 then 
        let prev_state = List.hd steps in 
        let other_steps = List.tl steps in 
        Printf.printf "+%04d: %s\n" (prev_state.pc |> Int64.to_int) (Mach.get_insn prev_state prev_state.pc |> Arm_stringifier.string_of_insn);
        loop prev_state other_steps
      else 
        Printf.printf "no previous steps\n%!"; loop m steps
    end
    | "r" | "register" -> begin
      try 
        let reg = Arm_parser.register_of_string (0, String.concat " " broken) (List.nth args 0) in
        let rval = Arm_parser.token_to_int64 (0, String.concat " " broken) (List.nth args 1) in 
        m.regs.(Mach.reg_index reg) <- rval;
        loop m steps
      with _ -> 
        Printf.printf "invalid command \"%s\"\n%!" (String.concat " " broken); loop m steps
    end
    | "q" | "quit" -> begin 
      Printf.printf "terminated via command\n%!"; exit 0
    end
    | "sh" | "show" -> begin 
      try 
        match (List.nth args 0) with 
        | "info" -> Mach.print_machine_info m; loop m steps
        | "state" -> Mach.print_machine_state m; loop m steps
        | "regs" -> Mach.print_machine_regs m; loop m steps
        | "flags" -> Mach.print_machine_flags m; loop m steps 
        | "pc" -> Mach.print_machine_pc m; loop m steps
        | _ -> Printf.printf "invalid command \"%s\"\n%!" (String.concat " " broken); loop m steps
      with _ ->
        Printf.printf "invalid arg \"%s\" (use \"info\", \"regs\", \"pc\", \"flags\" or \"state\")\n%!" (String.concat " " broken); loop m steps
    end
    | "h" | "help" -> begin 
      Printf.printf "s|step -> execute the current instruction\n";
      Printf.printf "bs|backstep -> go back to the previous machine state\n";
      Printf.printf "r|register <xR> <val> -> set the register <xR> to <val>\n";
      Printf.printf "sh|show <info/state/regs/flags/pc> -> show machine/state information\n";
      Printf.printf "h|help -> show this message\n";
    end
    | _ -> (Printf.printf "unknown command \"%s\"\n%!" command; loop m steps)
  in loop m []
