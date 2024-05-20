let step (m: Mach.t) : Mach.t = 
  let insn = Mach.get_insn m m.pc in 
  match insn with
  | (Arm.Mov, [o1; o2]) ->
    let reg = begin match o1 with
      | Arm.Reg r -> r
      | _ -> Mach.mach_error m (Arm.string_of_operand o1) "Unexpexted register"
    end in
    let v = begin match o2 with
      | Arm.Imm (Lit i) -> i
      | Arm.Imm (Lbl l) -> Mach.lookup_label m.info.layout l
      | Arm.Reg r -> m.regs.(Mach.reg_index r) 
      | _ -> Mach.mach_error m (Arm.string_of_operand o2) "Unexpected immediate"
    end in 
    m.regs.(Mach.reg_index reg) <- v;
    m
  | _ -> Mach.mach_error m (Arm.string_of_insn insn) "Unexpected instruction"

let run (m: Mach.t) : unit = 
  let rec loop (m: Mach.t) : unit = 
    let m' = step m in 
    m'.pc <- (Int64.add m'.pc 8L);
    loop m' in
  loop m 
