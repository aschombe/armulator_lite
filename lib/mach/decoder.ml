let operand_as_register (m: Mach.t) (op: Arm.operand) : Arm.reg =
  match op with 
  | Arm.Reg r -> r
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpexted register"

let operand_as_int64 (m: Mach.t) (op: Arm.operand) : int64 = 
  match op with 
  | Arm.Imm (Lit i) -> i
  | Arm.Imm (Lbl l) -> Mach.lookup_label m.info.layout l
  | Arm.Reg r -> m.regs.(Mach.reg_index r) 
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpected immediate"

let operand_as_label (m: Mach.t) (op: Arm.operand) : string = 
  match op with 
  | Arm.Imm(Lbl l) -> l 
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpexted label"

let operand_as_load_addr (m: Mach.t) (op: Arm.operand) : int64 = 
  match op with 
  | Arm.Offset(Arm.Ind1(im)) -> begin match im with 
    | Lit n -> n 
    | Lbl l -> Mach.lookup_label m.info.layout l
    end
  | Arm.Offset(Arm.Ind2(r)) -> m.regs.(Mach.reg_index r)
  | Arm.Offset(Arm.Ind3(r, Lit offs)) -> Int64.add m.regs.(Mach.reg_index r) (offs)
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpected offset"
