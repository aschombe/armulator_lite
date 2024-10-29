let is_w_register (r: Arm.reg) : bool = 
  match r with 
  | Arm.W0 | Arm.W1 | Arm.W2 | Arm.W3 | Arm.W4 | Arm.W5 | Arm.W6 | Arm.W7
  | Arm.W8 | Arm.W9 | Arm.W10 | Arm.W11 | Arm.W12 | Arm.W13 | Arm.W14 | Arm.W15
  | Arm.W17 | Arm.W18 | Arm.W19 | Arm.W20 | Arm.W21 | Arm.W22 | Arm.W23 | Arm.W24 
  | Arm.W25 | Arm.W26 | Arm.W27 | Arm.W28 | Arm.W29 | Arm.W30 | Arm.W31 -> true
  | _ -> false

let is_x_register (r: Arm.reg) : bool = not (is_w_register r)

let operand_as_register (m: Mach.t) (op: Arm.operand) : Arm.reg =
  match op with 
  | Arm.Reg r -> r
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpexted register"
let operand_is_register (op: Arm.operand) : bool =  
  match op with 
  | Arm.Reg _ -> true
  | _ -> false

let operand_as_int64 (m: Mach.t) (op: Arm.operand) : int64 = 
  match op with 
  | Arm.Imm (Lit i) -> i
  | Arm.Imm (Lbl l) -> Mach.lookup_label m.info.layout l
  | Arm.Reg r -> m.regs.(Mach.reg_index r)
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpected immediate"
let operand_is_int64 (op: Arm.operand) : bool = 
  match op with 
  | Arm.Imm (Lit _) -> true 
  | Arm.Imm (Lbl _) -> true
  | Arm.Reg _ -> true
  | _ -> false

let operand_as_int32 (m: Mach.t) (op: Arm.operand) : int32 = 
  match op with 
  | Arm.Imm (Lit i) -> i |> Int64.to_int32
  | Arm.Imm (Lbl l) -> Mach.lookup_label m.info.layout l |> Int64.to_int32
  | Arm.Reg r -> if is_w_register r then m.regs.(Mach.reg_index r) |> Int64.to_int32 else Mach.mach_error m (Arm_stringifier.string_of_operand op) "Needs W register"
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpected immediate"
let operand_is_int32 (op: Arm.operand) : bool = 
  match op with 
  | Arm.Imm (Lit _) -> true 
  | Arm.Imm (Lbl _) -> true
  | Arm.Reg _ -> true
  | _ -> false

let operand_as_label (m: Mach.t) (op: Arm.operand) : string = 
  match op with 
  | Arm.Imm(Lbl l) -> l 
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpexted label"
let operand_is_label (op: Arm.operand) : bool = 
  match op with 
  | Arm.Imm(Lbl _) -> true
  | _ -> false

let operand_as_load_addr (m: Mach.t) (op: Arm.operand) : int64 = 
  match op with 
  | Arm.Offset(Arm.Ind1(im)) -> begin match im with 
    | Lit n -> n 
    | Lbl l -> Mach.lookup_label m.info.layout l
    end
  | Arm.Offset(Arm.Ind2(r)) -> m.regs.(Mach.reg_index r)
  | Arm.Offset(Arm.Ind3(r, Lit offs)) -> Int64.add m.regs.(Mach.reg_index r) (offs)
  | Arm.Offset(Arm.Ind4(r1, r2)) -> Int64.add m.regs.(Mach.reg_index r1) m.regs.(Mach.reg_index r2)
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand op) "Unexpected offset"
let operand_is_load_addr (op: Arm.operand) : bool = 
  match op with 
  | Arm.Offset _ -> true
  | _ -> false