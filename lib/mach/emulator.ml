let mem_store_address (m: Mach.t) (o: Arm.operand) (rgs: int64 array) : int =
  match o with
  | Arm.Imm (Lit q) -> Mach.map_addr m q
  | Arm.Reg r -> Mach.map_addr m (rgs.(Mach.reg_index r))
  | Arm.Offset(Arm.Ind1(Arm.Lit i)) -> Mach.map_addr m i
  | Arm.Offset(Arm.Ind2(r)) -> Mach.map_addr m rgs.(Mach.reg_index r)
  | Arm.Offset(Arm.Ind3(r, Lit i)) -> Mach.map_addr m (Int64.add rgs.(Mach.reg_index r) i) 
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand o) "Unexpected operand"

let store_at (bt: Mach.sbyte) (addr: int) (m: Mach.sbyte array) = Array.set m addr bt
let read_at (addr: int64) (m: Mach.sbyte array) : Mach.sbyte = Array.get m (Int64.to_int addr)

let sbytes_of_data : Arm.data -> Mach.sbyte list = function
  | Arm.Quad i -> Mach.sbytes_of_int64 i 
  | Arm.QuadArr ia -> List.flatten (List.map Mach.sbytes_of_int64 ia) 
  | Arm.Byte b -> [Mach.Byte(b |> Char.chr)]
  | Arm.ByteArr ba -> List.map (fun b -> Mach.Byte(b |> Char.chr)) ba
  | Arm.Word w -> Mach.sbytes_of_int32 w
  | Arm.WordArr wa -> List.flatten (List.map Mach.sbytes_of_int32 wa)

let data_into_memory (addr: int) (value: Arm.data) (m: Mach.sbyte array) : unit =
  let bytes = sbytes_of_data value in 
  let f (i: int) (e: Mach.sbyte) = (store_at e (addr + i) m) in 
  List.iteri f bytes

let rec read_bytes (addr: int64) (count: int64) (m: Mach.sbyte array) : Mach.sbyte list =
  match count with
  | 1L -> [read_at addr m]
  | _ ->  (read_bytes addr (Int64.sub count 1L) m) @ [read_at (Int64.add addr (Int64.sub count 1L)) m]

let data_into_reg (r: Arm.reg) (value: Arm.data) (rgs: int64 array) : unit = 
  match value with
  | Arm.Quad i -> rgs.(Mach.reg_index r) <- i 
  | Arm.Word w -> rgs.(Mach.reg_index r) <- Int64.of_int32 w 
  | Arm.Byte b -> rgs.(Mach.reg_index r) <- Int64.of_int b
  | _ -> failwith "Unexpected data type"

let reg_store (v: Arm.data) (r: Arm.reg) (rgs: int64 array) : unit = data_into_reg r v rgs

let mem_store (m: Mach.t) (v: Arm.data) (o: Arm.operand) (rgs: int64 array) (mem: Mach.sbyte array) : unit =
  let str_addr = (mem_store_address m o rgs) in 
  data_into_memory str_addr v mem

let update_flags (result: Int64_overflow.t) : Mach.flags = 
  { n = if result.value < 0L then true else false; 
    z = if result.value = 0L then true else false; 
    c = result.overflow; 
    v = result.overflow; }

let step (m: Mach.t) : Mach.t = 
  let insn = Mach.get_insn m m.pc in
  print_endline (Printf.sprintf "+%04d: %s" (Int64.to_int m.pc) (Arm_stringifier.string_of_insn insn));
  (*Mach.print_machine_state m;*)
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
    let data = read_bytes load_addr 8L m.mem |> Mach.int64_of_sbytes in 
    m.regs.(Mach.reg_index reg) <- data;
    m
  | (Arm.Add, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.add addend1 addend2;
    m
  | (Arm.Adds, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.add addend1 addend2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Sub, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.sub addend1 addend2;
    m
  | (Arm.Subs, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.sub addend1 addend2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Mul, [o1; o2; o3]) -> 
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    m.regs.(Mach.reg_index reg) <- Int64.mul addend1 addend2;
    m
  | (Arm.Muls, [o1; o2; o3]) ->
    let reg = Decoder.operand_as_register m o1 in 
    let addend1 = Decoder.operand_as_int64 m o2 in 
    let addend2 = Decoder.operand_as_int64 m o3 in 
    let result = Int64_overflow.mul addend1 addend2 in 
    let n_flags = update_flags result in
    m.flags <- n_flags;
    m.regs.(Mach.reg_index reg) <- result.value;
    m
  | (Arm.Bl, [o1]) -> 
    let label_name = Decoder.operand_as_label m o1 in 
    let label_val = Mach.lookup_label m.info.layout label_name in 
    m.regs.(Mach.reg_index Arm.LR) <- m.pc;
    m.pc <- Int64.sub (Int64.sub label_val m.info.mem_bot) 8L; (* subtract 8 because step will increment PC *)
    m
  | (Arm.Ret, []) -> 
    m.pc <- m.regs.(Mach.reg_index Arm.LR);
    m
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_insn insn) "Unexpected instruction"

let run (m: Mach.t) : unit = 
  let rec loop (m: Mach.t) : unit =
    let m' = step m in 
    if Int64.equal m'.pc m'.info.exit_val || Int64.equal m'.regs.(Mach.reg_index Arm.SP) m'.info.exit_val then (Mach.print_machine_state m; print_endline "DONE") else
      (m'.pc <- (Int64.add m'.pc 8L); loop m')
  in print_endline "__emulator_start"; loop m 

