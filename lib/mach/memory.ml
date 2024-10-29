exception Invalid_address of int

let address_error (op_type: string) (addr: int) (highlight : string) (msg : string) : 'a =
  let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") "Invalid address!" in
  let () = Printf.fprintf Out_channel.stderr "\x1b[1;91m%s error \x1b[0mat address \x1b[1;97m0x%x\x1b[0m:" op_type addr in
  let () = Printf.fprintf Out_channel.stderr " %s.\n\n" msg in
  let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
  raise (Invalid_address addr)

let mem_store_address (m: Mach.t) (o: Arm.operand) (rgs: int64 array) : int =
  match o with
  | Arm.Imm (Lit q) -> Mach.map_addr m q
  | Arm.Reg r -> Mach.map_addr m (rgs.(Mach.reg_index r))
  | Arm.Offset(Arm.Ind1(Arm.Lit i)) -> Mach.map_addr m i
  | Arm.Offset(Arm.Ind2(r)) -> Mach.map_addr m rgs.(Mach.reg_index r)
  | Arm.Offset(Arm.Ind3(r, Lit i)) -> Mach.map_addr m (Int64.add rgs.(Mach.reg_index r) i) 
  | _ -> Mach.mach_error m (Arm_stringifier.string_of_operand o) "Unexpected operand"

let store_at (bt: Mach.sbyte) (addr: int) (m: Mach.sbyte array) = 
  try
    Array.set m addr bt
  with _ ->
    address_error "Write" (0x400000 + addr) "Invalid address!" "Could not write to memory!"

let read_at (addr: int) (m: Mach.sbyte array) : Mach.sbyte = 
  try 
    Array.get m addr
  with _ ->
    address_error "Read" (0x400000 + addr) "Invalid address!" "Could not read from memory!"
    
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

let rec read_bytes (addr: int) (count: int) (m: Mach.sbyte array) : Mach.sbyte list =
  match count with
  | 1 -> [read_at addr m]
  | _ ->  (read_bytes addr (count - 1) m) @ [read_at (addr + (count - 1)) m]

let rec write_bytes (addr: int) (count: int) (data: Mach.sbyte list) (m: Mach.sbyte array) : Mach.sbyte array =
  match count, data with
  | 1, [byte] ->
      m.(addr) <- byte; 
      m
  | _, byte::rest ->
      m.(addr) <- byte;
      write_bytes (addr - 1) (count - 1) rest m
  | _, [] -> m
  
let rec read_to_null_terminator (addr: int) (m: Mach.sbyte array) : Mach.sbyte list =
  let byte = read_at addr m in
  match byte with
  | Byte '\000' -> [byte]
  | Byte _ ->  [byte] @ (read_to_null_terminator (addr + 1) m)
  | _ -> []

let mem_store (m: Mach.t) (v: Arm.data) (o: Arm.operand) (rgs: int64 array) (mem: Mach.sbyte array) : unit =
  let str_addr = (mem_store_address m o rgs) in 
  data_into_memory str_addr v mem