let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 32
let ins_size = 8L                (* 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when pc = exit_addr *)

type sbyte = 
| InsFill 
| Insn of Arm.insn 
| Byte of char

type flags = {
  mutable n: bool; (* negative flag is set if the result of any instruction is negative *)
  mutable z: bool; (* zero flag is set if the result of any instruction is zero *)
  mutable c: bool; (* carry flag is set if the result of any instruction causes a carry *)
  mutable v: bool; (* signed overflow flag is set if the result of any instruction causes an overflow *)
}

type mach = {
    regs: int64 array;
    pc: int64;
    mem: sbyte array;
    flags: flags;
}

let reg_index = function
  | Arm.X0 -> 0 | Arm.X1 -> 1 | Arm.X2 -> 2 | Arm.X3 -> 3 
  | Arm.X4 -> 4 | Arm.X5 -> 5 | Arm.X6 -> 6 | Arm.X7 -> 7 
  | Arm.X8 -> 8 | Arm.X9 -> 9 
  | Arm.X10 -> 10 | Arm.X11 -> 11 | Arm.X12 -> 12 | Arm.X13 -> 13
  | Arm.X14 -> 14 | Arm.X15 -> 15 | Arm.X16 -> 16 | Arm.X17 -> 17
  | Arm.X18 -> 18 | Arm.X19 -> 19 | Arm.X20 -> 20 | Arm.X21 -> 21
  | Arm.X22 -> 22 | Arm.X23 -> 23 | Arm.X24 -> 24 | Arm.X25 -> 25
  | Arm.X26 -> 26 | Arm.X27 -> 27 | Arm.X28 -> 28 
  | Arm.SP -> 29 | Arm.XZR -> 31 | Arm.LR -> 30 

let map_addr addr = 
  if addr < mem_bot || addr >= mem_top then
    raise (Invalid_argument "map_addr")
  else
    Int64.to_int (Int64.sub addr mem_bot)

let get_insn (m: mach) (addr: int64) : Arm.insn =
  let i = map_addr addr in
  match m.mem.(i) with
  | Insn i -> i
  | _ -> raise (Invalid_argument "get_insn")

let get_byte (m: mach) (addr: int64) : sbyte =
  let i = map_addr addr in
  match m.mem.(i) with
  | Byte b -> Byte b
  | _ -> raise (Invalid_argument "get_byte")

let load_quad (m: mach) (addr: int64) : int64 =
  let bytes = Array.init 8 (fun i -> get_byte m (Int64.add addr (Int64.of_int i))) in
  let f b i = match b with
    | Byte c -> Int64.logor (Int64.shift_left i 8) (c |> Char.code |> Int64.of_int)
    | _ -> 0L
  in Array.fold_right f bytes 0L

let load_byte (m: mach) (addr: int64) : char = 
  match get_byte m addr with
  | Byte c -> c
  | _ -> raise (Invalid_argument "load_byte")

let store_byte (m: mach) (addr: int64) (b: char) : unit =
  let i = map_addr addr in
  m.mem.(i) <- Byte b 

let store_quad (m: mach) (addr: int64) (q: int64) : unit =
  let bytes = Array.init 8 (fun i -> Int64.to_int (Int64.logand (Int64.shift_right_logical q (i * 8)) 0xffL) |> Char.chr) in
  Array.iteri (fun i b -> store_byte m (Int64.add addr (Int64.of_int i)) b) bytes 

let encode_insn (i: Arm.insn) : sbyte array =
  let buf = Array.make 8 InsFill in
  buf.(0) <- Insn i;
  buf

let init (prog: Arm.prog) : mach =
  let mem = Array.make mem_size InsFill in
  let regs = Array.make nregs 0L in
  regs.(reg_index Arm.SP) <- mem_top;
  { regs = regs;
    pc = mem_bot;
    mem = mem;
    flags = { n = false; z = false; c = false; v = false; }
  }
