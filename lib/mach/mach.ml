exception Ok 
exception Segmentation_fault of string
exception No_entrypoint 

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 32
let insn_size = 8L                (* 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when pc = exit_addr *)

type sbyte = 
| InsFill 
| ExternSym of string
| Insn of Arm.insn 
| Byte of char

type flags = {
  mutable n: bool; (* negative flag is set if the result of any instruction is negative *)
  mutable z: bool; (* zero flag is set if the result of any instruction is zero *)
  mutable c: bool; (* carry flag is set if the result of any instruction causes a carry *)
  mutable v: bool; (* signed overflow flag is set if the result of any instruction causes an overflow *)
}

type mach = {
    entry: int64;
    layout: (string * int64) list;
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
  let addr = Int64.add addr mem_bot in
  if addr < mem_bot || addr >= mem_top then
    raise (Invalid_argument "map_addr")
  else
    Int64.to_int (Int64.sub addr mem_bot)

let get_insn (m: mach) (addr: int64) : Arm.insn =
  let i = map_addr addr in
  match m.mem.(i) with
  | Insn i -> i
  | _ -> raise (Invalid_argument "get_insn")

let mach_error (m: mach) (highlight : string) (msg : string) : 'a =
  if highlight <> "_start" then 
    let insn = get_insn m m.pc in
    let str_insn = Arm.string_of_insn insn in
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") str_insn in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mat address\x1b[1;97m0x%x\x1b[0m:" (m.pc |> Int64.to_int) in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (Segmentation_fault msg)
  else
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") "No label '_start' defined!" in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mat address\x1b[1;97m0x%x\x1b[0m:" (m.pc |> Int64.to_int) in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (No_entrypoint)

let gen_layout (prog: Arm.prog) : (string * int64) list =
  let text_block_layout ({entry=_; lbl=l; asm}: Arm.block) (offset: int64) : (int64 * (string * int64)) =
    let block_size = begin match asm with
      | Arm.Text insns -> List.length insns |> Int64.of_int
      | _ -> failwith "using text_block_layout on non-text block"
    end 
    in (Int64.add offset (Int64.mul insn_size block_size), (l, offset))
  in
  let data_block_layout ({entry=_; lbl=l; asm}: Arm.block) (offset: int64) : (int64 * (string * int64)) =
    let data_type_size (d: Arm.data) : int64 = 
      match d with 
      | Arm.Quad _ -> 8L
      | Arm.Byte _ -> 1L
      | Arm.QuadArr arr -> List.length arr |> Int64.of_int |> Int64.mul 8L
      | Arm.ByteArr arr -> List.length arr |> Int64.of_int
    in 
    let block_size = begin match asm with
      | Arm.Data bytes -> List.fold_left (fun acc d -> Int64.add acc (data_type_size d)) 0L bytes
      | _ -> failwith "using data_block_layout on non-data block"
    end 
    in (Int64.add offset block_size, (l, offset))
  in
  let rec compute_layout (prog: Arm.prog) (offset: int64) : (string * int64) list =
    match prog with
    | [] -> []
    | h::t ->
        let (offset, kvps) = begin
          match h with
          | Arm.GloblDef _ -> (offset, [])
          | Arm.ExternSym label -> (Int64.add insn_size offset, [(label, offset)])
          | Arm.TextDirect block -> List.fold_left (fun (offset_acc, kvps_acc) insn -> 
              let (offset, kvps) = text_block_layout insn offset_acc in
              (offset, kvps_acc @ [kvps])
            ) (offset, []) block
          | Arm.DataDirect block -> List.fold_left (fun (offset_acc, kvps_acc) insn -> 
              let (offset, kvps) = data_block_layout insn offset_acc in
              (offset, kvps_acc @ [kvps])
            ) (offset, []) block
          end in kvps @ (compute_layout t (Int64.add offset insn_size))
  in 
  let pre_obfuscation = compute_layout prog 0L in 
  List.map (fun (label, offset) -> (label, Int64.add offset mem_bot)) pre_obfuscation

let rec lookup_label (layout: (string * int64) list) (label: string) : int64 =
  match layout with
  | [] -> raise (Segmentation_fault ("Label '" ^ label ^ "' not found"))
  | (l, offset)::t -> if l = label then offset else lookup_label t label

let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

let build_program (prog: Arm.prog) : sbyte list =
  let build_insn (insn: Arm.insn) : sbyte list = [Insn insn; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill] in
  let build_data (data: Arm.data) : sbyte list = 
    match data with
    | Arm.Quad n -> sbytes_of_int64 n
    | Arm.Byte c -> [Byte (c |> Char.chr)]
    | Arm.QuadArr arr -> List.concat (List.map (fun n -> sbytes_of_int64 n) arr)
    | Arm.ByteArr arr -> List.map (fun c -> Byte (c |> Char.chr)) arr
  in 
  let build_block (block: Arm.block) : sbyte list =
    match block with
    | {entry=_; lbl=_; asm=Arm.Text insns} -> List.concat (List.map build_insn insns)
    | {entry=_; lbl=_; asm=Arm.Data data} -> List.concat (List.map build_data data)
  in 
  let build_directive (dir: Arm.tld) : sbyte list =
    match dir with
    | Arm.GloblDef _ -> []
    | Arm.ExternSym s -> [ExternSym s]
    | Arm.TextDirect blocks
    | Arm.DataDirect blocks -> List.concat (List.map build_block blocks)
  in
  List.concat (List.map build_directive prog)

let init (prog: Arm.prog) : mach =
  let program_bytes = build_program prog |> Array.of_list in
  let mem = Array.make mem_size InsFill in 
  Array.blit program_bytes 0 mem 0 (Array.length program_bytes);
  let regs = Array.make nregs 0L in
  let rec get_entry_addr (map: (string * int64) list) : int64 =
    match map with
    | [] -> mach_error { entry = 0L; layout = map; regs; pc = 0L; mem; flags = { n = false; z = false; c = false; v = false; } } "_start" "Entry point not defined"
    | (l, offset)::t -> if l = "_start" then offset else get_entry_addr t
  in
  let layout = gen_layout prog in 
  regs.(reg_index Arm.SP) <- mem_top;
  { entry = get_entry_addr layout;
    layout = layout;
    regs = regs;
    pc = mem_bot;
    mem = mem;
    flags = { n = false; z = false; c = false; v = false; }
  }

let print_sbyte_array (mem: sbyte array) (max_rows: int) : unit =
  let max_rows = max_rows * 8 in
  print_char '[';
  let print_byte byte = 
    match byte with
    | InsFill -> Printf.printf "InsFill, "
    | ExternSym s -> Printf.printf "ExternSym %s, " s
    | Insn i -> Printf.printf "Insn %s, " (Arm.ast_string_of_insn i)
    | Byte c -> Printf.printf "Byte '%s', " (c |> Char.escaped)
  in 
  Array.iteri (fun i byte -> 
    if i > max_rows then Printf.printf ""
    else
      begin
        if i mod 8 = 0 then Printf.printf "]\n[";
        print_byte byte;
        Printf.printf " "
      end
  ) mem; print_char ']'
