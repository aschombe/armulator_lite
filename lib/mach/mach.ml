exception Ok 
exception Segmentation_fault of string
exception No_entrypoint 

let insn_size = 8L

type sbyte = 
| InsFill 
| GlobalDef of string
| ExternSym of string
| Insn of Arm.insn 
| Byte of char

type flags = {
  mutable n: bool; (* negative flag is set if the result of any instruction is negative *)
  mutable z: bool; (* zero flag is set if the result of any instruction is zero *)
  mutable c: bool; (* carry flag is set if the result of any instruction causes a carry *)
  mutable v: bool; (* signed overflow flag is set if the result of any instruction causes an overflow *)
}

type mach_info = {
  mem_bot: int64;
  mem_size: int64;
  mem_top: int64;
  nregs: int;
  exit_val: int64;
  entry: (string * int64);
  layout: (string * int64) list;
}

type mach = {
    info: mach_info;
    mutable regs: int64 array;
    mutable pc: int64;
    mutable mem: sbyte array;
    flags: flags;
}

type t = mach

let reg_index = function
  | Arm.X0 | Arm.W0 -> 0 | Arm.X1 | Arm.W1 -> 1 | Arm.X2 | Arm.W2 -> 2 | Arm.X3 | Arm.W3 -> 3 
  | Arm.X4 | Arm.W4 -> 4 | Arm.X5 | Arm.W5 -> 5 | Arm.X6 | Arm.W6 -> 6 | Arm.X7 | Arm.W7 -> 7 
  | Arm.X8 | Arm.W8 -> 8 | Arm.X9 | Arm.W9 -> 9 
  | Arm.X10 | Arm.W10 -> 10 | Arm.X11 | Arm.W11 -> 11 | Arm.X12 | Arm.W12 -> 12 | Arm.X13 | Arm.W13 -> 13 
  | Arm.X14 | Arm.W14 -> 14 | Arm.X15 | Arm.W15 -> 15 | Arm.X16 | Arm.W16 -> 16 | Arm.X17 | Arm.W17 -> 17 
  | Arm.X18 | Arm.W18 -> 18 | Arm.X19 | Arm.W19 -> 19 | Arm.X20 | Arm.W20 -> 20 | Arm.X21 | Arm.W21 -> 21 
  | Arm.X22 | Arm.W22 -> 22 | Arm.X23 | Arm.W23 -> 23 | Arm.X24 | Arm.W24 -> 24 | Arm.X25 | Arm.W25 -> 25 
  | Arm.X26 | Arm.W26 -> 26 | Arm.X27 | Arm.W27 -> 27 | Arm.X28 | Arm.W28 -> 28 | Arm.SP | Arm.W29 -> 29
  | Arm.XZR -> 31 | Arm.LR | Arm.W30 -> 30 

let map_addr (m: mach) (addr: int64) : int = 
  let addr = Int64.add addr m.info.mem_bot in
  if addr < m.info.mem_bot || addr >= m.info.mem_top then
    raise (Invalid_argument "map_addr")
  else
    Int64.to_int (Int64.sub addr m.info.mem_bot)

let get_insn (m: mach) (addr: int64) : Arm.insn =
  let i = map_addr m addr in
  match m.mem.(i) with
  | Insn i -> i
  | _ -> raise (Invalid_argument "get_insn")

let mach_error (m: mach) (highlight : string) (msg : string) : 'a =
  if highlight <> "_start" then 
    let insn = get_insn m m.pc in
    let str_insn = Arm_stringifier.string_of_insn insn in
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") str_insn in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mat address \x1b[1;97m0x%x\x1b[0m:" (m.pc |> Int64.to_int) in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (Segmentation_fault msg)
  else
    let highlighted_line = Str.global_replace (Str.regexp highlight) ("\x1b[1;91m" ^ highlight ^ "\x1b[0m") "No label '_start' defined!" in
    let () = Printf.fprintf Out_channel.stderr "\x1b[1;91mSyntax error \x1b[0mat address\x1b[1;97m0x%x\x1b[0m:" (m.pc |> Int64.to_int) in
    let () = Printf.fprintf Out_channel.stderr " %s '%s'.\n\n" msg highlight in
    let () = Printf.fprintf Out_channel.stderr "\t%s\n\n" highlighted_line in
    raise (No_entrypoint)

let gen_layout (m: mach) (prog: Arm.prog) : (string * int64) list =
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
      | Arm.Word _ -> 4L 
      | Arm.Byte _ -> 1L
      | Arm.QuadArr arr -> List.length arr |> Int64.of_int |> Int64.mul 8L
      | Arm.WordArr arr -> List.length arr |> Int64.of_int |> Int64.mul 4L 
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
  List.map (fun (label, offset) -> (label, Int64.add offset m.info.mem_bot)) pre_obfuscation

let rec lookup_label (layout: (string * int64) list) (label: string) : int64 =
  match layout with
  | [] -> raise (Segmentation_fault ("Label '" ^ label ^ "' not found"))
  | (l, offset)::t -> if l = label then offset else lookup_label t label

let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

let sbytes_of_int32 (i:int32) : sbyte list =
  let open Char in 
  let open Int32 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffl |> to_int |> chr))
           [0; 8; 16; 24]

let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

let int32_of_sbytes (bs:sbyte list) : int32 = 
  let open Char in
  let open Int32 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0l
  in
  List.fold_right f bs 0l

let build_program (prog: Arm.prog) : sbyte list =
  let build_insn (insn: Arm.insn) : sbyte list = [Insn insn; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill] in
  let build_global_def (label: Arm.lbl) : sbyte list = [GlobalDef label; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill; InsFill] in
  let build_data (data: Arm.data) : sbyte list = 
    match data with
    | Arm.Quad n -> sbytes_of_int64 n
    | Arm.Byte c -> [Byte (c |> Char.chr)]
    | Arm.QuadArr arr -> List.concat (List.map (fun n -> sbytes_of_int64 n) arr)
    | Arm.ByteArr arr -> List.map (fun c -> Byte (c |> Char.chr)) arr
    | Arm.Word n -> sbytes_of_int32 n 
    | Arm.WordArr arr -> List.concat (List.map (fun n -> sbytes_of_int32 n) arr)
  in 
  let build_block (block: Arm.block) : sbyte list =
    match block with
    | {entry=_; lbl=_; asm=Arm.Text insns} -> List.concat (List.map build_insn insns)
    | {entry=_; lbl=_; asm=Arm.Data data} -> List.concat (List.map build_data data)
  in 
  let build_directive (dir: Arm.tld) : sbyte list =
    match dir with
    | Arm.GloblDef l -> build_global_def l
    | Arm.ExternSym s -> [ExternSym s]
    | Arm.TextDirect blocks
    | Arm.DataDirect blocks -> List.concat (List.map build_block blocks)
  in
  List.concat (List.map build_directive prog)

let init (prog: Arm.prog) (mem_bot: int64 option) (mem_size: int option) (exit_val: int64 option) (entry_label: string option) : mach = 
  let u_mem_bot = match mem_bot with | Some x -> x | None -> 0x400000L in 
  let u_mem_size = match mem_size with | Some x -> x | None -> 0x10000 in 
  let u_mem_top = Int64.add u_mem_bot (u_mem_size |> Int64.of_int) in 
  let u_exit_val = match exit_val with | Some x -> x | None -> 0xfdeadL in 
  let nregs = 32 in 
  let u_entry_label = match entry_label with | Some x -> x | None -> "_start" in
  let minfo = {
    mem_bot = u_mem_bot;
    mem_size = u_mem_size |> Int64.of_int;
    mem_top = u_mem_top;
    nregs = nregs;
    exit_val = u_exit_val;
    entry = (u_entry_label, 0L);
    layout = [];
  } in
  let program_bytes = build_program prog |> Array.of_list in
  let mem = Array.make u_mem_size InsFill in 
  Array.blit program_bytes 0 mem 0 (Array.length program_bytes);
  let regs = Array.make nregs 0L in
  let tmp_mach = { info = minfo; regs; pc = 0L; mem; flags = { n = false; z = false; c = false; v = false; } } in 
  let rec get_entry_addr (map: (string * int64) list) : int64 =
    match map with
    | [] -> mach_error tmp_mach u_entry_label "Entry point not defined"
    | (l, offset)::t -> if l = u_entry_label then offset else get_entry_addr t
  in
  let layout = gen_layout tmp_mach prog in
  regs.(reg_index Arm.LR) <- u_exit_val;
  regs.(reg_index Arm.SP) <- u_mem_top;
  let m = { info = { minfo with layout = layout; entry = (u_entry_label, get_entry_addr layout) };
    regs = regs;
    pc = (get_entry_addr layout);
    mem = mem;
    flags = { n = false; z = false; c = false; v = false; }
  } in 
  let sub_pc = Int64.sub m.pc m.info.mem_bot in 
  m.pc <- sub_pc;
  m

let print_machine_info (m: mach) : unit =
  let layout_str = List.map (fun (label, addr) -> "\t" ^ label ^ " @ " ^ (Int64.to_string addr)) m.info.layout |> String.concat "\n" in
  print_endline ("mem_bot = " ^ (Int64.to_string m.info.mem_bot));
  print_endline ("mem_size = " ^ (Int64.to_string m.info.mem_size));
  print_endline ("mem_top = " ^ (Int64.to_string m.info.mem_top));
  print_endline ("nregs = " ^ (Int.to_string m.info.nregs));
  print_endline ("exit_val = " ^ (Int64.to_string m.info.exit_val));
  print_endline ("entry = {" ^ (fst m.info.entry) ^ " @ " ^ (Int64.to_string (snd m.info.entry)) ^ "}");
  print_endline ("layout = [\n" ^ layout_str ^ "\n]")

let print_machine_state (m: mach) : unit = 
  let regs_string = Array.map Int64.to_string m.regs |> Array.to_list |> String.concat "; " in
  let n_flag = "n -> " ^ (Bool.to_string m.flags.n) in 
  let z_flag = "n -> " ^ (Bool.to_string m.flags.z) in 
  let c_flag = "n -> " ^ (Bool.to_string m.flags.c) in 
  let v_flag = "n -> " ^ (Bool.to_string m.flags.v) in 
  let flags_string = n_flag ^ "; " ^ z_flag ^ "; " ^ c_flag ^ "; " ^ v_flag in
  print_endline ("regs = [" ^ regs_string ^ "]");
  print_endline ("pc = " ^ (Int64.to_string m.pc));
  print_endline ("flags = {" ^ flags_string ^ "}")

let print_sbyte_array (mem: sbyte array) (max_rows: int) : unit =
  let max_rows = max_rows * 8 in
  print_char '[';
  let print_byte byte = 
    match byte with
    | InsFill -> Printf.printf "InsFill, "
    | GlobalDef l -> Printf.printf "GlobalDef %s" l
    | ExternSym s -> Printf.printf "ExternSym %s, " s
    | Insn i -> Printf.printf "Insn %s, " (Arm_stringifier.ast_string_of_insn i)
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

(* default machine has
 mem_bot = 0x400000L
 mem_size = 0x10000L
 mem_top = 0x410000L
 nregs = 32
 insn_size = 8L
 exit_val = 0xfdeadL
 *)
