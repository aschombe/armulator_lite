let compared = ref ""

let base_ran : bool ref = ref false
let can_start : bool ref = ref false
let base_program : Mach.t list ref = ref []
let compare_program : Mach.t list ref = ref []

let base_used_registers : Arm.reg list ref = ref [] 
let comp_used_registers : Arm.reg list ref = ref [] 

type match_window = (Arm.reg * int64 * int64)
type reg_matches = (Arm.reg * match_window list)

(* let int_to_reg (i: int) : Arm.reg = Arm_parser.register_of_string (0, "") ("x" ^ (string_of_int i)) *)

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let rec get_deepest_equal (rl1: int64 list) (rl2: int64 list) : int64 list = 
  match rl1, rl2 with 
  | [], [] -> []
  | _, [] -> []
  | [], _ -> []
  | h1::t1, h2::t2 -> if (Int64.equal h1 h2) then h1 :: (get_deepest_equal t1 t2) else []

let rec collect_register_delta (ml: Mach.t list) (reg: Arm.reg) : int64 list =
  match ml with
  | [] -> []
  | h::t -> h.regs.(Mach.reg_index reg) :: (collect_register_delta t reg) 

let find_all_matches (reg: Arm.reg) (base_delta: (Arm.reg * int64 list)) (comp_deltas: (Arm.reg * int64 list) list) : reg_matches =
  let deepest_equals = List.map (fun (r, l) -> (r, get_deepest_equal (snd base_delta) l)) comp_deltas in 
  let ordered_by_match_count = List.sort (fun (_, a) (_, b) -> (List.length a) - (List.length b)) deepest_equals in 
  let discard_unused = List.filter (fun (_r, l) -> (List.length l > 5) && (List.length l <= List.length (snd base_delta))) ordered_by_match_count in 
  let as_match_window = List.map (fun (r, l) -> (r, 0L, List.length l |> Int64.of_int)) discard_unused in
  (reg, as_match_window)

let find_match_ratio ((reg, mtchs): reg_matches) (max_step: int) : (Arm.reg * Arm.reg * float) = 
  let (r, highest_match_len) = List.map (fun (r,s,e) -> (r, Int64.sub e s |> Int64.to_int)) mtchs |> List.fold_left (fun (ra, la) (r, l) -> if l > la then (r, l) else (ra, la)) (reg, 0) in 
  (reg, r, (float_of_int highest_match_len) /. (float_of_int max_step))

let rec find_match_ratios (mtchs: reg_matches list) (max_step: int) : (Arm.reg * Arm.reg * float) list = 
  match mtchs with 
  | [] -> []
  | h::t -> find_match_ratio h max_step :: find_match_ratios t max_step

let print_used_registers (rgs: Arm.reg list) : unit = 
  Printf.printf "[plagiarism] [%s]\n" (List.map (fun r -> Arm_stringifier.string_of_reg r) rgs |> String.concat ",")

let rec print_possible_match_ratios (matches: (Arm.reg * Arm.reg * float) list) : unit = 
  match matches with 
  | [] -> ()
  | (br, cr, ratio)::t -> Printf.printf "[plagiarism] %s -> %s (%d%%)\n" (Arm_stringifier.string_of_reg br) (Arm_stringifier.string_of_reg cr) ((ratio *. 100.0) |> int_of_float); print_possible_match_ratios t

let run_plagiarism_detection (base: Mach.t list) (comp: Mach.t list) : unit =
  base_used_registers := !base_used_registers |> remove_duplicates;
  comp_used_registers := !comp_used_registers |> remove_duplicates;
  print_used_registers !base_used_registers;
  print_used_registers !comp_used_registers;
  Printf.printf "[plagiarism] \"base\" used %d registers, \"comp\" used %d registers\n" (List.length !base_used_registers) (List.length !comp_used_registers);
  Printf.printf "[plagiarism] comparing %d snapshots of \"base\" against %d snapshots of \"comp\"...\n" (List.length base) (List.length comp);
  let base_register_deltas = List.map (fun r -> (r, collect_register_delta base r)) !base_used_registers in 
  let comp_register_deltas = List.map (fun r -> (r, collect_register_delta comp r)) !comp_used_registers in
  let base_step_count = List.nth base_register_deltas 0 |> snd |> List.length in
  let all_possible_matches = List.mapi (fun i ds -> find_all_matches (List.nth !base_used_registers i) ds comp_register_deltas) base_register_deltas in 
  let ratios = find_match_ratios all_possible_matches base_step_count in 
  let total_score = List.fold_left (fun acc (_, _, e) -> acc +. e) 0.0 ratios in
  let average = total_score /. (float_of_int (List.length !base_used_registers)) in 
  print_possible_match_ratios ratios;
  Printf.printf "[plagiarism] total score = %d%%\n" ((average *. 100.0) |> int_of_float)

let read_file (file:string) : Arm_parser.code_line list =
  let lines = ref [] in
  let ln = ref 1 in
  let channel = open_in file in
  try while true; do
    lines := (!ln, input_line channel) :: !lines;
    ln := !ln + 1
  done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let start_compare_program (m: Mach.t) : unit =
  let lines = read_file !compared in
  let prog = Arm_parser.parse_assembly lines in
  let _stringified = Arm_stringifier.ast_string_of_prog prog in 
  let m' = Mach.init prog (Some(m.opts.debugger)) (Some(m.opts.print_machine_state)) (Some(m.info.mem_bot)) (Some(m.info.mem_size |> Int64.to_int)) (Some(m.info.exit_val)) (Some(m.info.entry |> fst)) in
  List.iter (fun pl -> let module M = (val pl : Plugins.EMULATOR_PLUGIN) in Cmd_parser.parse_arguments (Sys.argv |> Array.to_list) M.options; let _ = M.on_load m in ()) (Plugins.get_loaded_plugins ()); 
  if m'.opts.print_machine_state then Mach.print_machine_state m';
  if m'.opts.debugger then Emulator.debug m' else Emulator.run m'; can_start := true

let on_exit_restart_or_check (m: Mach.t) : unit =
  match !base_ran with
  | true -> Printf.printf "[plagiarism] ran comparison file\n"
  | false -> base_ran := true; base_program := !base_program @ [m]; start_compare_program m

let collect_data (m: Mach.t): Mach.t = 
  begin match !base_ran with
  | true -> compare_program := !compare_program @ [Mach.copy m]
  | false -> base_program := !base_program @ [Mach.copy m]
  end; m

let rec get_registers (m: Mach.t) (opl: Arm.operand list) : Arm.reg list = 
  match opl with
  | [] -> [] 
  | h::t -> 
    try 
      let r = Decoder.operand_as_register_quiet m h in r :: get_registers m t 
    with _ -> get_registers m t

let collect_used_registers (m: Mach.t): Mach.t =
  let insn = Mach.get_insn m m.pc in
  let all_regs = begin match insn with 
  | (_, argl) -> let used = get_registers m argl in used
  end in 
  begin match !base_ran with
  | true -> comp_used_registers := !comp_used_registers @ all_regs
  | false -> base_used_registers := !base_used_registers @ all_regs
  end; m

module M: Plugins.EMULATOR_PLUGIN = struct 
  let name = "plagiarism"
  let options = [
    ("--compare-with", Cmd_parser.Set_string compared, "Compare the base file with this one")
  ]
  let on_load = fun m -> if not !base_ran then Printf.printf "[plagiarism] checking base file against '%s'\n" !compared; m
  let on_unload = fun m -> if !base_ran && !can_start then run_plagiarism_detection !base_program !compare_program; m
  let on_exit = fun m -> on_exit_restart_or_check m; m

  let on_pre_execute = fun m -> collect_used_registers m
  let on_post_execute = fun m -> collect_data m
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)
