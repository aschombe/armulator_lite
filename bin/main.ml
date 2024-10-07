(* default program args
  debug = false
  validate = false
  write_arm = false

  mem_bot = 0x400000L 
  mem_size = 0x10000L 
  mem_top = 0x410000L
  exit_val = 0xfdeadL
  entry_label = "_start" *)

let debug = ref false 
let debugger = ref false
let print_ast = ref false
let print_machine_state = ref false
let print_machine_info = ref false
let input_file = ref "" 
let plugin_list = ref ""
let output_file = ref ""

let mem_bot = ref 0x400000
let mem_size = ref 0x10000 
let exit_val = ref 0xfdead 
let entry_label = ref "_start" 

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

let write_file (file:string) (contents: string) : unit =
  let channel = open_out file in
  output_string channel contents;
  close_out channel

let _debug lines = 
  let text_directives = Arm_parser.find_directives lines "text" in 
  let data_directives = Arm_parser.find_directives lines "data" in 
  print_endline "Text Directives:";
  List.iter (fun codelines -> Arm_parser.print_code_lines codelines) text_directives;
  print_endline "Data Directives:";
  List.iter (fun codelines -> Arm_parser.print_code_lines codelines) data_directives

let main (_plugins: (module Plugins.EMULATOR_PLUGIN) list) (lines: Arm_parser.code_line list) =
  let prog = Arm_parser.parse_assembly lines in
  if !output_file <> "" then write_file !output_file (Arm_stringifier.string_of_prog prog);

  let _stringified = Arm_stringifier.ast_string_of_prog prog in 
  (if !debugger then print_machine_info := true; print_machine_state := true);
  let m = Mach.init prog (Some(!debugger)) (Some(!print_machine_state)) (Some(!mem_bot |> Int64.of_int)) (Some(!mem_size)) (Some(!exit_val |> Int64.of_int)) (Some(!entry_label)) in
  if !print_machine_info then Mach.print_machine_info m;
  if !print_machine_state then Mach.print_machine_state m;
  if !debug then _debug lines; 
  if !print_ast then print_endline (Arm_stringifier.ast_string_of_prog prog);
  if !debugger then Emulator.debug m else Emulator.run m

let load_plugin (name: string) : unit = 
  let fname = Dynlink.adapt_filename name in 
  match Sys.file_exists fname with 
  | true -> 
    begin match Dynlink.loadfile fname with 
    | () -> ()
    | exception (Dynlink.Error err) -> failwith (Dynlink.error_message err)
    end
  | false -> failwith "plugin does not exist!"

let () = 
  let args =
  [ ("--debug-info", Cmd_parser.Set_bool debug, "Print debug information");
    ("--print-ast", Cmd_parser.Set_bool print_ast, "Print the AST of the assembly program");
    ("--print-mach-state", Cmd_parser.Set_bool print_machine_state, "Print the machine state");
    ("--print-mach-info", Cmd_parser.Set_bool print_machine_info, "Print the machine starting information");
    ("--debugger", Cmd_parser.Set_bool debugger, "Enable the emulator debugger (implies all --print-mach flags)");
    ("--file", Cmd_parser.Set_string input_file, "Input assembly file");
    ("--base-addr", Cmd_parser.Set_int (mem_bot), "Base memory address");
    ("--stack-size", Cmd_parser.Set_int (mem_size), "Program stack size");
    ("--exit-val", Cmd_parser.Set_int (exit_val), "End program when pc is this value");
    ("--entry-label", Cmd_parser.Set_string entry_label, "Entry label");
    ("--plugins", Cmd_parser.Set_string plugin_list, "Comma separated list of plugins to load");
    ("--help", Cmd_parser.Usage_msg, "Displays this message");
  ] in
  Cmd_parser.parse_arguments (Sys.argv |> Array.to_list) args;
  let plugin_names = (String.split_on_char ',' !plugin_list) |> List.filter (fun n -> not (n = "")) in
  List.iter (fun name -> begin Printf.printf "[plugin_loader] loading '%s'...%!" name; load_plugin (name^".cmxs"); Printf.printf "done.\n%!" end) plugin_names;
  let plugins = Plugins.get_loaded_plugins () in
  List.iter (fun pl -> let module M = (val pl : Plugins.EMULATOR_PLUGIN) in Cmd_parser.parse_arguments (Sys.argv |> Array.to_list) M.options; M.on_load ()) plugins; 
  let lines = read_file !input_file in 
  main plugins lines