open Arch_core
open Parsing

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
let plugin_list = ref ""
let output_file = ref ""

let mem_bot = ref 0x400000
let mem_size = ref 0x10000
let exit_val = ref 0xfdead
let entry_label = ref "_start"
let stdin = ref ""
let stdin_contents = ref ""

let _debug lines =
  let text_directives = Arm_parser.find_directives lines "text" in
  let data_directives = Arm_parser.find_directives lines "data" in
  print_endline "Text Directives:";
  List.iter (fun codelines -> Arm_parser.print_code_lines codelines) text_directives;
  print_endline "Data Directives:";
  List.iter (fun codelines -> Arm_parser.print_code_lines codelines) data_directives

let main (plugins: (module Plugins.EMULATOR_PLUGIN) list) (lines: Arm_parser.code_line list) =
  let prog = Arm_parser.parse_assembly lines in
  if !output_file <> "" then Fs.write_file !output_file (Arm_stringifier.string_of_prog prog);

  let _stringified = Arm_stringifier.ast_string_of_prog prog in
  (if !debugger then (print_machine_info := true; print_machine_state := true));
  (if !stdin <> "" then stdin_contents := String.concat "\n" (Fs.read_lines !stdin));
  let m = Mach.init prog (Some(!debugger)) (Some(!print_machine_state)) (Some(!mem_bot |> Int64.of_int)) (Some(!mem_size)) (Some(!exit_val |> Int64.of_int)) (Some(!entry_label)) (Some(!stdin_contents)) in
  List.iter (fun pl -> let module M = (val pl : Plugins.EMULATOR_PLUGIN) in Cmd_parser.parse_arguments (Sys.argv |> Array.to_list |> List.tl) M.options; let _ = M.on_load m in ()) plugins;
  if !print_machine_info then Mach.print_machine_info m;
  if !print_machine_state then Mach.print_machine_state m;
  if !debug then _debug lines;
  if !print_ast then print_endline (Arm_stringifier.ast_string_of_prog prog);
  if !debugger then Emulator.debug m else Emulator.run m

let load_plugin (name: string) : unit =
  let fname = Dynlink.adapt_filename ("./plugins/" ^ name ^ ".cmxs") in
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
    ("--base-addr", Cmd_parser.Set_int mem_bot, "Base memory address");
    ("--stack-size", Cmd_parser.Set_int mem_size, "Program stack size");
    ("--exit-val", Cmd_parser.Set_int exit_val, "End program when pc is this value");
    ("--entry-label", Cmd_parser.Set_string entry_label, "Entry label");
    ("--plugins", Cmd_parser.Set_string plugin_list, "Comma separated list of plugins to load");
    ("--stdin", Cmd_parser.Set_string stdin, "File that contains stdin contents");
    ("--help", Cmd_parser.Usage_msg, "Displays this message");
  ] in
  let files = Cmd_parser.parse_cmd_arguments (Sys.argv |> Array.to_list |> List.tl) args in
  let plugin_names = (String.split_on_char ',' !plugin_list) |> List.filter (fun n -> not (n = "")) in
  List.iter (fun name -> begin Printf.printf "[plugin_loader] loading '%s'...%!" name; load_plugin name; Printf.printf "done.\n%!" end) plugin_names;
  let plugins = Plugins.get_loaded_plugins () in
  let lines = List.fold_left (fun l f -> l @ Fs.read_asm_file f) [] files in
  main plugins lines
