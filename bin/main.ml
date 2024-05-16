let read_file (file:string) : Parser.code_line list =
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

let _debug lines = 
  let text_directives = Parser.find_directives lines "text" in 
  let data_directives = Parser.find_directives lines "data" in 
  print_endline "Text Directives:";
  List.iter (fun codelines -> Parser.print_code_lines codelines) text_directives;
  print_endline "Data Directives:";
  List.iter (fun codelines -> Parser.print_code_lines codelines) data_directives

let main lines =
  let prog = Parser.parse_assembly lines in
  let _stringified = Arm.ast_string_of_prog prog in 
  let m = Mach.init prog in
  Mach.print_sbyte_array m.mem 20

let () = 
  let filename = Sys.argv.(1) in
  let lines = read_file filename in
  main lines
