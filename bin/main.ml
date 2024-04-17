let read_file (file:string) : (int * string) list =
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

let () = 
  let filename = Sys.argv.(1) in
  let lines = read_file filename in
  let parsed_insns = Parser.parse_assembly lines in
  let stringified = Arm.ast_string_of_prog parsed_insns in 
  print_endline stringified
