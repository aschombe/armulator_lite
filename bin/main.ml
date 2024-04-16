let read_file (file:string) : string list =
  let lines = ref [] in
  let channel = open_in file in
  try while true; do
      lines := input_line channel :: !lines
  done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let () = 
  let filename = Sys.argv.(1) in
  let lines = read_file filename in
  let parsed_insns = Parser.parse_assembly lines in
  let stringified = Arm.string_of_prog parsed_insns in 
  print_endline stringified
