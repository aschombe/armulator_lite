open Printf 

let read_file (file:string) : string list =
  let lines = ref [] in
  let channel = open_in file in
  try while true; do
      lines := input_line channel :: !lines
  done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let write_file (file:string) (out:string) =
  let channel = open_out file in
  fprintf channel "%s" out;
  close_out channel

let parse_asm_file (filename : string) : unit =

  let stream = read_file filename
  in
  () 

let () = 
  let filename = Sys.argv.(1) in
  let _ = parse_asm_file filename in
  ()
