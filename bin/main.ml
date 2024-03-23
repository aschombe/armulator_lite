open Printf 

let read_file (file:string) : string =
  let lines = ref [] in
  let channel = open_in file in
  try while true; do
      lines := input_line channel :: !lines
  done; ""
  with End_of_file ->
    close_in channel;
    String.concat "\n" (List.rev !lines)

let write_file (file:string) (out:string) =
  let channel = open_out file in
  fprintf channel "%s" out;
  close_out channel
let parse_asm_file filename =

  let lexbuf = read_file filename |> 
               Lexing.from_string
  in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Parser.Error -> failwith @@ Printf.sprintf "Parse error"

let () = 
  let filename = Sys.argv.(1) in
  let ast = parse_asm_file filename in
  let out = Ast.string_of_prog ast in
  write_file (filename ^ ".ast") out
