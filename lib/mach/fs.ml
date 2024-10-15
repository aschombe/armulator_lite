let curr_fd = ref 3
let max_file_sz = 0x10000

let read_asm_file (file:string) : Arm_parser.code_line list =
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

let read_lines (file:string) : string list =
  let lines = ref [] in
  let channel = open_in file in
  try while true; do
    lines := input_line channel :: !lines
  done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let write_file (file:string) (contents: string) : unit =
  let channel = open_out file in
  output_string channel contents;
  close_out channel

let write_bytes (bl: Mach.sbyte array) (offset: int) (count: int) (contents: Mach.sbyte array): (int * Mach.sbyte array) = 
  Array.iteri (fun (i: int) (byte: Mach.sbyte) -> if i <= count then Array.set contents (offset + i) byte) bl;
  (count, bl)

let read_bytes (offset: int) (count: int) (contents: Mach.sbyte array): (int * Mach.sbyte array) =
  let rec get_bytes (offset: int) (count: int) (acc: int) (contents: Mach.sbyte array): Mach.sbyte list = 
    match count with 
    | 0 -> []
    | c -> Array.get contents (offset + acc) :: get_bytes offset (c - 1) (acc + 1) contents
  in
  let bl = get_bytes offset count 0 contents |> Array.of_list in 
  (Array.length bl, bl)

let open_file (file: string) : (int * int * Mach.sbyte array) =
  if Sys.file_exists file then begin 
    let contents = read_lines file |> String.concat "\n" in 
    let opened_file = (!curr_fd, 0, Mach.sbyte_array_of_string contents) in
    curr_fd := !curr_fd + 1; opened_file
  end else begin
    let opened_file = (!curr_fd, 0, Mach.sbyte_array_of_size max_file_sz) in
    curr_fd := !curr_fd + 1; opened_file
  end

let close_file (m: Mach.t) (fd: int) : (int * int * Mach.sbyte array) list =
  List.filter (fun (tfd, _, _) -> not (fd = tfd)) m.fd_table

(* todo: write sync function to save the contents of the fds to a file upon their respective system cal l*)