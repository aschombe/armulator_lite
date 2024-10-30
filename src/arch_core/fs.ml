open Parsing

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

let read_bytes (offset: int) (eof: int) (count: int) (contents: Mach.sbyte array): (int * Mach.sbyte array) =
  let rec get_bytes (offset: int) (count: int) (acc: int) (contents: Mach.sbyte array): Mach.sbyte list = 
    match count with 
    | 0 -> []
    | c -> if offset + acc <= eof then Array.get contents (offset + acc) :: get_bytes offset (c - 1) (acc + 1) contents else []
  in
  let bl = get_bytes offset count 0 contents |> Array.of_list in 
  (Array.length bl, bl)

let read_to_eof (eof: int) (contents: Mach.sbyte array) : Mach.sbyte array = 
  let rec get_bytes (eof: int) (acc: int) (contents: Mach.sbyte array): Mach.sbyte list = 
    match acc with 
    | v when v = eof -> []
    | _ -> if acc <= eof then Array.get contents (acc) :: get_bytes eof (acc + 1) contents else []
  in
  let bl = get_bytes eof 0 contents |> Array.of_list in 
  bl

let rec contents_to_char_list (contents: Mach.sbyte list) : char list = 
  match contents with 
  | [] -> []
  | (Byte x)::t -> x :: contents_to_char_list t
  | _ -> []

let open_file (file: string) : Mach.fd_entry =
  if Sys.file_exists file then begin 
    let contents = read_lines file |> String.concat "\n" in 
    let b_arr = Mach.sbyte_array_of_string contents in
    let opened_file = (!curr_fd, 0, Array.length b_arr, b_arr) in
    curr_fd := !curr_fd + 1; opened_file
  end else begin
    let opened_file = (!curr_fd, 0, 0, Mach.sbyte_array_of_size max_file_sz) in
    curr_fd := !curr_fd + 1; opened_file
  end

let close_file (m: Mach.t) (fd: int) : Mach.fd_entry list =
  List.filter (fun (tfd, _, _, _) -> not (fd = tfd)) m.fd_table

let fs_sync (m: Mach.t) : Mach.t = 
  let rec name_of_fd (fd: int) (tbl: Mach.fd_mapping list) : string = 
    match tbl with 
    | [] -> ""
    | (ffd, name, _)::t -> if fd = ffd then name else name_of_fd fd t in
  let files = List.map (fun (fd, _offset, eof, contents) -> (fd, name_of_fd fd m.info.fd_map, eof, read_to_eof eof contents)) m.fd_table in 
  let sync_file (fd: Mach.fd) (name: string) (last_sync: Mach.offset) (contents: Mach.sbyte array) : Mach.fd_mapping =
    let c_str = Array.to_list contents |> contents_to_char_list |> List.to_seq |> String.of_seq in
    let i_str = Array.to_list contents |> contents_to_char_list |> List.filteri (fun i _ -> i >= last_sync) |> List.to_seq |> String.of_seq in
    match name with 
    | "__internal_stdin" -> (fd, name, last_sync) (* you cant write to stdin, so just return existing information *)
    | "__internal_stdout" -> Printf.fprintf stdout "%s" i_str; (fd, name, last_sync + (String.length i_str))
    | "__internal_stderr" -> Printf.fprintf stderr "%s" i_str; (fd, name, last_sync + (String.length i_str))
    | n -> write_file n c_str; (fd, name, String.length c_str)
  in 
  let n_fd_map = List.map2 (fun (fd, _offset, _eof, contents) (_fd, name, last_sync) -> sync_file fd name last_sync contents) files m.info.fd_map in 
  m.info.fd_map <- n_fd_map;
  m