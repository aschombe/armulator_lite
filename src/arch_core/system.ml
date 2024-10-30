exception Invalid_syscall of int64

type syscall = SysWrite | SysRead | SysExit | SysOpen | SysClose | SysLseek
let number_to_syscall = function 
| 63L -> SysRead
| 64L -> SysWrite
| 93L -> SysExit
| 55L -> SysOpen
| 56L -> SysClose 
| 62L -> SysLseek
| n -> raise (Invalid_syscall n)

let syscall_read (m: Mach.t) (fd: int64) (buf: int64) (count: int64) : (Mach.t * int64) =
  let rec find_file (fd: int) (tbl: Mach.fd_entry list): Mach.fd_entry = 
    match tbl with 
    | [] -> (-1, -1, -1, Array.of_list [])
    | (tfd, offset, eof, contents)::t -> if tfd = fd then (tfd, offset, eof, contents) else find_file fd t
  in
  let (_fd, f_offset, f_eof, f_contents) = find_file (Int64.to_int fd) m.fd_table in 
  let (f_count, bytes) = Fs.read_bytes f_offset f_eof (Int64.to_int count) f_contents in 
  let mem = Memory.write_bytes (Mach.map_addr m buf) f_count (Array.to_list bytes) m.mem in 
  m.mem <- mem; 
  (m, f_count |> Int64.of_int)

let syscall_write (m: Mach.t) (fd: int64) (buf: int64) (count: int64) : (Mach.t * int64) =
  let bytes = Memory.read_bytes (Mach.map_addr m buf) (Int64.to_int count) m.mem |> Array.of_list in 
  let rec find_file (fd: int) (tbl: Mach.fd_entry list): Mach.fd_entry = 
    match tbl with 
    | [] -> (-1, -1, -1, Array.of_list [])
    | (tfd, offset, eof, contents)::t -> if tfd = fd then (tfd, offset, eof, contents) else find_file fd t
  in
  let rec update_file (fd: int) (offset: int) (eof: int) (contents: Mach.sbyte array) (tbl: Mach.fd_entry list): Mach.fd_entry list =
    match tbl with 
    | [] -> []
    | (tfd, old_offset, old_eof, old_contents)::t -> if tfd = fd then (tfd, offset, eof, contents) :: update_file fd offset eof contents t else (tfd, old_offset, old_eof, old_contents) :: update_file fd offset eof contents t
  in
  let (fd, offset, eof, contents) = find_file (Int64.to_int fd) m.fd_table in 
  if fd = -1 then (Fs.fs_sync m, -1L) else begin 
    let (f_offset, f_contents) = Fs.write_bytes bytes offset (Int64.to_int count) contents in
    let updated_fd_table = update_file fd f_offset (max eof f_offset) f_contents m.fd_table in 
    m.fd_table <- updated_fd_table;
    (Fs.fs_sync m, count)
  end

let syscall_open (m: Mach.t) (_dfd: int64) (p_filename: int64) (_flags: int64) (_mode: int64) : (Mach.t * int64) = 
  let bytes = Memory.read_to_null_terminator (Mach.map_addr m p_filename) m.mem in 
  let fname = Mach.string_of_sbytes bytes in
  let (fd, offset, eof, contents) = Fs.open_file fname in 
  m.info.fd_map <- m.info.fd_map @ [(fd, fname, offset)];
  m.fd_table <- m.fd_table @ [(fd, offset, eof, contents)];
  (m, Int64.of_int fd)

let syscall_close (m: Mach.t) (fd: int64) : (Mach.t * int64) = 
  let n_fd_table = Fs.close_file m (Int64.to_int fd) in 
  m.fd_table <- n_fd_table;
  (m, fd)

let syscall_lseek (m: Mach.t) (fd: int64) (offset: int64) (whence: int64) : (Mach.t * int64) = 
  let rec find_offset_of (fd: int) (tbl: (int * int * int* Mach.sbyte array) list): int = 
    match tbl with 
    | [] -> -1
    | (tfd, offset, _, _)::t -> if tfd = fd then offset else find_offset_of fd t
  in 
  let rec update_offset_of (fd: int) (n_offset: int) (tbl: Mach.fd_entry list): Mach.fd_entry list = 
    match tbl with 
    | [] -> []
    | (tfd, offset, eof, contents)::t -> if tfd = fd then (tfd, n_offset, eof, contents) :: update_offset_of fd n_offset t else (tfd, offset, eof, contents) :: update_offset_of fd n_offset t
  in 
  let curr_offset = find_offset_of (Int64.to_int fd) m.fd_table in 
  let new_offset = begin match whence with 
    (* SEEK SET *)
    | 0L -> offset |> Int64.to_int
    (* SEEK CUR *)
    | 1L -> (offset |> Int64.to_int) + curr_offset
    (* SEEK END *)
    | 2L -> (offset |> Int64.to_int) + Mach.max_file_sz
    | _ -> -1
  end in 
  let updated_fd_table = update_offset_of (Int64.to_int fd) new_offset m.fd_table in 
  m.fd_table <- updated_fd_table;
  (m, Int64.of_int new_offset)


let syscall_exit (m: Mach.t) (status: int64) : int64 =
  m.pc <- m.info.exit_val;
  status 

let execute_syscall (m: Mach.t) : Mach.t =
  let syscall_exec = number_to_syscall m.regs.(Mach.reg_index X8) in 
  let arg0 = m.regs.(Mach.reg_index X0) in 
  let arg1 = m.regs.(Mach.reg_index X1) in 
  let arg2 = m.regs.(Mach.reg_index X2) in 
  let arg3 = m.regs.(Mach.reg_index X3) in 
  let _arg4 = m.regs.(Mach.reg_index X4) in
  let _arg5 = m.regs.(Mach.reg_index X5) in 
  match syscall_exec with 
  | SysRead -> let (m', result) = syscall_read m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m'
  | SysWrite -> let (m', result) = syscall_write m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m'
  | SysExit -> let result = syscall_exit m arg0 in m.regs.(Mach.reg_index X0) <- result; let m' = Plugins.execute_plugin_event Plugins.OnExitEvent m in m'
  | SysOpen -> let (m', result) = syscall_open m arg0 arg1 arg2 arg3 in m'.regs.(Mach.reg_index X0) <- result; m'
  | SysClose -> let (m', result) = syscall_close m arg0 in m'.regs.(Mach.reg_index X0) <- result; m'
  | SysLseek -> let (m', result) = syscall_lseek m arg0 arg1 arg2 in m'.regs.(Mach.reg_index X0) <- result; m'

let rec __clib_printf (m: Mach.t) (fmt: char list) (args: int64 list) : unit =
  match fmt, args with
  | [], [] -> () 
  | _, [] -> Printf.printf "(%s)" (String.of_seq (List.to_seq fmt)) 
  | ('%'::'d'::f), (arg::rest_args) ->
    Printf.printf ("%d") (arg |> Int64.to_int);
    __clib_printf m f rest_args
  | ('%'::'l'::'d'::f), (arg::rest_args) ->
    Printf.printf ("%ld") (arg |> Int64.to_int32);
    __clib_printf m f rest_args
  | ('%'::'l'::'u'::f), (arg::rest_args) ->
    Printf.printf ("%lu") (arg |> Int64.to_int32);
    __clib_printf m f rest_args
  | ('%'::'l'::'l'::'d'::f), (arg::rest_args) ->
    Printf.printf ("%lld") (arg |> Int64.to_int);
    __clib_printf m f rest_args
  | ('%'::'l'::'l'::'u'::f), (arg::rest_args) ->
    Printf.printf ("%llu") (arg |> Int64.to_int);
    __clib_printf m f rest_args
  | ('%'::'c'::f), (arg::rest_args) ->
    Printf.printf ("%c") (arg |> Int64.to_int |> Char.chr);
    __clib_printf m f rest_args
  | ('%'::'s'::f), (arg::rest_args) ->
    let str = Memory.read_to_null_terminator (Mach.map_addr m arg) m.mem |> Mach.string_of_sbytes in
    Printf.printf "%s" str;
    __clib_printf m f rest_args
  | (c::f), _ -> 
    Printf.printf "%c" c;
    __clib_printf m f args
  | _ -> ()
  
let clib_printf (m: Mach.t) (fmt_str: string) (args: int64 list) =
  __clib_printf m (String.to_seq fmt_str |> List.of_seq) args

let execute_extern_function (m: Mach.t) (lbl: string) : Mach.t =
  let arg0 = m.regs.(Mach.reg_index X0) in 
  let arg1 = m.regs.(Mach.reg_index X1) in 
  let arg2 = m.regs.(Mach.reg_index X2) in 
  let arg3 = m.regs.(Mach.reg_index X3) in 
  let arg4 = m.regs.(Mach.reg_index X4) in 
  let arg5 = m.regs.(Mach.reg_index X5) in
  let arg6 = m.regs.(Mach.reg_index X6) in 
  let arg7 = m.regs.(Mach.reg_index X7) in 
  match lbl with 
  | "printf" -> 
    let str = Memory.read_to_null_terminator (Mach.map_addr m arg0) m.mem |> Mach.string_of_sbytes in 
    clib_printf m str [arg1; arg2; arg3; arg4; arg5; arg6; arg7];
    m
  | "scanf" -> m 
  | _ -> Mach.mach_error m (lbl) "Unimplemented external function"