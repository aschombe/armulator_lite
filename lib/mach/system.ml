exception Invalid_syscall of int64

type syscall = SysWrite | SysRead | SysExit
let number_to_syscall = function 
| 63L -> SysRead
| 64L -> SysWrite
| 93L -> SysExit
| n -> raise (Invalid_syscall n)

let syscall_read (_m: Mach.t) (_fd: int64) (_buf: int64) (_count: int64) : int64 =
  0L 

let syscall_write (m: Mach.t) (_fd: int64) (buf: int64) (count: int64) : int64 =
  let bytes = Memory.read_bytes buf count m.mem in 
  List.iter (fun (byte: Mach.sbyte) -> 
    match byte with
    | Byte(c) -> Printf.printf "%c" c
    | _ -> Printf.printf "."
  ) bytes; List.length bytes |> Int64.of_int 

let syscall_exit (m: Mach.t) (status: int64) : int64 =
  m.pc <- m.info.exit_val;
  status 

let execute_syscall (m: Mach.t) : Mach.t =
  let syscall_exec = number_to_syscall m.regs.(Mach.reg_index X8) in 
  let arg0 = m.regs.(Mach.reg_index X0) in 
  let arg1 = m.regs.(Mach.reg_index X1) in 
  let arg2 = m.regs.(Mach.reg_index X2) in 
  match syscall_exec with 
  | SysRead -> let result = syscall_read m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m
  | SysWrite -> let result = syscall_write m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m
  | SysExit -> let result = syscall_exit m arg0 in m.regs.(Mach.reg_index X0) <- result; m

let rec __clib_printf (fmt: char list) (args: int64 list) : unit =
  match fmt, args with
  | [], [] -> () 
  | _, [] -> Printf.printf "(%s)" (String.of_seq (List.to_seq fmt)) 
  | ('%'::'d'::f), (arg::rest_args) ->
    Printf.printf ("%d") (arg |> Int64.to_int);
    __clib_printf f rest_args
  | (c::f), _ -> 
    Printf.printf "%c" c;
    __clib_printf f args
  | _ -> ()
  
let clib_printf fmt_str args =
  __clib_printf (String.to_seq fmt_str |> List.of_seq) args

let execute_extern_function (m: Mach.t) (lbl: string) : Mach.t =
  let arg0 = m.regs.(Mach.reg_index X0) in 
  let arg1 = m.regs.(Mach.reg_index X1) in 
  let arg2 = m.regs.(Mach.reg_index X2) in 
  let arg3 = m.regs.(Mach.reg_index X3) in 
  let arg4 = m.regs.(Mach.reg_index X4) in 
  let arg5 = m.regs.(Mach.reg_index X5) in 
  match lbl with 
  | "printf" -> 
    let str = Memory.read_to_null_terminator arg0 m.mem |> Mach.string_of_sbytes in 
    clib_printf str [arg1; arg2; arg3; arg4; arg5];
    m
  | "scanf" -> m 
  | _ -> Mach.mach_error m (lbl) "Unimplemented external function"