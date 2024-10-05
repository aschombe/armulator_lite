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