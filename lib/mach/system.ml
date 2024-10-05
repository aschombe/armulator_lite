exception Invalid_syscall of int64

type syscall = SysWrite | SysRead | SysExit
let number_to_syscall = function 
| 63L -> SysRead
| 64L -> SysWrite
| 93L -> SysExit
| n -> raise (Invalid_syscall n)

let syscall_read (m: Mach.t) (fd: int64) (buf: int64) (count: int64) : int64 =
  0L 

let syscall_write (m: Mach.t) (fd: int64) (buf: int64) (count: int64) : int64 =
  0L 

let syscall_exit (m: Mach.t) (status: int64) : int64 =
  m.pc <- Int64.sub 0xfdeadL 8L;
  status 

let execute_syscall (m: Mach.t) (n: int64) : Mach.t =
  let syscall_exec = number_to_syscall n in 
  let arg0 = m.regs.(Mach.reg_index X0) in 
  let arg1 = m.regs.(Mach.reg_index X1) in 
  let arg2 = m.regs.(Mach.reg_index X2) in 
  match syscall_exec with 
  | SysRead -> let result = syscall_read m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m
  | SysWrite -> let result = syscall_write m arg0 arg1 arg2 in m.regs.(Mach.reg_index X0) <- result; m
  | SysExit -> let result = syscall_exit m arg0 in m.regs.(Mach.reg_index X0) <- result; m