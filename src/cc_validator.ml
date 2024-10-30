open Arch_core
open Asm
(*
This is a calling convention validator.
It checks that calling convention of the entire program is followed.

The calling convention is as follows:

-------------------------------------------------
                      |    Caller      |          Callee         |
Before Branching      | Save X0-X18    |          xxxxxx         |
After Entering Callee | xxxxxxxxxxxx   | Save X19-X28, X30/LR    |
Before Exiting Callee | xxxxxxxxxxxx   | Restore X19-X28, X30/LR |
After RET to Caller   | Restore X0-X18 |          xxxxxx         |
-------------------------------------------------

The calling convention is as follows:
- Caller saves X0-X18 before branching to callee
- Callee saves X19-X28, X30/LR before executing
- Callee restores X19-X28, X30/LR before exiting/after executing
- Caller restores X0-X18 after returning from callee

The validator checks that the calling convention is followed, including that the stack is properly managed (i.e. stack pointer is properly adjusted).
*)

(* idea for caller saved regs: 
once I encounter a sub sp, sp, x , I can enable a flag that allows the pushing of str encounters onto a stack, and then enable another flag once I ret, and as I go pop off the stack
*)

let ran : bool ref = ref false
let init_sp : int ref = ref 0

(* let caller_reg_snapshot : int array ref = ref [||] *)
let push_caller_regs : bool ref = ref false
let pop_caller_regs : bool ref = ref false
let caller_regs_stack : int list ref = ref []

let callee_reg_snapshot : int array ref = ref [||]

let run_cc_validator (m: Mach.t) : Mach.t =
  let sp = Int64.to_int m.regs.(Mach.reg_index Arm.SP) in
  if sp = !init_sp then
    Printf.printf "[cc_validator] stack pointer properly managed\n"
  else
    Printf.printf "[cc_validator] stack pointer not properly managed, expected SP: 0x%x, actual SP: 0x%x\n" !init_sp sp;
  
  (* check the stack *)
  if List.length !caller_regs_stack = 0 then
    Printf.printf "[cc_validator] stack properly managed\n"
  else
    Printf.printf "[cc_validator] stack not properly managed, expected stack to be empty, actual: %s\n" (String.concat ", " (List.map string_of_int !caller_regs_stack));
  m

(* let take_caller_reg_snapshot (m: Mach.t) : Mach.t =
  (* registers x0-x18 (inclusive)*)
  caller_reg_snapshot := Array.make 19 0;
  (* snapshot 0-18 *)
  for i = 0 to 18 do
    (!caller_reg_snapshot).(i) <- Int64.to_int m.regs.(i)
  done;
  m *)

let take_callee_reg_snapshot (m: Mach.t) : Mach.t =
  (* registers x19-x28 (inclusive)*)
  callee_reg_snapshot := Array.make 10 0;
  (* snapshot 19-28 *)
  for i = 19 to 28 do
    (!callee_reg_snapshot).(i - 19) <- Int64.to_int m.regs.(i)
  done;
  m

let on_pre_execute (m: Mach.t) : Mach.t =
  let good = ref true in
  let pc = m.pc in
  let instr = Mach.get_insn m pc in
  (match instr with
  | (Arm.Bl, _) ->
    let _ = take_callee_reg_snapshot m in ()
  | (Arm.Ret, _) ->
    (* check x19-x28 inclusive, if a register is not the same, print it *)
    for i = 19 to 28 do
      if Int64.to_int m.regs.(i) <> (!callee_reg_snapshot).(i - 19) then begin
        Printf.printf "[cc_validator] register x%d not properly restored, expected: 0x%x, actual: 0x%x\n" i (!callee_reg_snapshot).(i - 19) (Int64.to_int m.regs.(i));
        good := false
      end
    done;
    if !good then
      Printf.printf "[cc_validator] callee saved registers properly restored\n"
  | (Arm.Sub, [Arm.Reg(Arm.SP); Arm.Reg(Arm.SP); _]) -> push_caller_regs := true
  | (Arm.Add, [Arm.Reg(Arm.SP); Arm.Reg(Arm.SP); _]) -> pop_caller_regs := true
  | (Arm.Str, [Arm.Reg(src); Arm.Reg(Arm.SP); _]) when !push_caller_regs ->
    caller_regs_stack := (Int64.to_int m.regs.(Mach.reg_index src)) :: !caller_regs_stack;
    Printf.printf "[cc_validator] pushing x%d onto stack\n" (Int64.to_int m.regs.(Mach.reg_index src))
  | (Arm.Ldr, [Arm.Reg(dst); Arm.Reg(Arm.SP); _]) when !pop_caller_regs ->
    let expected = List.hd !caller_regs_stack in
    caller_regs_stack := List.tl !caller_regs_stack;
    if expected <> Int64.to_int m.regs.(Mach.reg_index dst) then
      Printf.printf "[cc_validator] expected x%d to be 0x%x, actual: 0x%x\n" (Int64.to_int m.regs.(Mach.reg_index dst)) expected (Int64.to_int m.regs.(Mach.reg_index dst))
    else
      Printf.printf "[cc_validator] popping x%d off stack\n" (Int64.to_int m.regs.(Mach.reg_index dst))
  | _ -> ());
  m
    
let on_start (m: Mach.t) : Mach.t =
  init_sp := Int64.to_int m.regs.(Mach.reg_index Arm.SP);
  m

let on_exit =
  match !ran with
  | true -> Printf.printf "[cc_validator] ran cc validator\n"
  | false -> ran := true; 
  
module M: Plugins.EMULATOR_PLUGIN = struct
  let name = "cc_validator"
  let options = [ ]

  let on_load = fun m -> m

  let on_unload = fun m -> 
    if !ran then run_cc_validator m else m
  
  (* Executes when the machine starts *)
  let on_start = fun m -> on_start m

  (* Executes when the machine stops *)
  let on_exit = fun m ->
    on_exit;
    m

  (* Executes before instruction is run *)
  let on_pre_execute = fun m -> on_pre_execute m
    
  (* Executes after PC increases *)
  let on_post_execute = fun m -> m
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)