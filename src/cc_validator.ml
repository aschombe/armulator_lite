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

let ran : bool ref = ref false
let init_sp : int ref = ref 0

let run_cc_validator (m: Mach.t) : Mach.t =
  begin
    let sp = Int64.to_int m.regs.(Mach.reg_index Arm.SP) in
    if sp = !init_sp then
      Printf.printf "[cc_validator] stack pointer properly managed\n"
    else
      Printf.printf "[cc_validator] stack pointer not properly managed, expected SP: 0x%x, actual SP: 0x%x\n" !init_sp sp;
    m
  end

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
  let on_start = fun m -> 
    ignore (on_start m);
    m

  (* Executes when the machine stops *)
  let on_exit = fun m ->
    on_exit;
    m

  (* Executes before instruction is run *)
  let on_pre_execute = fun m -> m

  (* Executes after PC increases *)
  let on_post_execute = fun m -> m
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)