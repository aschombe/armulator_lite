open Asm
open Arch_core
open Parsing

let test_arg = ref ""

let post_exec (m: Mach.t) : Mach.t = 
  if not (Int64.equal m.pc m.info.exit_val || Int64.equal m.regs.(Mach.reg_index Arm.SP) m.info.exit_val) then (
    Printf.printf "[test_plugin] peanits %s execution: \t%s\n%!" !test_arg (Arm_stringifier.string_of_insn (Mach.get_insn m m.pc))
  ); m

module M: Plugins.EMULATOR_PLUGIN = struct 
  let name = "test_plugin"
  let options = [
    ("--test-arg", Cmd_parser.Set_string test_arg, "A test argument")
  ]
  let on_load = fun m -> Printf.printf "[test_plugin] now loaded with option '%s'\n" !test_arg; m
  let on_unload = fun m -> m
  let on_start = fun m -> m
  let on_exit = fun m -> Printf.printf "[test_plugin] done with option\n"; m
  let on_pre_execute = fun m -> m
  let on_post_execute = post_exec
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)
