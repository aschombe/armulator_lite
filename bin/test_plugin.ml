let test_arg = ref ""

module M: Plugins.EMULATOR_PLUGIN = struct 
  let name = "test_plugin"
  let options = [
    ("--test-arg", Cmd_parser.Set_string test_arg, "A test argument")
  ]
  let on_load = fun _ -> Printf.printf "test_plugin now loaded with option '%s'\n" !test_arg
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)
