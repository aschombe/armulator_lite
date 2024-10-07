module M: Plugins.EMULATOR_PLUGIN = struct 
  let name = "my_plugin"
  let options = []
  let on_load = fun _ -> Printf.printf "this is another plugin\n"
end

let () = Plugins.append_plugin (module M : Plugins.EMULATOR_PLUGIN)
