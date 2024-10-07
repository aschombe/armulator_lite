type plugin_event = 
| PreExecution
| PostExecution 
| OnLoad

module type EMULATOR_PLUGIN = sig 
  val name : string
  val options : Cmd_parser.arg list
  val on_load : Mach.t -> Mach.t

  val on_execute : Mach.t -> Mach.t
end

let loaded_plugins : (module EMULATOR_PLUGIN) list ref = ref []

let rec search_plugins (name: string) (plugins: (module EMULATOR_PLUGIN) list) : (module EMULATOR_PLUGIN) option = 
match plugins with 
| [] -> None
| h::t ->
  begin 
    let module M = (val h : EMULATOR_PLUGIN) in 
    if M.name = name then Some h else search_plugins name t
  end

let get_plugin (name: string) : (module EMULATOR_PLUGIN) =
  let plugin = search_plugins name !loaded_plugins in 
  match plugin with 
  | Some p -> p
  | None -> failwith ("plugin '" ^ name ^"' not loaded")

let append_plugin (plugin: (module EMULATOR_PLUGIN)) : unit = loaded_plugins := !loaded_plugins @ [plugin]

let get_loaded_plugins (_: unit) : (module EMULATOR_PLUGIN) list = !loaded_plugins