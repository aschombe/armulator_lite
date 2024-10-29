type plugin_event = 
| OnLoadEvent
| OnUnloadEvent
| PreExecutionEvent
| PostExecutionEvent 
| OnExitEvent
| OnStartEvent

module type EMULATOR_PLUGIN = sig 
  val name : string
  val options : Cmd_parser.arg list
  val on_load : Mach.t -> Mach.t
  val on_unload : Mach.t -> Mach.t

  val on_start : Mach.t -> Mach.t
  val on_pre_execute : Mach.t -> Mach.t
  val on_post_execute : Mach.t -> Mach.t
  val on_exit : Mach.t -> Mach.t
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

let execute_plugin_event (event: plugin_event) (m: Mach.t) : Mach.t = 
  List.fold_left (
    fun m pl -> 
      let module M = (val pl : EMULATOR_PLUGIN) in 
      match event with 
      | PreExecutionEvent -> M.on_pre_execute m
      | PostExecutionEvent -> M.on_post_execute m
      | OnLoadEvent -> M.on_load m
      | OnExitEvent -> M.on_exit m
      | OnUnloadEvent -> M.on_unload m
      | OnStartEvent -> M.on_start m
    ) m !loaded_plugins
