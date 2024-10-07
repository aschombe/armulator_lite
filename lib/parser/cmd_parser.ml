let strref = ref ""
let boolref = ref false

type spec = 
| Set_bool of bool ref
| Set_string of string ref
| Set_int of int ref
| Usage_msg

type arg = (string * spec * string)

let set_string (opt: string ref) (value: string) : unit = opt := value
let set_int (opt: int ref) (value: int) : unit = opt := value
let set_bool (opt: bool ref) (value: bool) : unit = opt := value 

let is_option (o: string) : bool = String.starts_with ~prefix:"--" o

let string_of_arg (a: arg) : string = 
  match a with
  | (n, Set_bool _, desc) -> n ^ " [bool],\t"^ desc
  | (n, Set_string _, desc) -> n ^ " <string>,\t"^ desc
  | (n, Set_int _, desc) -> n ^ " <int>,\t"^ desc
  | (n, Usage_msg, desc) -> n ^ ",\t"^ desc

let string_of_arglist (opts: arg list) : string = 
  List.map (fun arg -> "\t" ^ (string_of_arg arg)) opts |> String.concat "\n"

let set_argument (f: spec) (v: string) : unit = 
  match f with 
  | Set_bool b -> b := bool_of_string v
  | Set_string s -> s := v
  | Set_int i -> i := int_of_string v
  | _ -> ()

let rec match_to_spec (option: string) (value: string) (opts: arg list) (c_args: arg list) : unit = 
  match opts with 
  | [] -> ()
  | (_, Usage_msg, _)::_ -> Printf.printf "./arml [options]\n%s\n" (string_of_arglist c_args); exit 0
  | (name, f, _)::t -> if name = option then set_argument f value else match_to_spec option value t c_args

let rec parse_arguments (args: string list) (opts: arg list) : unit =
  match args with 
  | [] -> ()
  | o::t when not (is_option o) -> parse_arguments t opts
  | o::v::t when (is_option o) && not (is_option v) -> match_to_spec o v opts opts; parse_arguments (v::t) opts
  | o::v::t when (is_option o) && (is_option v) -> match_to_spec o "true" opts opts; parse_arguments (v::t) opts
  | _ -> ()