let print_lines (lines : string list) : unit =
  List.iter (fun x -> print_endline x) lines

let is_empty (line : string) : bool = 
  let line = String.trim line in
  String.equal String.empty line

let is_directive (line : string) : bool = String.starts_with line ~prefix:"."

let find_directives (lines : string list) (d_name : string) : string list list = 
  let rec fdh (l: string list) (d: string) (active: bool) (acc : string list list) (cur : string list) : string list list =
    match l with
    | [] -> acc @ [cur]
    | h::t -> 
      if is_directive h then
        if String.starts_with h ~prefix:("." ^ d) then
          fdh t d true acc (cur @ [h])
        else if active then
          fdh t d false (acc @ [cur]) []
        else
          fdh t d false acc cur
      else if active then
        fdh t d true acc (cur @ [h])
      else
        fdh t d false acc cur
  in 
  fdh lines d_name false [] []

let parse_assembly (lines : string list) : Arm.prog =
  let directives = find_directives lines "text" in
  print_lines (List.concat directives);
  []


