let step (m: Mach.t) : Mach.t = 
  m

let run (prog: Arm.prog) : unit = 
  let m = Mach.init prog in 
  let rec loop (m: Mach.t) : unit = 
    let m' = step m in 
    loop m' in 
  loop m 
