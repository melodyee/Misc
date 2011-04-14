(* Discrete Event Simulation *)
module OneQueue = struct
type cycle = int
type event = int (* spit after n cycles *)
type queue = (cycle * event) list
type state = {
  cycle : cycle;
  }
let run q =
  let rec work q = match q with
    | [] -> ()
    | (c,e)::xs ->
      
      work xs in
  work q
end

let () =
  OneQueue.run [1,2;2,3;3,0]