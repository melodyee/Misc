(*open Linda*)
module Memory = Map.Make(struct type t = Int64.t let compare = Pervasives.compare end)
module Loongson2F = struct
  let iregs = Array.make 32 Int64.zero
  let fregs = Array.make 32 Int64.zero
  let memory = Memory.empty
(*  let interpret     *)
  end