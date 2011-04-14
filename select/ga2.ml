open Linda
open Printf
open ExtArray

module type CHROME = sig
  type t
  val mutate : mutateRatio:float -> t -> t
  val crossover : mutiplyRatio:float -> t array -> t
  val compare : t -> t -> int
  end

(* k-distance neighbors of each element, may contain duplicates *)
let neighbors k a =
  let n = length a in
  init n (fun i -> map (fun i -> get a @$ (abs i) mod n) (range (i-k+1) (i+1)))
        
module Population(C:CHROME) = struct
  type strategy =
        | Elitism of float (* how many elites to clone *)
        | ParentAndChild
        | ChildOnly
  
  open ExtArray
  module Cluster = struct
    type t = C.t array
    let evolve ?(parents=2) ?(mutiplyRatioF=const 1.0) t =
      mapi (fun i a -> C.crossover (mutiplyRatioF i) a)  (neighbors parents t)
      
    end
  type t = Cluster.t array
  let migrate ~migrateRatio ~migrateIntensity t =
    let immigrants = map (fun c -> take (fraction migrateRatio (length c)) c) t in
    zipWith append (shuffle migrateIntensity immigrants) t
  let evolve t =
    map Cluster.evolve t
     
  end   