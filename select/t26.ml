open Printf
open Linda
module type EFFECT = sig
  type t
  type effect
  val reads : t -> effect list
  val writes : t  -> effect list
  val mayAlias : effect -> effect -> bool
end
open Algebra


(*module type TEST = sig                                      *)
(*  type t                                                    *)
(*  val test : t -> Score.t                                   *)
(*end                                                         *)
(*(*let seen : Permutation.t HashSet.t*)                      *)
(*module Reorder(E:EFFECT)(T:TEST with type t = E.t) = struct *)
(*  let (/|) l l' = ExtList.intersectBy E.mayAlias l l' <> [] *)
(*  let canSwap a a' =                                        *)
(*    let ra,ra' = tmap E.reads (a,a') in                     *)
(*    let wa,wa' = tmap E.writes (a,a') in                    *)
(*    not (ra /| wa' || wa /| ra' || wa /| wa')               *)
(*  let canMoveOver a l =                                     *)
(*    ExtList.all (canSwap a) l                               *)
(*  let rec allIndependent = function                         *)
(*    | [] -> true                                            *)
(*    | x::xs -> canMoveOver x xs && allIndependent xs        *)
(*                                                            *)
(*  let morph = function                                      *)
(*    | [] -> []                                              *)
(*    | x::xs -> if canMoveOver x xs then [xs @ [x]]          *)
(*  module H = Heap.Imperative(struct                         *)
(*    type t = Score.t * E.t                                  *)
(*    let compare t t' = Score.compare (fst t) (fst t')       *)
(*    end)                                                    *)
(*                                                            *)
(*  let evolve h =                                            *)
(*    let m = H.maximum h in                                  *)
(*    morph m                                                 *)
(*end                                                         *)

(*module H = Heap.Imperative(struct                  *)
(*    type t = int*int                               *)
(*    let compare t t' = compare (fst t) (fst t')    *)
(*  end)                                             *)
(*                                                   *)
(*let () =                                           *)
(*  let h = H.create 3 in                            *)
(*  H.add h (2,3);                                   *)
(*  H.add h (22,13);                                 *)
(*  printf "%s\n" @$ show_intpair @$ H.pop_maximum h;*)
(*  printf "%s\n" @$ show_intpair @$ H.maximum h     *)