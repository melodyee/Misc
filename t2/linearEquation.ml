open Num
module type TERM = sig
  type t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val show : t -> string
end

(*module Equation (T:TERM) = struct*)
(*  type term = T.t                *)
(*  type t = (num*term) list * num *)
(*  type system = t list           *)
(*  let solve sys =                *)
(*end                              *)
  