open List
open Printf
open Linda
open ExtList
(*open ListMonad*)
(*open Uref                                                *)
(*                                                         *)
(*let a = Uref.uref "a"                                    *)
(*let b = Uref.uref "b"                                    *)
(*let () =                                                 *)
(*  Uref.union (b,a);                                      *)
(*(*  print_endline (Uref.deref ((Uref.uref "a")));*)      *)
(*  print_endline (Uref.deref (Uref.find a));              *)
(*  print_endline (Uref.deref (Uref.find b));              *)
(*  print_endline (Uref.deref (Uref.find (Uref.uref "a")));*)
(*  print_endline (Uref.deref (Uref.find (Uref.uref "b"))) *)
  
(*let () = RankEquation.test ()*)

(*open StateMonad                               *)
(*let inc =                                     *)
(*  get >>= fun s ->                            *)
(*    put @$ s + 1                              *)
(*let () =                                      *)
(*  print_int (execState (replicateM 30 inc) 0) *)

(*let () = print_int (T2.S.f 1)*)
(*open T2                 *)
(*open S                  *)
(*                        *)
(*let () = print_int (f 2)*)

(*module type SHOW = sig                        *)
(*  type t                                      *)
(*  val show : t -> string                      *)
(*end                                           *)
(*                                              *)
(*module SHOW_LIST (S:SHOW) = struct            *)
(*let show l =                                  *)
(*  String.concat " " (List.map S.show l)       *)
(*end                                           *)
(*module SHOW_INT_LIST = SHOW_LIST (struct      *)
(*  type t = int                                *)
(*  let show = string_of_int                    *)
(*  end)                                        *)
(*let show_int_list = SHOW_INT_LIST.show        *)
(*(*let f = SHOW_LIST (struct *)                *)
(*(*  type t = int            *)                *)
(*(*  let show = string_of_int*)                *)
(*(*  end).show               *)                *)
(*let () = print_endline@$ show_int_list [1;2;3]*)

module type NUM = sig
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (mod) : t -> t -> t
  val compare : t -> t -> int
  val zero : t
  val one : t
  val show : t -> string
  val read : string -> t
  val of_int : int -> t
end  
  
module Numeric (N:NUM) = struct
  open N
  include N
(*  let (+) = (+)        *)
(*  let (-) = (-)        *)
(*  let ( * ) = ( * )    *)
(*  let (/) = (/)        *)
(*  let (mod) = (mod)    *)
(*  let compare = compare*)
(*  let zero = zero      *)
(*  let one = one        *)
(*  let show = show      *)
    
  let (>=) a a' = compare a a' >=0
  let (<=) a a' = compare a a' <=0
  let (>) a a' = compare a a' >0
  let (<) a a' = compare a a' <0
  let ge = (>=)
  let le = (<=)
  let gt = (>)
  let lt = (<)
  let max a a' = if a > a' then a else a'
  let min a a' = if a > a' then a' else a  
  let gcd m n =
	  let rec gcd' a b =
	    if b=zero then a else gcd' b (a mod b)
	  in if m>n then gcd' m n else gcd' n m
  let lcm m n = (m/gcd m n)*n
  let sum l = foldl (+) zero l
  let dotProduct l l' = sum @$ zipWith ( * ) l l' 
end
module Int = Numeric (struct
  type t = int
  open Pervasives
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let (/) = (/)      
  let (mod) = (mod)    
  let compare = compare
  let zero =0
  let one = 1
  let show = string_of_int
  let read = int_of_string
  let of_int = id
  end)
module Long = Numeric (struct
  open Int64
  type t = int64
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div    
  let (mod) = rem    
  let compare = compare
  let zero = zero
  let one = one
  let show = to_string
  let read = of_string
  let of_int = of_int
  end)
module Rational = Numeric (struct
  open Num
  type t = num
  let (+) = add_num
  let (-) = sub_num
  let ( * ) = mult_num
  let (/) = div_num
  let (mod) = mod_num
  let compare = compare
  let zero = num_of_int 0
  let one = num_of_int 1
  let show = string_of_num
  let read = num_of_string
  let of_int = num_of_int
  end)
let () = print_endline (Int.show @$ Int.gcd 35 20)
let () = print_endline (Long.show @$ Long.gcd 35L 20L)
let () = print_endline (Rational.show @$ Rational.gcd (Rational.of_int 25) (Rational.of_int 30) )   

(** 2D dense matrix*)
module Matrix (N:NUM) = struct
  open Array
  open N
  open ExtArray
  type t = N.t array array
  type vec = N.t array
  exception WrongDimension
  let checkDim b = if not b then raise WrongDimension
  let dims ma = (length ma,length ma.(0))
  let create (m,n) = Array.make_matrix m n zero
  let row ma i = ma.(i)
  let col ma j = init (snd (dims ma)) (fun i -> ma.(i).(j))
  let dotproduct a a' = fold_left (+) zero @$ zipWith ( * ) a a'
  let mult ma ma' =
    let (m,n) = dims ma in
    let (m',n') = dims ma' in
    checkDim (n=m');
    let ma'' = create (m,n') in
    for i =0 to m do
      for j =0 to n' do
        ma''.(i).(j) <- dotproduct (row ma i) (col ma' j)
      done
    done
end

(*module POLY (N:NUM) = struct                       *)
(*  open N                                           *)
(*  let add l l2 = zipWith (+) l l2                  *)
(*end                                                *)
(*module FPOLY = POLY (struct                        *)
(*  type t = float                                   *)
(*  let (+) = (+.)                                   *)
(*  end)                                             *)
(*let () = print_float @$ hd @$ FPOLY.add [1.] [2.]  *)
(*module IPOLY = POLY (struct                        *)
(*  type t = int                                     *)
(*  let (+) = (+)                                    *)
(*  end)                                             *)
(*let () = print_int @$ hd @$ IPOLY.add [1] [2]      *)