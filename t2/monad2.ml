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

module type MONAD2 = sig
  type ('s,'a) t
  val bind : ('s,'a) t -> ('a -> ('s,'b) t ) -> ('s,'b) t
  val return : 'a -> ('s,'a) t
end

module Monad2 (M:MONAD2) = struct
  open M
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id 
  let fmap f xs = xs >>= (return *@ f)
  let fail = failwith
    let liftM f m = m >>= fun x -> return @$ f x
    let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
    let liftM3 f m m' m'' = 
      m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
    let ap fm m =
        fm >>= fun f ->
            m >>= fun x ->
                return @$ f x            
    let sequence ms =
        let k m m' =
            m >>= fun x ->
                m' >>= fun xs ->
                    return (x::xs) in  
        List.fold_right k ms (return [])    
end
module type MONADPLUS2 = sig
  include MONAD2
  val mzero : 'a
  val mplus : 'a -> 'a -> 'a
end
module MonadPlus2 (M:MONADPLUS2) = struct
  open M
  module Monad = Monad2(M)
  open Monad
  let guard flg = if flg then return () else mzero
  let msum l = List.fold_right mplus l mzero
end

module StateMonad = Monad2 (struct
  type ('s,'a) t = 's -> 'a*'s
	let bind (m:('s,'a) t) (k: 'a -> ('s,'b) t) : ('s,'b) t = 
	  fun s ->
	    let (a',s') = m s in
	    let (a'',s'') = k a' s' in
	    (a'',s'')
  let return (x:'a) : ('s,'a) t = fun s -> (x,s)
  end)
  
(*open Matrix.ListMonad                  *)
(*open Matrix.ListMonad.Monad            *)
(*(*open Matrix.ListMonad.MonadO*)       *)
(*let () = ignore ([1;2;3;4] >>= fun x ->*)
(*  print_int x;                         *)
(*  print_newline ();                    *)
(*  return x)                            *)