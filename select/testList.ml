open QuickCheck
open Linda
(*open Lidoutil*)


(* for generating random int lists *)
module AL = Arbitrary_list(Arbitrary_int) ;;
(* for printing out int lists *)
module SL = PShow_list(PShow_int) ;;
(* for being able to test (int list -> bool) *)
module Testable_list_to_bool =
  Testable_fun
    (struct
			module M = Arbitrary_list(Arbitrary_int)
			include M
			end)
    (SL)
    (Testable_bool) ;;
module Testable_int_to_bool =
  Testable_fun
    (Arbitrary_int)
    (PShow_int)
    (Testable_bool) ;;

(*module C = Check(Testable_int_to_bool)                                        *)
let () =
	let module C = Check(Testable_list_to_bool) in	
	let prop_revrev : 'a list -> bool =
	  fun xs -> List.rev (List.rev xs) = xs in
	C.quickCheck prop_revrev;
	let module C = Check(Testable_int_to_bool) in	
	let prop_range : int -> bool =
	  fun n -> let l = ExtList.range 0 n in 
			if n<=0 then l = [] else ExtList.last l = pred n in
	C.quickCheck prop_range;
	                                    
(*let prop_range : int -> bool =                                        *)
(*  fun n -> let l = range n in if n<=0 then l = [] else last l = pred n*)
(*let test = C.quickCheck prop_revrev*)
(*let test = C.quickCheck prop_range  *)
