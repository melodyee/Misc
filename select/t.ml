open List
open Linda
open ExtList
module H = Hashtbl
open Printf

let debug = ref true

type atom = AStr of string | AInt of int
let astr s = AStr s
let aint i = AInt i
let string_of_atom = function
	| AStr s -> s
	| AInt i -> string_of_int i
type selectSpec = SMust | SMay
type spec =
	| Or of selectSpec * atom list
	| Str of string

let mylgcc2 = "loongcc -O3 -Wb,-WOPT:use_ld_st_offset=1 -static -loongson2f -Wb,-CG:float_use_madd -CG:use_loongson2e_multdivmod -lm "
let mylgcc = mylgcc2 ^ " -ipa " 
(*+ " -IPA:keeplight=0 "*)

let mn =
	let l = ref [] in
	for i = 1 to 700 do
		for j = 1 to i do
			addRefList (AStr (Printf.sprintf " -DM=%d -DN=%d" i j)) l
		done;
	done;
	!l	

let specs = [Or (SMust,(map astr [mylgcc;mylgcc2]))
	;Or (SMay,mn)
	]

let specSize = function
	| Or (_,l) -> length l
	| Str _ -> 1
let size specs = product (map specSize specs)
let coverage n specs = max 0. @$ min 1. @$ float n /. float (size specs)
let ensureNotNull l pool = if null l then [nth pool @$ Random.int (length pool)] else l 
let psample p sample spec = match spec with
	| Or (SMay,l) -> Or (SMay,ensureNotNull (sample p @$ sort compare l) l)
	| Or (SMust,_)
	| Str _ -> spec
let rec flatten specs = match specs with
	| [] -> [""]
	| Str s :: xs -> map ((^) s) @$ flatten xs 
	| Or (_,l) :: xs -> concatMap (fun e -> map ((^) e) @$ flatten xs) (map string_of_atom l)


(*let select n specs =                                                                                      *)
(*	let seen = H.create 37 in                                                                               *)
(*	flatten @$ map (fun spec -> psample (coverage n specs) sample spec) specs                               *)
(*(*	flatten @$ map (fun spec -> psample (max 0. @$ min 1. @$ ratio n (specSize spec)) sample spec) specs*)*)

(*let () = print_endline @$ show_list string_of_int @$ randSample 0.01 @$ range 1 100*)
(*let () =                                                              *)
(*(*	print_endline @$ show_list (fun x -> x ^ "\n") @$ flatten specs;*)*)
(*	print_endline @$ show_list (fun x -> x ^ "\n") @$ select 15 specs   *)
(*(*	print_endline @$ string_of_int @$ size spec *)                    *)

(*let fourier l =*)
	

(*let () =                                                   *)
(*	let ma = [|[|0.;-1.;-2.|];[|1.;0.;3.|];[|-3.;5.;3.|]|] in*)
(*	print_endline @$ show_matrix string_of_float @$ ma;      *)
(*	print_endline @$ show_matrix string_of_float @$ inv ma   *)

let fourier_poly a x =
	let s = ref 0. in
	for i =0 to Array.length a -1 do
		let t = ref 1. in
		for j =0 to Array.length a -1 do
			if j<>i then
				t := !t *. sin ((x -. fst a.(j)) /. 2.) /. sin ((fst a.(i) -. fst a.(j)) /. 2.)
		done;
		s := !s +. !t *. snd a.(i)
	done;
	!s
	 
(*let () =                                                                                                 *)
(*	let f x = sin (30.*.x) *. sin (10.*.x*.x -. x +. x*.x*.x)/. (x+.0.1) in                                *)
(*	let n = 30 in                                                                                          *)
(*	let a = Array.init n (fun i -> let x = float i /. (2.*.pi *. float n)  in (x,f x)) in                  *)
(*	ignore @$ Array.init (8*n)                                                                             *)
(*		(fun i -> let x = float i /. (2.*.pi*.float (8*n)) in printf "%f,%f,%f\n" x (f x) (fourier_poly a x))*)
(*(*	Array.iter (fun (x,y) -> printf "%f\n" (f x -. fourier_poly a x) ) a*)                               *)

(*let () =                                                                                                *)
(*	let f x = sin (30.*.x) *. sin (10.*.x*.x -. x +. x*.x*.x)/. (x+.0.1) in                               *)
(*(*	let a = Array.map (fun x -> x,f x) [|0.01;0.2;0.4;0.9;1.|] in*)                                     *)
(*	let n = 10 in                                                                                         *)
(*	let a = Array.init n (fun i -> let x = 0.9 *. float i /. float n in (x,f x)) in                       *)
(*	ignore @$ Array.init (8*n)                                                                            *)
(*		(fun i -> let x = 0.9 *. float i /. (float (8*n)) in printf "%f,%f,%f\n" x (f x) (fourier_poly a x))*)

(*                                                                                                                        *)
(*let () =                                                                                                                *)
(*	 (print_endline *@ show_list string_of_int) @$ map (ofBinary*@grayToBinary *@ binaryToGray*@toBinary) (range 0 100) 	*)
