open Linda

let s = "1   2  7  3
5   6  0  4
9  10 11  8
13 14 15  12"

open Algebra


module type Cell = sig
	type t
	val show : t -> string 
end
module Grid(C:Cell) = struct
	type t = C.t array array
	module P = Point(Int)
	let init m n f =
		ExtArray.init_matrix m n f
	let legal g (x,y) =
		let m,n = dims g in
		0<=x && x<n && 0<=y && y<m 
	let neighbours4 g p = List.filter (legal g) @$ P.neighbours4 p
	let neighbours8 g p = List.filter (legal g) @$ P.neighbours8 p
	let onBorder g p = List.exists (not*@legal g) @$ neighbours4 g p
	let show g = show_matrix C.show g 
end

(*module Board = struct                                                                                                                             *)
(*	type 'a t = {                                                                                                                                   *)
(*			d : 'a array array;                                                                                                                         *)
(*			x : int ;                                                                                                                                   *)
(*			y : int ;                                                                                                                                   *)
(*		}                                                                                                                                             *)
(*	let inside b (x,y) =                                                                                                                            *)
(*		let m,n = dims b.d in                                                                                                                         *)
(*		0<=x && x<n && 0<=y && y<m                                                                                                                    *)
(*	let checkPos b (x,y) =                                                                                                                          *)
(*		if inside b (x,y) then Some {b with x = x; y =y} else None                                                                                    *)
(*	let up b (x,y) = checkPos b (x,y+1)                                                                                                             *)
(*	let down b (x,y) = checkPos b (x,y-1)                                                                                                           *)
(*	let left b (x,y) = checkPos b (x-1,y)                                                                                                           *)
(*	let right b (x,y) = checkPos b (x+1,y)                                                                                                          *)
(*(*	let up,down,left,right =                                                                                                                    *)*)
(*(*		tmap4 (fun f -> fun b p -> checkPos b (f p)) ((fun (x,y) -> (x,y+1)),(fun (x,y) -> (x,y-1)),(fun (x,y) -> (x-1,y)),(fun (x,y) -> (x+1,y)))*)*)
(*end                                                                                                                                               *)

module SlideBoard = struct
	module Pos = Point(Int)
	type 'a t = {
			d : 'a Parray.t Parray.t;
			cur : Pos.t
		}
	let check cur f b = 
		let dims = (Parray.length b.d,Parray.length (Parray.get b.d 0)) in
		if Pos.within cur dims && Pos.within (0,0) cur then Some (f b) else None
	let get b p = Parray.get (Parray.get b.d (snd p)) (fst p)
	let set b p v = {b with d = Parray.set (Parray.get b.d (snd p)) (fst p) v}
	let swap b p p' =
		let t = get b p in
		set (set b p (get b p')) p' t
	let move mv =
		let cur' = mv cur in
		let b' = swap b cur cur' in
		check {b' with cur = Pos.left cur }
end

(*let () =*)
	