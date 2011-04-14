open Linda
open Printf

open ExtList

module ACM = struct
	open ExtString
	let process fn f = ExtList.iteri f @$ lines @$ ExtUnix.readAll (open_in fn) 
end
open Algebra

module Pos = Point(Int)
module PosMap = ExtMap.Make(struct
	type t = Pos.t
	let compare = compare
	end)
 
(*let zipWith m m' =*)
	
exception Found of Pos.t	
let find f m = 
		try PosMap.iter (fun k v -> if f v then raise (Found k)) m;failwith "zero not found"
		with Found p -> p 
	  		

open MaybeMonad

module type STATE = sig
	type t
	type action
	val equal : t -> t -> bool
	val compare : t -> t -> int
	val hash : t -> int
	val next : t -> (t*action) list
	val final : t -> bool
	
	val cost : t -> float
	(* must not over-estimate *)
end  

let trace s h =
	let rec work s acc =
		try
			let (s',a) = HashMap.get h s in 
			work (s') (a::acc) 
		with Not_found -> acc in
	work s [] 

module Search(S:STATE) = struct
	exception Found of S.action list
	exception Stop of S.t
	module T = Weak.Make(S)
	module M = Map.Make(S)
	let dfs ?(lim=None) s =
		let visited = T.create 3 in
		let rec work i s acc =
			if lim == None || Some i <= lim then begin
				T.add visited s;
				if S.final s then raise (Found (List.rev acc)) else
					iter (fun (s',a) ->
						work (i+1) s' (a::acc)) (filter (not*@T.mem visited*@fst) @$ S.next s) 
				end in
		try work 0 s [];None
		with Found r -> Some r
	let ida s =
		try
			let n = ref 0 in
			while true do
				incr n;
				match dfs ~lim:(Some !n) s with 
					| Some r -> raise (Found r)
					| None -> ()
			done;None
		with Found r -> Some r 
	let bfs s =
		let visited = T.create 3 in
		let h = HashMap.create 3 in
		let q = Queue.create () in
		Queue.push s q;
		try
			while true do
				let s = Queue.pop q in
				T.add visited s;
				if S.final s then raise (Stop s) else
					iter (fun (s',a) -> 
					Queue.add s' q;
					HashMap.set h s' (s,a)) (filter (not*@T.mem visited*@fst) @$ S.next s)
			done;
			None
		with Stop s -> Some (trace s h)
			| Queue.Empty -> None
	let astar s =
		let depth = HashMap.create 3 in		
		let cost' =
			Cache.memo @$ fun t -> 
			round (S.cost t) + HashMap.get depth t in
		let module H = Heap.Imperative(struct
				type t = S.t
				let compare t t' = compare (cost' t') (cost' t)
				(* using max-heap *)
			end) in
		let visited = T.create 3 in
		let queue = H.create 3 in
		let rec work i s =
			HashMap.set depth s 0;
			H.add queue s;
			try
				let s = H.pop_maximum h in
				T.add visited s
			with H.EmptyHeap -> None in
		work 0 s
end

module Board = struct
	type t = {
		p : Pos.t;
		m : int PosMap.t;
		n : int }
	let equal = (=)
	
	let of_string s =
		let lns = ExtString.lines s in
		let m = PosMap.foldli (fun i m e -> PosMap.foldli 
			(fun j m s -> PosMap.add (Pos.make i j) (int_of_string s) m) m @$ ExtString.words e) PosMap.empty @$ 
				lns in
		{ p = find (fun v -> v=0) m; m = m; n = length lns}

	let move b p' =
		if Pos.within p' (b.n-1,b.n-1) && Pos.within (0,0) p' then 
			Some {b with m = PosMap.swap b.p p' b.m; p = p'} else None
			
	let lift f b= move b (f b.p)
	let left,right,up,down = tmap4 lift (Pos.left,Pos.right,Pos.up,Pos.down)
	let hash = Hashtbl.hash
	type action = string
	let next b = concatMap (function (Some b,a) -> [b,a] | _ -> []) [left b,"L";right b,"R";up b,"U";down b,"D"]
	let compare = compare
	let zipWith f b b' =
		PosMap.zipWith f b.m b'.m
	let show b =
		Printf.sprintf "(%s,\n%s" (Pos.show b.p) @$
			show_list ~sep:"\n" (fun l -> show_list (string_of_int*@snd) l) (tile b.n @$ sort compare @$ PosMap.to_list b.m)
end
module S = Search(struct 
	include Board
	let final_state = of_string "1 2 3 4\n5 6 7 8\n9 10 11 12\n 13 14 15 0"
	let final = equal final_state
	let cost t = 
		float @$ PosMap.fold (fun _ b a-> a + (int_of_bool b) ) (zipWith (!=) t final_state) 0
	end) 

let () =
	let s = "1   2  7  3
5   6  0  4
9  10 11  8
13 14 15  12" in
	let b = Board.of_string s in
	printf "%s\n" @$  Board.show @$ b;
	flush stdout;
(*	iter (fun f -> ignore @$ (f b >>= fun b' ->                                                    *)
(*		return @$ printf "%s\n\n" @$ Board.show @$ b')) [Board.left;Board.right;Board.up;Board.down];*)
	match S.ida b with
		| Some l -> printf "%s\n" @$ show_stringlist l
		| None -> printf "failed\n"
	