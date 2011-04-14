open Array
open Linda
(*open ExtList*)
open ExtArray
open Printf
let align n p = n/p*p

type address = int
type cacheLineState =
	| Cached of address
	| Dirty of address
	| Invalid
let show_cacheLineState = function
	| Cached addr -> sprintf "( ,0x%x)" addr
	| Dirty addr -> sprintf "(W,0x%x)" addr
	| Invalid -> "( ,0x00000000)" 
(*let vacant cs = cs = Invalid*)
type instr =
	| Load of address
	| Store of address
type program = instr list

type memOp =
	| Hit
	| Read of address
	| Write of address

let show_memOp = function
	| Hit -> "Hit"
	| Read addr -> sprintf "Read %d" addr
	| Write addr -> sprintf "Write %d" addr 
type trace = memOp list

class cache assoc totalsize linesize =
	let size = (totalsize/assoc/linesize) in
	let line i = (i/linesize) mod size in
	let datas = Array.init assoc (fun i -> Array.make size Invalid) in
	let search p i =
		let ln = line i in
		let rec work j = if j>=assoc then None else 
			if p i datas.(j).(ln) then Some j else work (j+1) in
		work 0 in
	let hasHit i = search (fun i cs -> match cs with
		| Dirty addr
		| Cached addr when i =addr -> true 
		| _ -> false) i in
	let hasVacant i = search (fun i cs -> match cs with
		| Invalid -> true
		| _ -> false) i in
	let select () = Random.int assoc in			
	object
	method show =
		String.concat "\n-\n" @$ to_list @$ map (fun a -> show_array show_cacheLineState a) datas
	method load i' =
		let i = align linesize i' in
		let ln = line i in
		match hasHit i with
		| Some j -> [Hit]
		| None -> begin
			match hasVacant i with
			| Some j -> 
				datas.(j).(ln) <- Cached i;
				[Read i]
			| None -> 
				let j = select () in
				let r = match datas.(j).(ln) with
					| Dirty i' -> [Write i']
					| _ -> [] in
				datas.(j).(ln) <- Cached i;
				[Read i] @ r
			end
	method store i' =
		let i = align linesize i' in
		let ln = line i in
		match hasHit i with
		| Some j ->
			datas.(j).(ln) <- Dirty i;
			[]
		| None -> begin
			match hasVacant i with
			| Some j ->
				datas.(j).(ln) <- Dirty i;
				[]
			| None -> 
			let j = select () in
			let r = match datas.(j).(ln) with
				| Dirty i' -> [Write i']
				| _ -> [] in
			datas.(j).(ln) <- Dirty i;
			r
			end
end

(*let () =                             *)
(*	let cache = new cache 4 65536 32 in*)
(*	print_endline @$ cache#show        *)

(*let playMemop cache = function            *)
(*	| Read i -> begin match get cache i with*)
(*		| Vacant ->                           *)
(*			cache.(i) <- Occupied i             *)
(*let play cache trace =                    *)
(*	foreach trace @$ fun memop ->           *)
(*		                                      *)
(*let () =                                  *)
(*	let cache = Array.make 64000 Vacant in  *)
(*	                                        *)

