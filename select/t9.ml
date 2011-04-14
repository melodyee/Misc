(*module Dag = struct    *)
(*	type t = {           *)
(*			nodes : Hashtbl.t*)
(*			                 *)
(*		}                  *)
(*end                    *)

module Rtree = struct
	type 'a t = Leaves of ('a * 'a t) list
	let lift f = function
		| Leaves l -> Leaves (f l) 
	let lift2 f = function
		| Leaves l -> f l
	let rec map f t = lift (List.map (fun (e,t') -> f e,map f t')) t
	let rec iter f t = lift2 (List.iter (fun (e,t') -> f e;iter f t')) t
	let empty = Leaves [] 
(*	let of_list = function*)
(*		| [] ->  empty      *)
(*		| (a,a')::xs ->     *)
(*	let rec fold f i t = lift (List.map (fun (e,t') -> f i e,map f t')) t*)
end
