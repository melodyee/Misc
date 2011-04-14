open List
open Array
open Linda
open ExtList
open ExtArray
open Printf

let debug = ref true

module type CHROME = sig
(*	type gene*)
	type t
  val crossover : t array -> t  
(*	val crossover2 : t -> t -> t*)
	val mutate : float -> t -> t
	val random : t -> t (*generate following a template*)
(*	val countGenes : t -> int*)
	val show : t -> string
end

module BinaryChrome = struct
	type gene = bool
	type t = BitSet.t
	open Array
	let of_array a = BitSet.of_bool_array a
	let to_array bs = BitSet.to_bool_array bs
(*	let crossover2 = BitSet.crossover2*)
  let crossover a = 
(*    fprintf stderr "crossover %s\n" @$ show_array BitSet.show a;*)
(*    flush stderr;                                               *)
    BitSet.crossover a
	let countGenes = BitSet.bits
	let mutate p chm =
		let n = countGenes chm in
		let chm' = BitSet.copy chm in
		List.iter (fun i -> BitSet.toggle chm' i) (sampleInt (0,n) (max 1 @$ fraction p n));
(*    fprintf stderr "mutate %s\n" @$ BitSet.show chm;*)
(*    flush stderr;                                   *)
		chm'
	let random chm =
(*    fprintf stderr "random %s\n" @$ BitSet.show chm;*)
(*    flush stderr;                                   *)
    BitSet.random (BitSet.bits chm) 
	let show chm = "{" ^ Algebra.IntArray.show ~sep:";" (BitSet.to_array chm) 
		^ "@" ^ string_of_int (BitSet.bits chm)  
end
(*open BinaryChrome                                                             *)
(*let a = of_array [|true;false;true;true|]                                     *)
(*let a' = of_array [|false;true;true;true|]                                    *)
(*let () =                                                                      *)
(*  Random.self_init ();                                                        *)
(*	print_endline @$ show @$ crossover2 a a';                                   *)
(*  print_endline @$ show_array string_of_bool @$ to_array @$ crossover [|a;a'|]*)

(*type flagSpec =                                                                                      *)
(*	| FRange of int * int                                                                              *)
(*	| FIsland of int array                                                                             *)
(*	| FStr of string array                                                                             *)
(*type flagGene = int * flagSpec                                                                       *)
(*let show_flagGene = function                                                                         *)
(*	| i,FRange (a, b) -> sprintf "<%d|%d,%d>" i a b                                                    *)
(*	| i,FIsland a -> sprintf "<%d|%s>" i (show_intarray a)                                             *)
(*	| i,FStr a -> sprintf "<%d|%s>" i (show_stringarray a)                                             *)
(*                                                                                                     *)
(*let randomGene = function                                                                            *)
(*	| _,FRange(a,b) -> a+Random.int (b-a),FRange(a,b)                                                  *)
(*	| _,FIsland a -> Random.int (length a),FIsland a                                                   *)
(*	| _,FStr a -> Random.int (length a),FStr a                                                         *)
(*                                                                                                     *)
(*module FlagChrome :Chrome with type gene = flagGene = struct                                         *)
(*	type gene = flagGene                                                                               *)
(*	type t = digitSet                                                                                  *)
(*end	                                                                                                *)
(*                                                                                                     *)
(*(*	type gene =                                               *)                                     *)
(*(*		| GInt of int * (int*int) (* v,[a,b] *)                 *)                                     *)
(*(*		| GStr of int * string array                            *)                                     *)
(*(*	let show_gene = function                                  *)                                     *)
(*(*		| GInt (v,(a,b)) -> "<v|a,b>"                           *)                                     *)
(*(*		| GStr (i,a) -> sprintf "<%d|%s>" i (show_stringarray a)*)                                     *)
(*(*	let show = show_array show_gene                           *)                                     *)
(*(*	let mutateGene = function                          *)                                            *)
(*(*		| GInt(_,(a,b)) -> GInt(Random.int (b-a+1),(a,b))*)                                            *)
(*(*		| GStr(_,a) -> GStr(Random.int (length a),a)     *)                                            *)
(*module FlagChrome :Chrome= struct                                                                    *)
(*	open Array                                                                                         *)
(*	type chrome = int array                                                                            *)
(*	type t = chrome                                                                                    *)
(*	let of_array a = a                                                                                 *)
(*	let to_array a = a                                                                                 *)
(*	let to_bitset a = BitSet.of_array a                                                                *)
(*	let of_bitset a = BitSet.to_array a                                                                *)
(*	let genes = length                                                                                 *)
(*	let crossover2 a a' =                                                                              *)
(*		let n = genes a in                                                                               *)
(*		let bs = BitSet.random n in                                                                      *)
(*		init n (fun i -> if BitSet.get bs i then a.(i) else a'.(i))                                      *)
(*                                                                                                     *)
(*(*	let crossover cuts chm chm' =                  *)                                                *)
(*(*		let n = Array.length chm in                  *)                                                *)
(*(*		let l = sampleInt (0,n-1) cuts in            *)                                                *)
(*(*		let chms = [|chm;chm'|] in                   *)                                                *)
(*(*		let res = Array.copy chm in                  *)                                                *)
(*(*		let rec work o sel = function                *)                                                *)
(*(*			| [] -> Array.blit chms.(sel) o res o (n-o)*)                                                *)
(*(*			| x::xs ->                                 *)                                                *)
(*(*				Array.blit chms.(sel) o res o (x-o);     *)                                                *)
(*(*				work x (1-sel) xs in                     *)                                                *)
(*(*		work 0 0 l;                                  *)                                                *)
(*(*		res                                          *)                                                *)
(*(*let crossover chms =                       *)                                                      *)
(*(*	let chm = chms.(0) in                    *)                                                      *)
(*(*	let k = length chms in                   *)                                                      *)
(*(*	let n = length chm in                    *)                                                      *)
(*(*	init n (fun i -> chms.(Random.int k).(i))*)                                                      *)
(*	let mutate p chm' =                                                                                *)
(*		let chm = Array.copy chm' in                                                                     *)
(*		let n = length chm in                                                                            *)
(*		let k = int_of_float (p *. float n) in                                                           *)
(*		let a = Array.of_list @$ sampleInt (0,n-1) k in                                                  *)
(*		for i =0 to length a -1 do                                                                       *)
(*			chm.(a.(i)) <- mutateGene chm.(a.(i))                                                          *)
(*		done;                                                                                            *)
(*		chm                                                                                              *)
(*	let random chm = Array.init (Array.length chm) (fun i -> mutateGene chm.(i))                       *)
(*                                                                                                     *)
(*end                                                                                                  *)
(*(*	let repr a fills =                                                           *)                  *)
(*(*		let sb = Buffer.create 3 in                                                *)                  *)
(*(*		for i = 0 to length a -1 do                                                *)                  *)
(*(*			match a.(i) with                                                         *)                  *)
(*(*				| GStr (s,_) ->                                                        *)                  *)
(*(*					Buffer.add_string sb s;Buffer.add_string sb fills.(i)                *)                  *)
(*(*				| GInt (v,_,_) ->                                                      *)                  *)
(*(*					Buffer.add_string sb (string_of_int v);Buffer.add_string sb fills.(i)*)                  *)
(*(*		done;                                                                      *)                  *)
(*(*		Buffer.contents sb                                                         *)                  *)
(*(*module type EvolveConfig = sig                                         *)                          *)
(*(*	type chrome                                                          *)                          *)
(*(*	val popSizeThresholds : int * int                                    *)                          *)
(*(*(*	val fitnessTime : int (* suggested time for each run *)*)          *)                          *)
(*(*(*	val fitness : chrome -> float*)                                    *)                          *)
(*(*end                                                                    *)                          *)
(*(*                                                                       *)                          *)
(*(*module LocalExecConfig = struct                                        *)                          *)
(*(*	let popSizeThresholds = 30,50                                        *)                          *)
(*(*(*	let fitness = *)                                                   *)                          *)
(*(*end                                                                    *)                          *)
(*(*                                                                       *)                          *)
(*(*module Evolve(C:Chrome)(E:EvolveConfig with type chrome = C.t) = struct*)                          *)
(*(*	let evolve =                                                         *)                          *)
(*(*                                                                       *)                          *)
(*(*end                                                                    *)                          *)
(*                                                                                                     *)
type strategy =
		| Elitism of float (* how many elites to clone *)
		| ParentAndChild
		| ChildOnly
module Population(C:CHROME) = struct
	open Array
	module C= C
	type t = C.t array
  let parentTable = Hashtbl.create 3
  let parents t = try Hashtbl.find_all parentTable t with Not_found -> []
	let create n chm =
		init n (fun i -> if i=0 then chm else C.random chm)
  let crossover2 a a' = 
    let r = C.crossover [|a;a'|] in
    Hashtbl.add parentTable r a;
    Hashtbl.add parentTable r a';
    r
  let mutate mutationSeverity a =
    let r = C.mutate mutationSeverity a in
    Hashtbl.add parentTable r a;
    r
	let reproduce (mutationRatio,mutationSeverity) a n =
		let cnt = ref 0 in
		let l = ref [] in
		while !cnt < n do
			let a' = init (length a-1) (fun i -> crossover2 a.(i) a.(i+1)) in
			addRefList
				(map (mutate mutationSeverity) (sample a' (fraction mutationRatio (length a')))) l;
			addRefList a' l;
			cnt := length a' + !cnt
		done;
		concat !l
	let evolve minPopSize maxPopSize strategy migrationRatio
										(mutationRatio,mutationSeverity) fitness compare a  =
		let n = maxPopSize in
		let childrenSize = match strategy with
			| Elitism p -> min maxPopSize (fraction (1.-.p) n)
			| _ -> maxPopSize in
		let a' = reproduce (mutationRatio,mutationSeverity) (shuffle migrationRatio a) childrenSize in
		let a'' = sortByMap (flip compare) fitness a' in
		fprintf stderr "[iter]children = %d\n" (length a');
		let r = take maxPopSize @$
			match strategy with
				| Elitism p ->
				  mergeSortedBy (fun e e' -> flip compare (fitness e ) (fitness e')) (take (fraction p n) a) (take (fraction (1.-.p) n) a'')
				| ParentAndChild ->
					mergeSortedBy (fun e e' -> flip compare (fitness e ) (fitness e')) a a''
				| ChildOnly ->
					a'' in
    fprintf stderr "<%s> [evolve]\n %s \n %s\n" (ExtUnix.ppTime (Unix.time ())) (show_array C.show r) @$ 
        show_array ~sep:",\n" (fun c -> C.show c ^ " <== " ^ show_list C.show @$ parents c) r;          
    r
end

(*let evolve pop =*)

(*let () =                                                                                         *)
(*	let f = fun i -> (i-10) * (i-20) in                                                            *)
(*	let l' = range 0 30 in                                                                         *)
(*	let l = List.map f l' in                                                                       *)
(*	print_endline @$ show_list string_of_int l;                                                    *)
(*	printf "opt = %d\n" @$ findOptAll (neg*@f) l';                                                 *)
(*	printf "opt = %d\n" @$ findOptRand 0.1 (neg*@f) l';                                            *)
(*	printf "opt = %d\n" @$ findOptUniPeakInterval 4 (neg*@f) 0 29;                                 *)
(*	iter print_intarray (map (sample (Array.of_list @$ range 0 10)) (Array.of_list @$ range 0 20));*)

let greedyBalance l =
	let rec work a a' l l' = function
		| [] -> l
		| b :: xs ->
			if abs (a+b-a') < abs (a-b-a') then work (a+b) a' (b::l) l' xs
			else work a (a'+b) l (b::l') xs in
	work 0 0 [] [] l
let randBalance n l'=
	let a = Array.of_list l' in
	let s = Algebra.IntList.sum l'/2 in
		ExtList.maximumByMap compare (fun l -> - Algebra.Int.sqr (sum l -s) )
			@$ List.map (fun _ -> Array.to_list @$ sample a (length a/2)) (ExtList.range 0 n)
module P = Population(BinaryChrome)			
let geneticBalance m n l =
	let a = of_list l in
	let s = sum l / 2 in
	let express v =
		let res = ref [] in
(*		print_endline @$ BinaryChrome.show v;*)
		Array.iteri (fun i b -> if b then addRefList a.(i) res) @$ BinaryChrome.to_array v ;
(*		BitSet.iteri (fun i b -> if b then addRefList a.(i) res) (BinaryChrome.to_bitset v);*)
		!res in
	let fitness v =
(*    fprintf stderr "fitness %s\n" @$ BitSet.show v;*)
(*    flush stderr;                                  *)
    -abs(IntList.sum (express v) - s) in
	express @$ List.hd @$
		ExtList.sortByMap (flip compare) fitness @$
		List.map (fun _ -> ExtArray.hd @$ 
		(funPower n (P.evolve 30 50 (Elitism 0.1) 0.1 (0.1,0.1) fitness compare)) @$ 
(*			(P.create 30 (BinaryChrome.of_bitset @$ BitSet.create (length a) false)))*)
			(P.create 30 (BinaryChrome.of_array @$ Array.make (length a) false)))
				(ExtList.range 0 m)
let climbHill cur delta cmp fitness =
	hd @$ sortByMap cmp fitness (delta cur)
(*let () =                                    *)
(*	let l = [1;3;5;34;3;21] in                *)
(*	let show_res str f l =                    *)
(*		print_endline str;                      *)
(*(*		let (l,a,l',a') = f l in*)            *)
(*(*		print_intlist @$ l;*)                 *)
(*(*		printf "sum(l) = %d\n" a;*)           *)
(*		let l' = f l in                         *)
(*		let a' = IntList.sum l' in              *)
(*		print_endline @$ show_intlist @$ l';    *)
(*		printf "sum(l') = %d\n" a';             *)
(*		printf "---------\n";                   *)
(*		in                                      *)
(*	print_endline @$ show_intlist @$ l;       *)
(*	printf "sum = %d\n" (IntList.sum l);      *)
(*	show_res "greedy:" greedyBalance l;       *)
(*	Random.self_init ();                      *)
(*	show_res "rand:" (randBalance 10000) l;   *)
(*	show_res "genetic:" (geneticBalance 4 3) l*)

(*let tc = fromSome @$ PartialOrder.TransitiveClosure.ofPartialOrder @$ *)
(*    PartialOrder.of_list [1,2;2,3]                                    *)
(*                                                                      *)
(*                                                                      *)
(*module PO = Population(struct                                         *)
(*  type t = int list                                                   *)
(*  let random =                                                        *)
(*    fromSome *@ T27.PartialOrder.random tc                            *)
(*(*  let countGenes = Algebra.dumb*)                                   *)
(*  let crossover = T27.PartialOrder.crossover                          *)
(*  let show l = show_intlist l                                         *)
(*  let mutate p l = fromSome @$ T27.PartialOrder.mutate tc p l         *)
(*  end)                                                                *)
  
(*let () =                                              *)
(*  Random.self_init ();                                *)
(*  let l = ExtList.range 0 5 in                        *)
(*  let t = PO.C.random l in                            *)
(*  let t' = PO.C.random l in                           *)
(*  print_endline @$ show_intlist @$ t;                 *)
(*  print_endline @$ show_intlist @$ t';                *)
(*  print_endline @$ show_intlist @$ PO.crossover2 t t';*)
(*  print_endline @$ show_intlist @$ PO.C.mutate 0.1 t';*)
(*  print_endline @$ show_intlist @$ PO.C.mutate 0.5 t';*)
(*  print_endline @$ show_intlist @$ PO.C.mutate 0.8 t' *)
  
  
  