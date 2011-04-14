open Linda
open Printf
open ExtList

module type SEARCH = sig
  type state
  val next : state -> state list
  exception End of state
  end
module Dijkstra (O:Set.OrderedType) (S:SEARCH with type state = O.t) =struct
  let search i =
    let module H = Heap.Imperative(O) in
    let h = H.create 3 in
    let rec work h =
      let m = H.pop_maximum h in
      work (List.fold_left (fun h s -> H.add h s;h) h (S.next m)) in
    H.add h i;
    try work h with S.End s -> s
  end

module Revision(S:SHOWEQ) = struct
  module M = struct
	  type elt = S.t
		type t =
		  | Left of elt list
		  | Right of elt list
		  | Same of elt list
		let right x = Right x
		let left x = Left x
		let same x = Same x
		let show =
      let lift = show_list ~sep:"\n" in  
      function
	    | Left x -> sprintf "(left %s)" (lift S.show x)
	    | Right x -> sprintf "(right %s)" (lift S.show x)
	    | Same x -> sprintf "(same %s)" (lift S.show x)
		type state = int * t list * elt list * elt list
	  exception End of state
	  let next (i,acc,l,l') = match l,l' with
		    | [],[] -> raise (End (i,acc,l,l'))
		    | [],l' -> [i-length l',Right l'::acc,[],[]]
		    | l,[] -> [i-length l,Left l::acc,[],[]]
		    | x::xs,y::ys ->
          match commonBy S.eq l l' with
            | [],(_,_) ->
                let ys',ys'' = break (S.eq x) l' in
                let xs',xs'' = break (S.eq y) l in
                [i-length ys',Right ys'::acc,l,ys'';
                 i-length xs',Left xs'::acc,xs',l'] @
                [i-2,Left [x]::Right [y]::acc,xs,ys]
(*            (match acc with [] | Left _::_ | Same _::_ -> [i-1,Left [x]::acc,xs,l'] | Right _::_ -> [])*)
(*            @ [i-1,Right [y]::acc,l,ys]                                                                *)
            | c,(xs,ys) ->
              [i+2*length c,Same c::acc,xs,ys]

  end
  include M
  let align l l' =
		let module D = Dijkstra(struct
		  type t = state
		  let compare (i,_,_,_) (i',_,_,_) = Pervasives.compare i i' 
		  end)
		  (M) in
    printf "here\n";
    let (i,l,_,_) = D.search (0,[],l,l') in
    printf "%d,%s\n" i @$ show_list ~sep:"\n" show (rev l)
  end
  
module R = Revision(ExtString) 


(*let () =                                                                                             *)
(*   print_endline @$ show_stringlist @$ longestCommonSubsequenceBy (=) ["a";"b";"c"] ["a";"c";"d";"b"]*)

module StringWithHash = struct
  type t = int * string
  let eq t t' =
    fst t = fst t' && snd t = snd t'
  let ofString s = Hashtbl.hash s,s
  let toStr t = snd t
  let zero = 0,""
  let length t = String.length (snd t)
  end

open ExtString
let () =
  let fn = Sys.argv.(1) in
  let fn' = Sys.argv.(2) in
(*  let fn = "/home/zsc/tmpd/17.s" in*)
(*  let fn' = "/home/zsc/tmpd/17.2.s" in*)
  let s = ExtUnix.readFile fn in
  let s' = ExtUnix.readFile fn' in
(*  print_endline @$ show_pair Int.show (show_list ~sep:"\n" R.show) @$ *)
(*  R.align (lines s) (lines s')*) 
  print_endline @$ show_list id ~sep:"\n" @$
    ExtList.longestCommonSubsequenceBy (=) "" String.length (lines s) (lines s')
(*    let aux s = List.map StringWithHash.ofString @$ lines s in                                              *)
(*    let chop sh sh' =                                                                                       *)
(*      let aux sh sh' =                                                                                      *)
(*	      let hs = HashSet.of_list (List.map fst sh) in                                                       *)
(*	      List.filter (fun (e,_) -> HashSet.mem hs e) sh' in                                                  *)
(*      aux sh' sh,aux sh sh' in                                                                              *)
(*    let l,l' = chop (aux s) (aux s') in                                                                     *)
(*    List.map snd @$                                                                                         *)
(*        ExtList.longestCommonSubsequenceBy StringWithHash.eq StringWithHash.zero StringWithHash.length l l' *)



