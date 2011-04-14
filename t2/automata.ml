module State = struct
  type t =
    | SAccept of int
    | SReject of int
    | SUnknown of int
  let id = function
    | SAccept i -> i
    | SReject i -> i
    | SUnknown i -> i
  let count = ref 0
  let mark_accept x = SAccept (id x)
  let create () =
    incr count;
    SUnknown !count
end
type alphabet = int
type state = State.t
  module S = Set.Make(struct
      type t = state
      let compare s s' = compare (State.id s) (State.id s')
    end
    )
    
  let uniq l =
    S.elements (List.fold_right S.add l S.empty)

module EpsNFA = struct
  open List
  type table = (state, (alphabet option, state list) Hashtbl.t) Hashtbl.t
  type t = {
    table : table;
    root : state;
    reject : state;
  }
  let rec eps_closure t s =
    try uniq (concat (map (eps_closure t) (Hashtbl.find (Hashtbl.find t.table s) None))) 
    with Not_found -> []
  let step t input states =
    let aux s =
      let non_eps = try Hashtbl.find (Hashtbl.find t.table s) input
      with Not_found -> [t.reject] in
      eps_closure t s @ non_eps in
    uniq (concat (map aux states))
end
    
module NFA = struct
  open List
  type table = (state, (alphabet, state list) Hashtbl.t) Hashtbl.t
  type t = {
    table : table;
    root : state;
    reject : state;
  }
  let step t input states =
    let aux s =
      try Hashtbl.find (Hashtbl.find t.table s) input
      with Not_found -> [t.reject] in
    uniq (concat (map aux states))
  let elim_eps_rules t =
    let aux s h =
      let h' = Hashtbl.create 3 in
      TODO List.iter EpsNFA.eps_closure s
    Hashtbl.iter aux t.table
end

module DFA = struct
  open List
  type table = (state, (alphabet, state) Hashtbl.t) Hashtbl.t
  type t = {
    table : table;
    root : state;
    reject : state;
  }
  let uniq l =
    S.elements (List.fold_right S.add l S.empty)
  let step t input state =
    let aux s =
      try Hashtbl.find (Hashtbl.find t.table s) input
      with Not_found -> t.reject in
    aux state
(* let ofNFA nfa = let state = [nfa.root]; *)
    
  end
