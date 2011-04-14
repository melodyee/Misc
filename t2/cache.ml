open Printf

type addr = Int32.t

module Cache = struct
  type tag = Int32.t
  type t = tag option array array
  let create assoc length : t =
    assert (length mod assoc=0);
    Array.init assoc (fun _ -> Array.init (length/assoc) (fun _ -> None))
  type state =
    | Hit of int
    | Miss of int
  exception E of state
  let run t addr =
    let len = Int32.of_int (Array.length t.(0)) in
    let pos = Int32.to_int (Int32.rem addr len) in
    let tag = Int32.div addr len in
    let assoc = Array.length t in
    try
	    for i = 0 to assoc-1 do
	      if t.(i).(pos) = Some tag then raise (E (Hit i))
      done;
      for i = 0 to assoc-1 do
          if t.(i).(pos) = None then raise (E (Miss i))
      done;
      let i = Random.int assoc in
      t.(i).(pos) <- Some tag;
      false
    with 
      | E (Hit i) -> 
        true
      | E (Miss i) -> 
        t.(i).(pos) <- Some tag;
        false

  let runSeq t seq =
    let rec work acc = function
      | [] -> List.rev acc
      | x::l -> work (run t x::acc) l in
     work [] seq
  end

let testString = "1 3 2 3 1 4 9 2 4 1 9"
let splitChar s c =
  let n = String.length s in
  let rec work i acc =
    if i >= n then List.rev acc else
      try
        let i' = String.index_from s i c in
        work (i'+ 1) (String.sub s i (i'- i):: acc)
      with Not_found -> List.rev (String.sub s i (n - i):: acc)
  in
  work 0 []
let parse s = List.map Int32.of_string (splitChar s ' ')  
let show_list ?(brackets="[","]") ?(sep =";") show l =
  fst brackets ^ (fun x -> (String.concat sep)(List.map show x)) l ^ snd brackets
let show_boollist l = show_list ~brackets:("","") ~sep:"" (fun x -> if x then "H" else "M") l
let () =
  let cache = Cache.create 1 8 in
  print_endline (show_boollist (Cache.runSeq cache (parse testString)));
  let cache2 = Cache.create 2 8 in
  print_endline (show_boollist (Cache.runSeq cache2 (parse testString)))
  