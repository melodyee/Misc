open Printf
(* In the following discussion, we will assume full associativity, as a    *)
(* set-associative cache of associativity M can be considered as a full    *)
(* associative cache of size M.                                            *)

(* we assume random replacement policy, under which an element may be      *)
(* evicted even if there are free slots                                    *)

module Cache2 = struct
  type address = int
  type isHit = bool
  type t = {
    content : address option array;
    presence : (address, unit) Hashtbl.t;
    accept : address -> isHit;
    acceptArray : address array -> isHit array;
    acceptArray2 : address array -> float;
    acceptArray3 : address array -> int -> float array;
    stat : isHit array -> int;
  }
  exception Found
  let create n =
    let presence = Hashtbl.create n in
    let cache = Array.init n (fun _ -> None) in
    let accept addr =
      try Hashtbl.find presence addr; true
      with Not_found -> begin
            let k = Random.int n in
            (match cache.(k) with
              | Some a -> Hashtbl.remove presence a
              | None -> ());
            cache.(k) <- Some addr;
            Hashtbl.add presence addr ();
            false
          end in
    let acceptArray addrs =
      Array.init (Array.length addrs) (fun i -> accept addrs.(i)) in
    let acceptArray3 addrs n =
      let zip = (fun a a' -> Array.init (Array.length a)
                    (fun i -> a.(i) +. a'.(i))) in
      let sums = ref (Array.map (fun _ -> 0.) addrs) in
      for i =0 to n-1 do 
        sums := zip !sums (Array.init (Array.length addrs)
                      (fun i -> if accept addrs.(i) then 1.0 else 0.0))
      done;
      Array.map (fun e -> e /. float n) !sums
    in
    let acceptArray2 addrs =
      let s = ref 0 in
      Array.iter (fun e -> if accept e then incr s) addrs;
      float !s in
    { content = cache;
      presence = presence;
      accept = accept;
      acceptArray = acceptArray;
      acceptArray2 = acceptArray2;
      acceptArray3 = acceptArray3;
      stat = fun arr -> Array.fold_left (fun a b -> if b then a + 1 else a) 0 arr
    }
end

module Cache3 = struct
  type address = int
  type expectation = float
  type isHit = expectation
  type t = {
    accept : address -> isHit;
    acceptArray : address array -> isHit array;
    acceptArray2 : address array -> isHit;
    acceptArray3 : address array -> int -> float array;
    stat : isHit array -> expectation ;
  }
  exception Found
  let create n =
    let epsilon = 0.01 in
    let pruned = ref 0 in
    let ratio = 1. -. 1. /. (float n) in
    let threshold = log epsilon /. log ratio in
    printf "threshold = %f\n" threshold;
    let offset = ref 0. in
    let map = Hashtbl.create 100 in
    let hsigma = ref 0. in
    let sigma = ref Int64.zero in
    let kappa = ref 0 in
    let offset' = ref 0 in
    let map' = Hashtbl.create 100 in
    let accept addr =
      let ehit =
        try
          let r = ratio ** (Hashtbl.find map addr +. !offset) in
          (try
            sigma := Int64.add !sigma (Int64.of_int (Hashtbl.find map' addr + !offset'));
            hsigma := !hsigma +. 1. /. float (Hashtbl.find map' addr + !offset');
            incr kappa
          with Not_found -> assert false);
          r
        with Not_found -> 0. in
      offset := !offset +. 1. -. ehit;
      incr offset';
      if false && !offset > 2. *. threshold then begin
        (* printf "offset = %f\n" !offset; *)
        let off = !offset in
        Hashtbl.iter (fun k v ->
                if v +. off > threshold then (Hashtbl.remove map k; incr pruned)
                else Hashtbl.replace map k (v +.threshold)) map;
        offset := !offset -. threshold
      end;
      Hashtbl.replace map addr (-. !offset);
      Hashtbl.replace map' addr (- !offset');
      ehit in
    let acceptArray addrs =
      Array.init (Array.length addrs) (fun i -> accept addrs.(i)) in
    let acceptArray2 addrs =
      let s = ref 0. in
      Array.iter (fun e -> s := !s +. accept e) addrs;
      printf "pruned = %d\n" !pruned;
      printf "lambda = %f\n" (Int64.to_float !sigma /. float !kappa);
      printf "harmonic lambda = %f\n" (1./. (!hsigma /. float !kappa));
      !s in
    { accept = accept;
      acceptArray = acceptArray;
      acceptArray2 = acceptArray2;
      acceptArray3 = (fun addrs _ -> acceptArray addrs);
      stat = fun arr -> Array.fold_left (+.) 0. arr;
    }
end

module Cache4 = struct
  type address = int
  type expectation = float
  type isHit = expectation
  type t = {
    accept : address -> isHit;
    acceptArray : address array -> isHit array;
    acceptArray2 : address array -> isHit;
    acceptArray3 : address array -> int -> float array;
    stat : isHit array -> expectation ;
  }
  exception Found
  let create n =
    let epsilon = 0.01 in
    let cleared = ref 0 in
    let ratio = 1. -. 1. /. (float n) in
    let k = log epsilon /. log ratio in
    printf "threshold = %f\n" k;
    let offset = ref 0. in
    let c = ref 0. in
    let h' = Hashtbl.create 100 in
    let h'' = Hashtbl.create 100 in
    let h''notCleared = ref true in
    let atLower = ref true in
    let accept addr =
      let ehit =
        if !atLower then
          try ratio ** (Hashtbl.find h' addr +. !offset)
          with Not_found ->
              (try ratio ** (Hashtbl.find h'' addr +. !offset)
              with Not_found -> 0.)
        else
          try ratio ** (Hashtbl.find h'' addr +. !offset)
          with Not_found ->
              (try ratio ** (Hashtbl.find h' addr +. !offset)
              with Not_found -> 0.)
      in
      offset := !offset +. 1. -. ehit;
      c := !c +. 1. -. ehit;
      if !c >= 2. *. k then begin
        atLower := true;
        c := !c -. 2.*.k;
        Hashtbl.clear h';
        incr cleared;
        h''notCleared := true;
      end else if !h''notCleared && !c >= k then begin
        atLower := false;
        Hashtbl.clear h'';
        incr cleared;
        h''notCleared := false;
      end;
      (* printf "offset = %f\n" !offset; *)
      if !atLower then Hashtbl.replace h' addr (-. !offset) else
        Hashtbl.replace h'' addr (-. !offset);
      ehit in
    let acceptArray addrs =
      Array.init (Array.length addrs) (fun i -> accept addrs.(i)) in
    let acceptArray2 addrs =
      let s = ref 0. in
      Array.iter (fun e -> s := !s +. accept e) addrs;
      printf "table cleared = %d\n" !cleared;
      !s in
    { accept = accept;
      acceptArray = acceptArray;
      acceptArray2 = acceptArray2;
      acceptArray3 = (fun addrs _ -> acceptArray addrs);
      stat = fun arr -> Array.fold_left (+.) 0. arr;
    }
end

let readLines ic =
  let last = ref 0 in
  let dupe_count = ref 0 in
  let l = ref [] in
  try
    while true do
      let ln = input_line ic in
      let i =
        try Int32.of_string ln
        with e -> (printf "%s#\n" ln; raise e) in
      let i' = Int32.to_int (Int32.shift_right i 12) in
      if i' == !last then incr dupe_count else begin
        (* printf "i' = %d\n" i'; *)
        l := i' :: !l;
        last := i';
      end
    done;
    failwith "readLines"
  with End_of_file -> !dupe_count, Array.of_list (List.rev !l)

let trace_of_string s =
  Array.init (String.length s) (fun i -> int_of_char s.[i])

let time f =
  let t1 = Sys.time () in
  f ();
  Sys.time () -. t1

module C = Cache3
let test_file fname size =
  Random.self_init ();
  let ic = open_in fname in
  let dupe_count, pages = readLines ic in
  close_in ic;
  let cache = C.create size in
  let hits = ref 0. in
  let t = time (fun () -> hits := cache.C.acceptArray2 pages) in
  printf "time = %f\n" t;
  printf "hits = %f\n" !hits;
  printf "length = %d\n" (Array.length pages);
  printf "dupe_count = %d\n" dupe_count

let opt_cache () =
  Array.iter (fun i -> printf "cache size = %d\n" i;
          test_file "linpack.trace" i) (Array.init 20 (fun i -> 100 * (i + 1)))

let test_file2 fname size num =
  Random.self_init ();
  let ic = open_in fname in
  let dupe_count, pages = readLines ic in
  close_in ic;
  let cache = C.create size in
  let hits = ref [||] in
  let t = time (fun () -> hits := cache.C.acceptArray3 pages num) in
  printf "time = %f\n" t;
  printf "hits = %f\n" (Array.fold_left (+.) 0. !hits);
  printf "length = %d\n" (Array.length pages);
  printf "dupe_count = %d\n" dupe_count;
  let len = 100 in
  Array.iter (fun e -> printf "%f\n" e) (Array.sub !hits (Array.length !hits-1-len) len)

let () =
(*  test_file2 "linpack.trace" 256 5*)
    opt_cache ()

(* open Cache3 let cache = create 4 let hitArray = cache.acceptArray       *)
(* (trace_of_string "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd") let stat = *)
(* cache.stat hitArray                                                     *)