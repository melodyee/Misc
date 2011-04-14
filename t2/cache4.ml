open Printf
open Bigarray
(* In the following discussion, we will assume full associativity, as a    *)
(* set-associative cache of associativity M can be considered as a full    *)
(* associative cache of size M.                                            *)

type address = int
type isHit = float

let zipWith op a a' =
  Array.init (min (Array.length a) (Array.length a')) (fun i -> op a.(i) a'.(i))
let pair a b = a, b
let rec list_zipWith f l l2 = match l, l2 with
  | [], _
  | _,[] -> []
  | x:: xs, y:: ys -> f x y:: list_zipWith f xs ys
let list_zip l l' = list_zipWith pair l l'
let list_unzip l =
  let rec loop a b = function [] -> List.rev a, List.rev b
    | (x, y):: xs -> loop (x:: a) (y:: b) xs in
  loop [] [] l
let rec list_take n l =
  match (n, l) with
    (0, _) -> []
  | (n, (a :: b)) -> a :: (list_take (n - 1) b)
  | _ -> []
let rec list_drop n l =
  match (n, l) with
    (0, _) -> l
  | (n, (_ :: b)) -> list_drop (n - 1) b
  | _ -> []
let round f = int_of_float (floor (f +.0.5))
let sum arr = Array.fold_left ((+.)) 0. arr
let transpose a =
  if a ==[||] then [||] else
    let m = Array.length a in
    let n = Array.length a.(0) in
    let a' = Array.init n (fun i -> Array.init m (fun j -> a.(j).(i))) in
    a'
let average arr = sum arr /. float (Array.length arr)
let variance arr =
  let miu = average arr in
  average (Array.map (fun e -> (e -. miu) *. (e -. miu)) arr)
let sample_variance arr =
  let miu = average arr in
  sum (Array.map (fun e -> (e -. miu) *. (e -. miu)) arr) /. float (Array.length arr - 1)
let print_float_array arr =
  Array.iter (printf "%f ") arr;
  printf "\n"

let prepend e arr = Array.init (1 + Array.length arr)
    (fun i -> if i = 0 then e else arr.(i - 1))
let append e arr = Array.init (1 + Array.length arr)
    (fun i -> if i = Array.length arr then e else arr.(i))
let array_tail arr = Array.sub arr 1 (Array.length arr - 1)
let average_arrayAux op red n f =
  let sums = ref (f ()) in
  for i = 0 to n - 2 do
    sums := zipWith op !sums (f ())
  done;
  Array.map (fun e -> red e ) !sums
let average_array n f = average_arrayAux (+.) (fun e -> e /. float n) n f
let average_array_array n f =
  average_arrayAux (zipWith (+.)) (fun e -> Array.map (fun e -> e /. float n) e) n f

let complement arr = Array.map (fun e -> 1.-.e) arr
let array_last arr = arr.(Array.length arr - 1)
let rem a b = if a >= 0 then a mod b else b - ((- a) mod b)

module FloatPolynomial = struct
  type t = float list
  let eval z t =
    let rec work acc = function
      | [] -> acc
      | x:: xs -> work (z *.(acc +.x)) xs in
    work 0. (List.rev t)
  let z t = 0. :: t
  let mergeWith op t t' =
    let rec work t t' = match t, t' with
      | [], t' -> t'
      | t,[] -> t
      | x:: xs, y:: ys -> op x y :: work xs ys in
    work t t'
  let scale k t =
    List.map (( *. ) k) t
  let neg t = scale (- 1.) t
  let add t t' = mergeWith (+.) t t'
  let sub t t' =
    let rec work t t' = match t, t' with
      | [], t' -> neg t'
      | t,[] -> t
      | x:: xs, y:: ys -> x -. y :: work xs ys in
    work t t'
  let rec mul t t' = match t with
    | [] -> []
    | _ -> add (scale (List.hd t) t') (z (mul (List.tl t) t'))
  
  (* let rec kmul k t t' = match t with | [] -> [] | _ -> add (list_take k   *)
  (* (scale (List.hd t) t')) (z (kmul (k-1) (List.tl t) t'))                 *)
  let print t =
    List.iter (Printf.printf "%f ") t;
    printf "\n"
end

module PolyMap = struct
  module FP = FloatPolynomial
  type t = {
    map : (address, FP.t) Hashtbl.t;
    mutable offset : FP.t
  }
  let create () = {
    map = Hashtbl.create 3;
    offset = [0.];
  }
  let get t addr =
    try Some (FP.add (Hashtbl.find t.map addr) t.offset)
    with Not_found -> None
  let set t addr value =
    Hashtbl.replace t.map addr (FP.sub value t.offset)
  let addOffset t off =
    t.offset <- FP.add t.offset off
  let getOffset t = t.offset
  let print t =
    ()
  (* printf "offset = %f\n" t.offset; Hashtbl.iter (fun k v -> printf "%d -> *)
  (* %f\n" k v) t.map                                                        *)
  let remove t addr =
    Hashtbl.remove t.map addr
  let name = "PolyMap"
end

module PolynomialCache = struct
  module FP = FloatPolynomial
  type t = {
    map : PolyMap.t;
    accept : address -> FP.t;
  }
  let create k =
    let m = PolyMap.create () in
    let accept addr =
      let exi = match PolyMap.get m addr with
        | None -> [1.]
        | Some sum ->
          list_take k (FP.z sum)
(*            FP.add (list_take k (FP.z sum)) (FP.scale 0.001 (list_drop k (FP.z sum)))*)
(*            list_take k (FP.add (FP.z sum) (FP.mul (FP.z sum) (FP.z sum)))*)
      in
      let ehit = FP.sub [1.] exi in
      PolyMap.addOffset m exi;
      PolyMap.set m addr [0.];
      ehit
    in
    { map = m;
      accept = accept;}
  let accept t address = t.accept address
end

module BoundHistory = struct
  type 'a t = {
    size : int;
    content : 'a array;
    position : int ref;
    max_offset : int ref;
    read : int (* offset, 0 to be just written *) -> 'a option;
    write : 'a -> unit;
  }
  let create size =
    let content = Array.init size (fun _ -> Obj.magic 0) in
    let position = ref 0 in
    let max_offset = ref (- 1) in
    let read i =
      if i < 0 || i > !max_offset then None else
        let n = rem (!position - 1 - i) size in
        Some (content.(n)) in
    let write e =
      content.(!position) <- e;
      incr position;
      if !position >= size then position := !position - size;
      incr max_offset;
      max_offset := min (size - 1) !max_offset in
    { size = size;
      content = content;
      position = position;
      max_offset = max_offset;
      read = read;
      write = write;
    }
  let read t i = t.read i
  let write t e = t.write e
end

module type CACHE = sig
  type t
  val create : float -> int -> t
  val accept : t -> isHit (* absolute probability *) * address
  -> t * isHit (* conditional probability *)
  val accept_background : t -> float -> address -> isHit
  val print : t -> unit
(* val name : string *)
end

module Sim (C: CACHE) = struct
  let trace_to_array trace = Array.map (fun e -> (0., e)) trace
  let hitRatio array result = sum (zipWith (fun (p, _) p' -> (1. -. p) *. p') array result)
    /. sum (Array.map (fun (p, _) -> 1. -. p) array)
  let runTraceProb epsilon size array =
    let t = ref (C.create epsilon size) in
    Array.init (Array.length array) (fun i ->
            let t', isHit = C.accept !t array.(i) in
            t := t';
            isHit)
  let runTrace epsilon size trace =
    runTraceProb epsilon size (trace_to_array trace)
  
  let runTraceMultiLevelProb epsilon size_array trace =
    let t = ref (trace_to_array trace) in
    Array.map (fun size ->
            let res = runTraceProb epsilon size !t in
            (* let hitRatio = hitRatio !t res in *)
            t := zipWith (fun (p, addr) p' -> 1. -. (1.-.p) *. (1. -. p'), addr) !t res;
            (* hitRatio, *)
            sum (Array.map fst !t),
            Array.map fst !t
        (* res *)
      ) size_array
  let runNTraceMultiLevelProb n epsilon size_array trace =
    zipWith (fun a a' -> a, a')
      (average_array n (fun () ->
                Array.map fst (runTraceMultiLevelProb epsilon size_array trace)))
      (average_array_array n (fun () ->
                Array.map snd (runTraceMultiLevelProb epsilon size_array trace)))
  let runNTrace n epsilon size trace =
    let caches = Array.init n (fun _ -> C.create epsilon size) in
    Array.map (fun e ->
            average (Array.map (fun t -> let _, isHit = C.accept t e in isHit) caches))
      (trace_to_array trace)
  let runNTraceVariance n epsilon size array =
    let arr = Array.init n
        (fun _ -> average (complement (runTrace epsilon size array))) in
    average arr, sample_variance arr
  let runTraceApproxVariance lambdas consider_cov epsilon size array =
    let arr = complement (runTrace epsilon size array) in
    let covariance lambdas miss_array =
      let pre_n_sum n off array =
        let s = ref 0. in
        for i = 0 to n - 1 do
          let i' = off - i - 1 in
          if i' >= 0 then s:= !s +. array.(i)
        done;
        !s in
      let coeffs = Array.map (fun a -> a *. (1.-.a) /. (float size -. a)) miss_array in
      average (Array.mapi (fun i a ->
                  (1. -. a) *. (pre_n_sum (lambdas.(i)) i coeffs)
            ) miss_array) in
    (* let cov = if consider_cov then average (zipWith (fun a a' -> a *.       *)
    (* (1.-.a) *. (1.-.a') /. (float size -. a)) arr (array_tail arr)) else 0. *)
    (* in                                                                      *)
    let cov = if consider_cov then covariance lambdas arr else 0. in
    average arr,
    (cov +. average (Array.map (fun e -> e -. e *. e) arr))
    /. (float (Array.length array))
    
  let runTracePolynomials k array =
    let c = PolynomialCache.create k in
    Array.map (fun addr -> PolynomialCache.accept c addr) array

  let runTraceApproxVarianceComplex _ _ epsilon size array =
    let module C = Complex in
    let psum_variance size array =
      let of_float xi = { C.re = xi; C.im = 0.} in
      let get_re { C.re = re } = re in
      let rprod = ref C.one in
      let prod = ref C.one in
      let dxi_sum = ref 0. in
      let aux m exi =
        let xi = of_float exi in
        rprod := C.mul !rprod
          (C.inv (C.add C.one (C.div xi (C.sub m C.one))));
        prod := C.mul !prod
          (C.sub C.one (C.div xi m));
        get_re (C.sub !rprod !prod) in
      let m = float size in
      let m1 = of_float (float_of_int size) in
      let m2 = C.neg m1 in
      let mi = C.mul C.i m1 in
      (* let mi2 = C.mul C.i m2 in *)
      Array.map (fun e ->
              dxi_sum := !dxi_sum +. e -. e *. e;
              !dxi_sum +. (aux m1 e +. aux m2 e -. 2. *. (aux mi e)) /. 2. *. m *. m) array in
    let arr = complement (runTrace epsilon size array) in
    average arr,
    array_last (psum_variance size arr)
    /. float (Array.length array) /. float (Array.length array)
  let runNStream n epsilon size stream =
    let ts = Array.init n (fun _ -> C.create epsilon size) in
    try
	    while true do
        let addr = Stream.next stream in
        let isHit = average (Array.map (fun t -> snd (C.accept t (0.0,addr))) ts) in
        printf "%f\n" isHit
(*        ignore isHit*)
	    done
    with Stream.Failure -> ()
  exception ExitLoop
  let runConcurrentStreams epsilon size delay streams =
    let insert ((te,_,_) as e) l =
      let rec work pre = function
        | [] -> List.rev (e::pre)
        | ((tx,_,_) as x)::xs ->
          if tx <= te then work (x::pre) xs else
            (List.rev pre) @ (e :: x:: xs) in
       work [] l in
    let t = C.create epsilon size in
    let events = ref [] in
    let cycle = ref 0 in
    let misses = ref 0 in
    (try
	    while true do
	       match !events with
	        | [] -> 
            Array.iteri 
	            (fun i s -> 
	              try events := (!cycle, i, Stream.next s) :: !events
	              with Stream.Failure -> ()) streams;
            if !events = [] then raise ExitLoop
        | (c, i,addr)::es -> begin
            cycle := max !cycle c;
            printf "%d->%d\n" !cycle i;
            let isHit = snd (C.accept t (0.0,addr)) in
            (try
              let d = if isHit >0.99 then 1 else delay in
              events := insert 
                (!cycle+d, i, Stream.next streams.(i)) es
            with Stream.Failure -> events := es);
            if isHit>0.99 then begin
              incr cycle
            end else begin
              incr misses;
              cycle := !cycle + delay
            end
          end
	    done
    with ExitLoop -> ());
    !cycle, !misses
end

let extend accept =
  fun t (isHit, addr) ->
      let q = 1. -. isHit in
      if q == 1.0 || Random.float 1.0 <= q then
        let t', isHit = accept t addr in
        t', isHit
      else t, 1.

module RoundRobinCache : CACHE = struct
  type t = {
    size : int;
    content : address option array;
    position : int ref;
    presence : (address, unit) Hashtbl.t;
    accept : address -> float;
    accept_background : float -> address -> float;
  }
  let create _ size =
    let position = ref 0 in
    let content = Array.init size (fun _ -> None) in
    let presence = Hashtbl.create size in
    let evict k =
      match content.(k) with
      | Some a ->
          Hashtbl.remove presence a;
      | None -> () in
    let accept addr =
      if Hashtbl.mem presence addr then 1. else begin
        evict !position;
        Hashtbl.add presence addr ();
        content.(!position) <- Some addr;
        incr position;
        if !position >= size then position := !position - size;
        0.
      end in
    let accept_background background addr =
      if Random.float 1.0 < background then begin
        evict (!position);
        content.(!position) <- None;
        incr position;
      end;
      accept addr in
    { size = size;
      content = content;
      position = position;
      presence = presence;
      accept = accept;
      accept_background = accept_background;
    }
  let accept t (isHit, addr) =
    let q = 1. -. isHit in
    if q == 1.0 || Random.float 1.0 <= q then
      let isHit = t.accept addr in
      t, isHit
    else t, 1.
  let accept_background t background addr =
    t.accept_background background addr
  let print t =
    printf "position = %d\n" !(t.position);
    Array.iter (function
        | None -> printf ". "
        | Some addr -> printf "%d " addr) t.content
  let name = "RoundRobinCache"
end

module PLRUCache : CACHE = struct
  type node = {
    id : int;
    mutable left : node option;
    mutable right : node option;
    mutable parent : node option;
  }
  type t = {
    root : node;
    leaf2node : (address, node) Hashtbl.t;
    node2leaf : (node, address) Hashtbl.t;
  }
  let create _ size =
    let count = ref 0 in
    let n = round (log (float size) /. log 2.) in
    if abs_float (2. ** (float n) -. float size) > 0.1 then printf "2^%d <> %d\n" n size;
    let rec work i =
      incr count;
      if i = 0 then
        { id = !count;
          left = None;
          right = None;
          parent = None;}
      else begin
        let left = work (i - 1) in
        let right = work (i - 1) in
        let n = {
          id = !count;
          left = Some left;
          right = Some right;
          parent = None } in
        left.parent <- Some n;
        right.parent <- Some n;
        n
      end in
    { root = work n;
      leaf2node = Hashtbl.create size;
      node2leaf = Hashtbl.create size;}
  let bottomup t =
    let rec work pre po =
      match po with
      | Some p ->
          (match p.left with
            | Some { id = id } when id = pre.id ->
                let right = p.right in
                p.right <- p.left;
                p.left <- right
            | _ -> ());
          work p p.parent
      | None -> () in
    work t t.parent
  let accept' t addr =
    let rec work n =
      match n.left with
      | Some n' -> work n'
      | None ->
          (try Hashtbl.remove t.leaf2node (Hashtbl.find t.node2leaf n)
          with Not_found -> ());
          Hashtbl.replace t.node2leaf n addr;
          Hashtbl.add t.leaf2node addr n;
          bottomup n in
    try
      bottomup (Hashtbl.find t.leaf2node addr);
      t, 1.
    with Not_found ->
        (work t.root;
          t, 0.)
  let accept = extend accept'
  let accept_background t background addr = failwith "accept_background"
  let print t =
    let rec print_node n =
      let rec work po = match po with
        | Some p -> print_node p
        | None -> () in
      work n.left;
      work n.right;
      try printf "%d, " (Hashtbl.find t.node2leaf n)
      with Not_found -> () in
    print_node t.root
end

module LRUCache : CACHE = struct
  type t = {
    size : int;
    stack : (address list) ref;
    accept : address -> float;
  }
  let create _ size =
    let stack = ref [] in
    let accept addr =
      let rec work pre i = function
        | [] ->
            stack := addr :: !stack;
            0.
        | x :: xs ->
            if x == addr then begin
              stack := x:: (List.rev pre @ xs);
              1.
            end else
            if i < size - 1 then work (x:: pre) (i + 1) xs else begin
              stack := addr :: List.rev pre;
              0.
            end in
      work [] 0 !stack in
    { size = size;
      stack = stack;
      accept = accept;}
  let accept' t addr =
    let isHit = t.accept addr in
    t, isHit
  let accept = extend accept'
  let accept_background t background addr = failwith "accept_background"
  let print t =
    List.iter (fun e -> printf "%d " e) !(t.stack)
  let name = "LRUCache"
end

module type MAP = sig
  type t
  val create : int -> float -> t (* create size epsilon *)
  val get : t -> address -> float option
  val set : t -> address -> float -> unit
  val addOffset : t -> float -> unit
  val getOffset : t -> float
  val print : t -> unit
  val remove : t -> address -> unit
  val name : string
end



module FullMap : MAP = struct
  type t = {
    map : (address, float) Hashtbl.t;
    mutable offset : float
  }
  let create n _ = {
    map = Hashtbl.create n;
    offset = 0.;
  }
  let get t addr =
    try Some (Hashtbl.find t.map addr +. t.offset)
    with Not_found -> None
  let set t addr value =
    Hashtbl.replace t.map addr (value -. t.offset)
  let addOffset t off =
    t.offset <- t.offset +. off
  let getOffset t = t.offset
  let print t =
    printf "offset = %f\n" t.offset;
    Hashtbl.iter (fun k v -> printf "%d -> %f\n" k v) t.map
  let remove t addr =
    Hashtbl.remove t.map addr
  let name = "FullMap"
end

module BoundMap : MAP = struct
  type t = {
    mutable h' : (address, float) Hashtbl.t;
    mutable h'' : (address, float) Hashtbl.t;
    mutable offset : float;
    threshold : float;
    mutable c : float;
  }
  let create n epsilon = {
    h' = Hashtbl.create n;
    h'' = Hashtbl.create n;
    offset = 0.;
    threshold = log epsilon /. (log ( 1. -. 1. /. float n));
    c = 0.;
  }
  let get t addr =
    try Some (Hashtbl.find t.h' addr +. t.offset)
    with Not_found ->
        try Some (Hashtbl.find t.h'' addr +. t.offset)
        with Not_found -> None
  
  let set t addr value =
    Hashtbl.replace t.h' addr (value -. t.offset)
  
  let getOffset t = t.offset
  let swap t =
    Hashtbl.clear t.h'';
    let h'' = t.h'' in
    t.h'' <- t.h';
    t.h' <- h''
  
  let addOffset t off =
    t.offset <- t.offset +. off;
    t.c <- t.c +. off;
    if t.c > t.threshold then begin
      swap t;
      t.c <- t.c -. t.threshold
    end
  let print t =
    printf "offset = %f\n" t.offset;
    printf "threshold = %f\n" t.threshold;
    printf "c = %f\n" t.c;
    Hashtbl.iter (fun k v -> printf "%d -> %f\n" k v) t.h';
    printf "--\n";
    Hashtbl.iter (fun k v -> printf "%d -> %f\n" k v) t.h''
  let remove t addr =
    Hashtbl.remove t.h' addr;
    Hashtbl.remove t.h'' addr
  let name = "BoundMap"
end

(* Monte Carlo simulation *)
module MonteCarloCache = struct
  type t = {
    content : address option array;
    presence : (address, unit) Hashtbl.t;
    accept : address -> isHit;
    accept_background : float -> address -> isHit;
  }
  exception Found
  let create _ n =
    let presence = Hashtbl.create n in
    let cache = Array.init n (fun _ -> None) in
    let evict k =
      match cache.(k) with
      | Some a -> Hashtbl.remove presence a
      | None -> () in
    let accept addr =
      try Hashtbl.find presence addr; 1.
      with Not_found -> begin
            let k = Random.int n in
            evict k;
            cache.(k) <- Some addr;
            Hashtbl.add presence addr ();
            0.
          end in
    let accept_background background addr =
      if Random.float 1.0 < background then begin
        let k = Random.int n in
        evict (k);
        cache.(k) <- None;
      end;
      accept addr in
    { content = cache;
      presence = presence;
      accept = accept;
      accept_background = accept_background;
    }
  let accept' t addr =
    let isHit = t.accept addr in
    t, isHit
  let accept = extend accept'
  let accept_background t background addr =
    t.accept_background background addr
  let print t =
    Array.iter (function
        | None -> printf ". "
        | Some addr -> printf "%d " addr) t.content
  let name = "MonteCarloCache"
  let zetas ?(total = false) size trace =
    let epsilon = 0.001 in
    let module M = FullMap in
    let m = M.create size epsilon in
    let t = create epsilon size in
    Array.map (fun addr ->
            let _, isHit = accept' t addr in
            let zeta = match M.get m addr with
              | None -> - 1
              | Some sum -> round sum in
            M.addOffset m (1. -. isHit);
            M.set m addr 0.;
            if total then round (M.getOffset m) else zeta;
      ) trace
  let fixedpoint s size =
    let n = ref 0 in
    let mo = String.length s in
    let miu = ref 0. in
    let omiu = ref 1. in
    let epsilon = 0.001 in
    let t = create epsilon size in
    let rec work () =
      if !n > 1000000
      (* && abs_float (!miu -. !omiu) < epsilon *)
      then !miu else begin
        let _, ehit = accept' t (int_of_char s.[!n mod mo]) in
        omiu := !miu;
        miu := (!miu *. float !n +. 1. -. ehit) /. float (!n + 1);
        incr n;
        work ()
      end in
    work ()
end

(* Path integral simulation *)
module PathIntegralCache : CACHE = struct
  exception Found
  type t = {
    accept : address -> isHit;
  }
  let create' epsilon size threshold =
    let count = ref 0 in
    let alpha = 1. /. float size in
    let states = ref (Hashtbl.create size) in
    Hashtbl.add !states (Array.init size (fun i -> 0)) 1.;
    let states' = ref (Hashtbl.create size) in
    let accept addr =
      if size >= 4 then 0. else begin
        let swap_table a b =
          Hashtbl.clear !a;
          let t = !a in
          a := !b;
          b := t; in
        let inc h k v =
          try Hashtbl.replace h k (Hashtbl.find h k +. v)
          with Not_found ->
              (incr count;
                Hashtbl.replace h k v) in
        let sort_and_truncate n =
          (* printf "sort_and_truncate %d\n" n; *)
          flush_all ();
          let rec aux n acc l =
            if n = 0 then acc, l else
              match l with
              | [] -> assert false
              | (v, _):: xs -> aux (n - 1) (v +.acc) xs in
          let l = ref [] in
          Hashtbl.iter (fun k v -> l := (v, k)::!l) !states';
          let acc, l' = aux n 0. (List.sort compare !l) in
          let alpha = 1./.(1.-.acc) in
          Hashtbl.clear !states;
          Hashtbl.clear !states';
          List.iter (fun (v, k) -> Hashtbl.add !states k (v *.alpha)) l' in
        let res = ref 0. in
        (* Hashtbl.iter (fun k v -> Array.iter (printf "%d ") k;printf "%f\n" v)   *)
        (* !states;                                                                *)
        count := 0;
        Hashtbl.iter (fun k v ->
                try
                  Array.iter (fun e -> if e == addr then raise Found) k;
                  raise Not_found
                with
                | Found ->
                    inc !states' k v;
                    res := !res +. v;
                | Not_found ->
                    for j = 0 to size - 1 do
                    (*                    if (v *. alpha) > epsilon then*) (* this will destropy M=3 case *)
                      inc !states'
                        (Array.of_list (List.sort compare
                                ((Array.to_list (Array.mapi
                                          (fun i e -> if j = i then addr else e) k)))))
                        (v *. alpha)
                    done
          ) !states;
        if !count > 5 * threshold then sort_and_truncate (!count - threshold) else
          swap_table states states';
        !res
      end in
    { accept = accept }
  let create epsilon size =
    create' epsilon size (if size = 2 then 3 else 12)
  let accept' t addr =
    let res = t.accept addr in
    t, res
  let accept = extend accept'
  let accept_background t background addr = failwith "accept_background"
  let print t = failwith "print"
end

module type KERNEL = sig
  type map
  val create :
  int -> float ->
  map * (float -> address -> float) option
  * (float * address -> float) option * (address -> float)
  (* create size epsilon -> map * accept_with_background * accept_prob *   *)
  (* accept                                                                *)
  val print_map : map -> unit
end

module LRUSimpleKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let size' = float size in
    let m = M.create size epsilon in
    m, None, None, fun addr ->
        let exi =
          match M.get m addr with
          | Some esum ->
              if esum >= size' then 1. else esum /. size'
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        exi
  let name = "LRUSimpleKernel"
end

module LRUKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let size' = float size in
    let m = M.create size epsilon in
    let mhit = M.create size epsilon in
    m, None, None, fun addr ->
        let exi =
          match M.get m addr with
          | Some esum ->
              if esum >= size' then 1. else
                (match M.get mhit addr with
                  | Some ehitsum -> if ehitsum +. esum >= size' then esum /. size' else 0.
                  | None -> assert false)
          | None -> 1. in
        M.addOffset m exi;
        M.addOffset mhit (1. -. exi);
        M.set m addr 0.;
        M.set mhit addr 0.;
        exi
  let name = "LRUKernel"
end

module RoundRobinKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let size' = float size in
    let m = M.create size epsilon in
    let accept_with_background background addr =
      let miss = match M.get m addr with
        | Some esum ->
            esum *. (1. +. background) >= size' -. 0.001
        | None -> true in
      if miss then begin
        M.addOffset m 1.;
        M.set m addr 0.;
        1.
      end else 0. in
    m, Some accept_with_background, None, accept_with_background 0.
  let name = "RoundRobinKernel"
end

module AverageKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let alpha = 1. /. float size in
    let beta = float size in
    let m = M.create size epsilon in
    let mdist = M.create size epsilon in
    let accept_with_background background addr =
      let exi =
        let lambda = match M.get mdist addr with
          | Some lambda -> lambda
          | None -> 0. in
        match M.get m addr with
        | Some esum ->
            if esum +. background *. lambda > beta then 1. else
              alpha *. esum
        | None -> 1. in
      M.addOffset m exi;
      M.set m addr 0.;
      M.addOffset mdist 1.;
      M.set mdist addr 0.;
      exi
    in
    m, Some accept_with_background, None, fun addr ->
        let exi = match M.get m addr with
          | Some esum ->
              if esum > beta then 1. else
                alpha *. esum
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        exi
  let name = "AverageKernel"
end

module PowerKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let mdist = M.create size epsilon in
    let accept_with_background background addr =
      let exi =
        let lambda = match M.get mdist addr with
          | Some lambda -> lambda
          | None -> 0. in
        match M.get m addr with
        | Some esum ->
            1. -. ratio ** (esum +. background *. lambda)
        | None -> 1. in
      M.addOffset m exi;
      M.set m addr 0.;
      M.addOffset mdist 1.;
      M.set mdist addr 0.;
      exi
    in
    let accept_prob (isHit, addr) =
      if isHit >= 1.0 then 0. else begin
        let exi = (match M.get m addr with
            | Some esum -> 1. -. ratio ** esum
            | None -> 1.) in
        let esum' = match M.get m addr with
          | Some esum ->
              log (isHit *. ratio ** esum +. (1. -. isHit)) /. log ratio
          | None ->
              log (1. -. isHit) /. log ratio
        in
        M.addOffset m ((1. -. isHit) *.exi);
        M.set m addr esum';
        exi
      end
    in
    m, Some accept_with_background,
    Some accept_prob ,
    (* None, *)
    fun addr ->
        let exi = match M.get m addr with
          | Some esum -> 1. -. ratio ** esum
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        exi
  let name = "PowerKernel"
end

module PowerSimpleKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    m, None,
    None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum -> 1. -. ratio ** esum
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        exi
end

module PowerStatKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let m2 = M.create size epsilon in
    let m3 = M.create size epsilon in
    let ml = M.create size epsilon in
    m, None,
    None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum -> 1. -. ratio ** esum
          | None -> 1. in
        (match M.get m addr, M.get m2 addr, M.get m3 addr, M.get ml addr with
          | Some esum, Some esum2, Some esum3, Some lam ->
              printf "%f,%f,%f,%f\n" esum esum2 esum3 lam
          | _ -> ());
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset m2 (exi -. exi *.exi);
        M.set m2 addr 0.;
        M.addOffset m3 (exi -. exi *.exi *.exi);
        M.set m3 addr 0.;
        M.addOffset ml 1.;
        M.set ml addr 0.;
        exi
end

module PowerArtKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let m2 = M.create size epsilon in
    m, None,
    None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum ->
              1. -. ratio ** (esum *. 1.1)
          (* (match M.get m2 addr with | None -> assert false | Some esum2 *)
          (* -> let                                                        *)
          (* std = esum2 ** 0.5 in (* printf "std = %f\n" std;*) (* 1. -. ratio **   *)
          (* (esum +. 0.5 *. std)*) (* 1. -. 2./.3. *. ratio ** (esum -. 0.5 *.      *)
          (* std)*) (* -. 1./.3. *. ratio ** (esum +. std) *) (* 1. -. ratio **      *)
          (* esum*) ) *)
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset m2 (exi -. exi *.exi);
        M.set m2 addr 0.;
        exi
end

module PowerPreciseKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let alpha = 1. /. float size in
    let m = M.create size epsilon in
    let mdist = M.create size epsilon in
    let exib = ref 0. in
    let exic = ref 0. in
    m, None, None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum ->
              let lambda = match M.get mdist addr with
                | None -> assert false
                | Some lambda -> lambda in
              (* printf "lambda = %d\n" (round lambda); *)
              (match round lambda with
                | 0 -> assert false
                | 1 ->
                    esum *. alpha
                | 2 ->
                    let p = 1. -. (1. -. !exic) /. (1. -. !exib +. ratio *. !exib) in
                    let p1 = (1. -. !exib) *. p +. !exib *. (1. -. p) *. ratio in
                    let p2 = !exib *. (1. -. ratio *. (1. -. p)) in
                    1. -. p1 *. ratio -. p2 *. ratio *. ratio
                | _ -> 1. -. ratio ** esum)
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset mdist 1.;
        M.set mdist addr 0.;
        exib := !exic;
        exic := exi;
        exi
  let name = "PowerKernel"
end

module BinomialAvgKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let alpha = 1. /. float size in
    let m = M.create size epsilon in
    let mdist = M.create size epsilon in
    m, None, None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum ->
              let lambda = match M.get mdist addr with
                | None -> assert false
                | Some lambda -> lambda in
              1. -. (1. -. esum *. alpha /. lambda) ** lambda
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset mdist 1.;
        M.set mdist addr 0.;
        exi
end

module BinomialKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    (* let alpha = 1. /. float size in *)
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let epsilon' = 0.000001 in
    m, None, None, fun addr ->
        if size = 1 then 1. else begin
          let exi = match M.get m addr with
            | Some esum -> 1. -. ratio ** (((exp esum -. 1.) /. epsilon'))
            | None -> 1. in
          M.addOffset m (log ( 1. +. epsilon' *. exi));
          M.set m addr 0.;
          exi
        end
end

module PowerComplexKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let m2 = M.create size epsilon in
    let m3 = M.create size epsilon in
    m, None, None,
    let r2 = -. 1. /. 2. *. (abs_float (log ratio)) ** 2. in
    let r3 = -. 1. /. 6. *. (abs_float (log ratio)) ** 3. in
    fun addr ->
        let exi = match M.get m addr with
          | Some esum ->
              let a2 = match M.get m2 addr with
                | Some esum2 ->
                    r2 *. esum2
                | None -> assert false in
              let a3 = match M.get m3 addr with
                | Some esum3 ->
                    r3 *. esum3
                | None -> assert false in
              (* printf "a2 = %f\n" a2; printf "a3 = %f\n" a3; *)
              1. -. ratio ** esum +. a2 -. a3
          | None -> 1. in
        M.addOffset m exi;
        M.addOffset m2 (exi -. exi ** 2.);
        M.addOffset m3 (exi -. exi ** 3.);
        M.set m addr 0.;
        M.set m2 addr 0.;
        M.set m3 addr 0.;
        exi
  let name = "PowerKernel"
end

module PowerNoVarianceKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let beta = log ratio in
    let m = M.create size epsilon in
    let macc = M.create size epsilon in
    m, None, None,
    fun addr ->
        if size = 1 then 1. else begin
          let exi = match M.get m addr with
            | Some esum ->
                let acc = match M.get macc addr with
                  | Some acc -> acc
                  | None -> assert false in
                let delta = (ratio -. 1. -. beta) *. esum -. acc in
                (* printf "delta = %f\n" delta; *)
                1. -. ratio ** esum -. delta
            | None -> 1. in
          M.addOffset m exi;
          M.addOffset macc (ratio ** exi -. 1. -. beta *. exi);
          M.set m addr 0.;
          M.set macc addr 0.;
          exi
        end
end

module ProductKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let alpha = 1. /. float size in
    let m = M.create size epsilon in
    m, None, None, fun addr ->
        if size = 1 then 1. else begin
          let exi = match M.get m addr with
            | Some esum -> 1. -. exp esum
            | None -> 1. in
          M.addOffset m (log ( 1. -. alpha *. exi));
          M.set m addr 0.;
          exi
        end
  let name = "ProductKernel"
end

module RecipProductKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let alpha' = 1. /. float (size - 1) in
    let m = M.create size epsilon in
    m, None, None, fun addr ->
        if size = 1 then 1. else begin
          let exi = match M.get m addr with
            | Some esum -> 1. -. exp (-. esum)
            | None -> 1. in
          M.addOffset m (log ( 1. +. alpha' *. exi));
          M.set m addr 0.;
          exi
        end
  let name = "ProductKernel"
end

module HybridKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let module PoM = PowerKernel(M) in
    let _, _, _, pom = PoM.create size epsilon in
    let module PrM = ProductKernel(M) in
    let _, _, _, prm = PrM.create size epsilon in
    (* let alpha = 1. /. float size in let alpha2 = alpha *. alpha in let beta *)
    (* = log (1. -. 1. /. float size) in let beta2 = beta *. beta in           *)
    let m = M.create size epsilon in
    m, None, None, fun addr ->
        2. *. pom addr -. prm addr
  (* let r = -. alpha2 *. alpha /. (beta2 *. beta /. 6.) in (r *. pom addr   *)
  (* -. prm addr) /. (r -. 1.) (beta2 *. pom addr -. alpha2 *.prm addr) /.   *)
  (* (beta2 -. alpha2)                                                       *)
end

module CubicKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let alpha = 1. /. float size in
    let alpha2 = alpha *. alpha in
    let ratio = 1. -. 1. /. float size in
    let beta = log ratio in
    let beta2 = beta *. beta in
    let m = M.create size epsilon in
    let m2 = M.create size epsilon in
    let m3 = M.create size epsilon in
    let m4 = M.create size epsilon in
    let mprod = M.create size epsilon in
    m, None, None, fun addr ->
        let exi =
          match M.get m addr, M.get m2 addr, M.get m3 addr, M.get m4 addr, M.get mprod addr with
          | Some esum, Some esum2, Some esum3, Some esum4, Some eprod ->
          (* printf "exi_esum = %f, exi_eprod = %f\n" (1. -. ratio ** esum) (1. -. e *)
          (* ** eprod); 1. -. (2. *. ratio ** esum -. 1. *. e ** eprod (* -. 2. *.   *)
          (* 0.5 *. beta2 *. esum2 *) (* +. 2. *. 0.5 /. 3. *. beta2 *. beta *.      *)
          (* esum3*) ) 1. -. (1.5 *. ratio ** esum -. 0.5 *. e ** eprod) printf      *)
          (* "esum = %f, esum2 = %f, eprod = %f\n" esum esum2 eprod;                 *)
              1. -. (-. alpha2 *. ratio ** esum
                +. beta2 *. exp eprod
                (* -. 0.5 *. beta2 *. alpha2 *. esum2 +. 0.5 /. 3. *. beta2 *. beta *.     *)
                (* alpha2 *. esum3 -. 0.5 /. 3. /.4. *. beta2 *. beta *. beta *. alpha2 *. *)
                (* esum4                                                                   *)
              ) /. (-. alpha2 +. beta2)
          | None, _, _, _, _ -> 1.
          | _ -> assert false in
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset m2 (exi -. exi *. exi);
        M.set m2 addr 0.;
        M.addOffset m3 (exi -. exi *. exi *. exi);
        M.set m3 addr 0.;
        M.addOffset m4 (exi -. exi *. exi *. exi *. exi);
        M.set m4 addr 0.;
        M.addOffset mprod (log ( 1. -. alpha *. exi));
        M.set mprod addr 0.;
        (* printf "exi = %f\n" exi; *)
        exi
end

module ThreePointKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let md = M.create size epsilon in
    m, None,
    None,
    fun addr ->
        let exi = match M.get m addr with
          | Some esum -> begin
                match M.get md addr with
                | Some dsum ->
                    let del = esum -. floor esum in
                    let p2 = -. (dsum -. (1.-.del) ** 2. -.2.*.del *.(1.-.del)) in
                    let p1 = ((1.-.del) -.p2) *.0.5 in
                    let p3 = 1.-.p1 -.p2 in
                    (* printf "p1,p2,p3=%f,%f,%f\n" p1 p2 p3; *)
                    1. -. (p1 *. ratio ** (floor esum -.1.)
                      +. p2 *. ratio ** floor esum
                      +. p3 *. ratio ** (floor esum +.1.))
                | _ -> assert false
              end
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        M.addOffset md (exi -. exi *. exi);
        M.set md addr 0.;
        exi
end

module NormalKernel (M: MAP) : KERNEL = struct
  type map = M.t
  let print_map = M.print
  let create size epsilon =
    let ratio = 1. -. 1. /. float size in
    let beta = log ratio in
    let m = M.create size epsilon in
    let md = M.create size epsilon in
    m, None,
    None,
    fun addr ->
        if size = 1 then 1. else begin
          let exi = match M.get m addr with
            | Some esum -> begin
                  match M.get md addr with
                  | Some dsum ->
                  (* printf "extra = %f\n" (dsum /. 2. *. beta); *)
                      1. -. ratio ** (esum -. dsum /. 2. *. beta) (* wrong sign? *)
                  | _ -> assert false
                end
            | None -> 1. in
          M.addOffset m exi;
          M.set m addr 0.;
          M.addOffset md (exi -. exi *. exi);
          M.set md addr 0.;
          exi
        end
end

module ProbCache (K: KERNEL) :CACHE = struct
  type t = {
    map : K.map;
    accept : address -> isHit;
    kernel_background : (float -> address -> isHit) option;
    kernel_prob : (isHit * address -> isHit) option;
  }
  let create epsilon n =
    let m, kernel_background, kernel_prob, kernel = K.create n epsilon in
    let accept addr = 1. -. kernel addr in
    { map = m;
      kernel_background = kernel_background;
      accept = accept;
      kernel_prob = kernel_prob }
  let accept' t address =
    let isHit = t.accept address in
    t, isHit
  let accept t (p, addr) =
    match t.kernel_prob with
    | None ->
        extend accept' t (p, addr)
    | Some f ->
        if p >= 1. then t, 1. else
          let isHit = 1. -. f (p, addr) in
          t, isHit
  let accept_background t background addr =
    match t.kernel_background with
    | None -> assert false
    | Some f ->
        1. -. f background addr
  let print t = K.print_map t.map
end

module Historgram = struct
  type 'a t = ('a * int) list
  let of_array array =
    let h = Hashtbl.create 3 in
    Array.iter (fun e -> try Hashtbl.replace h e (1 + Hashtbl.find h e)
            with Not_found -> Hashtbl.add h e 1) array;
    let l = ref [] in
    Hashtbl.iter (fun k v -> l := (k, v):: !l) h;
    List.sort compare !l
  let process_array arrs : 'a t array =
    Array.map of_array (transpose arrs)
  let diff scale arr arr' =
    of_array (zipWith
          (fun e e' -> float (round (scale *. abs_float (e -. e'))) /. scale) arr arr')
  let print print_e t =
    List.iter (fun (e, i) -> printf "%s%d " (print_e e) i) t;
    printf "\n"
  let weighted_average t =
    let sf, sn =
      List.fold_left (fun (sf, sn) (f, n) -> (sf +. f *. float n, sn + n)) (0., 0) t in
    sf /. float sn
  let approx_hit_ratio t size scale =
    let n = List.fold_left (+) 0 (List.map snd t) in
    let powerKernel n = 1. -. (1. -. 1. /. float size) ** n in
    let aux i =
      let r = float i /. float scale in
      let rhs =
        sum (Array.of_list (List.map
                  (fun (f, n) ->
                        if f = 0 then float n else
                          float n *. powerKernel (float f *. r)) t)) in
      abs_float (r *. float n -. rhs) in
    list_take 10 (List.map (fun (a, b) -> a, 1. -. float b /. float scale)
          (List.sort compare (Array.to_list (Array.init scale (fun i -> aux i, i)))))
  let sum hist =
    List.fold_left (fun s (e, n) -> s + e * n) 0 hist
  let sum_float hist =
    List.fold_left (fun s (e, n) -> s +.e *. float n) 0. hist
  let count hist =
    List.fold_left (fun s (_, n) -> s + n) 0 hist
  let average sum hist =
    float (sum hist) /. float (count hist)
  let variance hist =
    let miu = average sum hist in
    List.fold_left (fun s (e, n) -> s +.(float e -.miu) *.(float e -.miu)) 0. hist /.
    float (count hist)
  let normalize hist =
    let total = float (count hist) in
    List.map (fun (e, n) -> (e, float n /. total)) hist
end
module Lambda = struct
  type t = int array (* zero represents infinity *)
  let trace_to_lambdas epsilon size trace =
    let module M = FullMap in (* cannot use BoundMap here as we don't allow to forget by offset *)
    let module PC = ProbCache(PowerKernel(BoundMap)) in
    let c = PC.create epsilon size in
    let m = M.create 3 epsilon in
    Array.map (fun addr ->
            let lam =
              if snd (PC.accept c (0., addr)) < epsilon then begin
                M.remove m addr;
                0
              end else
                (match M.get m addr with
                  | Some lambda -> round lambda
                  | None -> 0) in
            M.addOffset m 1.;
            M.set m addr 0.;
            lam
      ) trace
  let run size lambdas : isHit array =
    let module BH = BoundHistory in
    let bh = BH.create 10000 in
    let ratio = 1. -. 1. /. float size in
    let diff so so' = match so, so' with
      | Some esum, Some esum' -> Some (esum -. esum')
      | Some esum, None -> Some esum
      | None, _ -> assert false
    (* | _ -> None *)
    in
    BH.write bh 0.;
    Array.map (fun lambda ->
            let exi =
              if lambda == 0 then 1. else
                match diff (BH.read bh 0) (BH.read bh lambda) with
                | None -> 1.
                | Some esum ->
                (* printf "esum = %f\n" esum; *)
                    1. -. ratio ** esum (* PowerKernel *) in
            (match BH.read bh 0 with
              | Some esum -> BH.write bh (esum +. exi);
              | None -> assert false);
            (* printf "exi = %f\n" exi; *)
            1. -. exi
      ) lambdas
  let run_by_average size lambdas : isHit array =
    let exi = ref 1. in
    let alpha = 1. /. float size in
    let ratio = 1. -. 1. /. float size in
    Array.map (fun lambda ->
            let dexi = if lambda == 0 then (1. -. !exi) *. alpha else
                alpha *. (1. -. !exi -. ratio ** (float lambda *. !exi)) in
            exi := !exi +. dexi;
            1. -. !exi
      ) lambdas
  let runTrace epsilon size trace =
    let lambdas = trace_to_lambdas epsilon size trace in
    run_by_average size lambdas
  let berg_hit_ratio epsilon size trace =
    let lambdas = trace_to_lambdas epsilon 5000000 trace in
    let hist = Historgram.of_array lambdas in
    snd (List.hd (Historgram.approx_hit_ratio hist size 1000))
  let first_order_background_effect size background hitArray lambdas =
    let ratio = 1. -. 1. /. float size in
    Array.mapi (fun i hit -> hit *. ratio ** (lambdas.(i) *. background)) hitArray
end

let readLines shifts ic =
  let last = ref 0 in
  let dupe_count = ref 0 in
  let l = ref [] in
  try
    while true do
      let ln = input_line ic in
      let i =
        try Int32.of_string ln
        with e -> (printf "%s#\n" ln; raise e) in
      let i' = Int32.to_int (Int32.shift_right i shifts) in
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

(*let trace_of_file shifts fn =                                  *)
(*  let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in               *)
(*  let str = Array1.map_file fd int32 c_layout false (-1) in    *)
(*  Unix.close fd;                                               *)
(*  Array.init (Array1.dim str) (fun i ->                        *)
(*    Int32.to_int (Int32.shift_right (Array1.get str i) shifts))*)

let stream_of_file num shifts fn =
  let ic = open_in_bin fn in
  let s = Stream.of_channel ic in
  Stream.from (fun i ->
           if i > num then None else
          try
            let aux s = int_of_char (Stream.next s) in
            let c = aux s in
            let c2 = aux s in
            let c3 = aux s in
            let c4 = aux s in
            Some ((c lor (c2 lsl 8) lor (c3 lsl 16) lsr shifts) lor (c4 lsl (24 - shifts)))
          with Stream.Failure -> close_in ic;None)

let trace_of_file shifts fname =
  let ic = open_in fname in
  let dupe_count, array = readLines shifts ic in
  close_in ic;
  printf "[%s]:dupe_count = %d\n" fname dupe_count;
  array

let time f =
  let t1 = Sys.time () in
  f ();
  Sys.time () -. t1

let take n array = Array.init n (fun i -> array.(i))

let string_power str n =
  String.concat "" (Array.to_list (Array.init n (fun _ -> str)))

let rec scanl f i = function
  | [] -> []
  | x:: xs ->
      let i' = f i x in
      i':: scanl f i' xs

let lagrange xys x =
  let aux arr j x =
    let p = ref 1. in
    Array.iteri (fun i xi -> if i <> j then p := !p *. (x -. xi)) arr;
    !p in
  let xs = Array.map fst xys in
  Array.fold_left (+.) 0. (Array.mapi
        (fun i xi -> snd (xys.(i)) *. aux xs i x /. aux xs i xi) xs)

let () =
  let epsilon = 0.00001 in
  let sizes = [| 2; 4; 8; 16; 32; 64 |] in
  let len = 16 in
  let traces = [| trace_of_string (string_power "abc" len);
    trace_of_string (string_power "abcd" len);
    trace_of_string (string_power "abac" len);
    trace_of_string (string_power "abcdef" len);
    trace_of_string (string_power "abcdefghijkl" len);
    trace_of_string (string_power "abcdefghijklmnop" len);
    take 300 (trace_of_file 7 "linpack.trace");
    take 1000 (trace_of_file 12 "linpack.trace");
    take 1000 (trace_of_file 8 "linpack.trace");
    take 1000 (trace_of_file 7 "linpack.trace");
(*    take 5000 (trace_of_file 12 "linpack.trace");*)
    |] in
  
  (* let trace = take 200000 (trace_of_file 7 "linpack.trace") in *)
  
  (* let trace = (trace_of_file 7 "linpack.trace") in *)
  let stream_len = 1000 in
  let streams = [
    stream_of_file stream_len 7 "linpack00.bin";
    stream_of_file stream_len 7 "swim00.bin";
    stream_of_file stream_len 7 "wupwise00.bin";
    stream_of_file stream_len 7 "facerec00.bin";
    ] in
  let module SL = Sim(LRUCache) in
  let module SPLS = Sim(ProbCache(LRUSimpleKernel(FullMap))) in
  let module SPL = Sim(ProbCache(LRUKernel(FullMap))) in
  let module SPPL = Sim(PLRUCache) in
  let module SRR = Sim(RoundRobinCache) in
  let module SPRR = Sim(ProbCache(RoundRobinKernel(FullMap))) in
  let module SM = Sim(MonteCarloCache) in
  let module SI = Sim(PathIntegralCache) in
  let module SPM = Sim(ProbCache(PowerKernel(FullMap))) in
  let module SPB = Sim(ProbCache(PowerKernel(BoundMap))) in
  let module SPSM = Sim(ProbCache(PowerSimpleKernel(FullMap))) in
  let module SPAM = Sim(ProbCache(PowerArtKernel(FullMap))) in
  let module SPBiAM = Sim(ProbCache(BinomialAvgKernel(FullMap))) in
  let module SPBiM = Sim(ProbCache(BinomialKernel(FullMap))) in
  let module SPPM = Sim(ProbCache(PowerPreciseKernel(FullMap))) in
  let module SPCM = Sim(ProbCache(PowerComplexKernel(FullMap))) in
  let module SPAB = Sim(ProbCache(AverageKernel(BoundMap))) in
  let module SPPrM = Sim(ProbCache(ProductKernel(FullMap))) in
  let module SPPrB = Sim(ProbCache(ProductKernel(BoundMap))) in
  let module SPRPrM = Sim(ProbCache(RecipProductKernel(FullMap))) in
  let module SPRPrB = Sim(ProbCache(RecipProductKernel(BoundMap))) in
  let module SPHM = Sim(ProbCache(HybridKernel(FullMap))) in
  let module SPCuM = Sim(ProbCache(CubicKernel(FullMap))) in
  let module SPPNVM = Sim(ProbCache(PowerNoVarianceKernel(FullMap))) in
  let module SPTPM = Sim(ProbCache(ThreePointKernel(FullMap))) in
  let module SPNM = Sim(ProbCache(NormalKernel(FullMap))) in
  let module SPPStM = Sim(ProbCache(PowerStatKernel(FullMap))) in
  Random.self_init ();
  let runTraces = [SL.runTrace; SPLS.runTrace; SPL.runTrace;
    SPPL.runTrace;
    SRR.runTrace; SPRR.runTrace;
    SM.runTrace;
    SM.runNTrace 5; SM.runNTrace 50 ; SM.runNTrace 500;
    (* SM.runNTrace 5000; *)
    SPM.runTrace; SPB.runTrace; SPCM.runTrace; SPAB.runTrace; SPPrB.runTrace;
    SPBiAM.runTrace; SPBiM.runTrace;
    Lambda.runTrace;] in
  let runTracesLess = [
    SM.runNTrace 500;
    SM.runNTrace 5;
    SM.runNTrace 50;
    SL.runTrace;
    SRR.runTrace;
    (* SM.runTrace; SM.runNTrace 5000; *)
    SI.runTrace;
    SPAM.runTrace;
    SPM.runTrace;
    (* SPCM.runTrace; SPPM.runTrace; SPSM.runTrace; *)
    SPPrM.runTrace;
    (* SPBiM.runTrace; *)
    SPRPrM.runTrace;
    SPRPrB.runTrace;
    SPNM.runTrace;
    (* SPPStM.runTrace; SPTPM.runTrace; SPHM.runTrace; SPCuM.runTrace;         *)
    (* SPPNVM.runTrace;                                                        *)
    ] in
  let runTracesLessLess = [
    SM.runNTrace 500 epsilon;
    SM.runNTrace 5 epsilon;
    SM.runNTrace 50 epsilon;
    SPM.runTrace epsilon;
    SPB.runTrace 0.1;
    SPB.runTrace 0.01;
    SPRPrB.runTrace 0.01;
    ] in
  let testRunTrace ?(less = true) ?(print_trace = false) trace size =
    printf "testRunTrace\n";
    let aux f =
      let arr = (f epsilon size trace) in
      if print_trace then begin
        Array.iter (printf "%f,") arr;
        print_newline ()
      end else
        printf "hitRatio = %f\n" (average arr) in
    (* Array.iter (printf "%d, ") trace; print_newline (); *)
    List.iter aux (if less then runTracesLess else runTraces) in
  let testSizeChange normalize trace =
    printf "testSizeChange\n";
    let sizes =
      Array.init 12 (fun i -> i + 1) in
    (* Array.init 10 (fun i -> round (2. ** float (i + 1))) in *)
    let aux f = fun epsilon size trace -> average (f epsilon size trace) in
    let res = Array.map (fun f ->
              Array.map (fun size -> (f epsilon size trace)) sizes)
        (append Lambda.berg_hit_ratio (Array.of_list (List.map aux runTracesLess
                ))) in
    if not normalize then
      Array.iter (fun arr -> Array.iter (printf "%f,") arr; printf "\n") res
    else begin
      Array.iter (printf "%f,") res.(0); printf "\n";
      Array.iter (fun arr -> Array.iter (printf "%f,") arr; printf "\n")
        (Array.map (Array.mapi (fun i e ->
                      e /. res.(0).(i))) res)
    end;
    printf "\n" in
  let testRunTraceMultiLevel print_trace trace size =
    printf "testRunTraceMultiLevel\n";
    let aux f =
      let arr = (f epsilon sizes trace) in
      if print_trace then begin
        Array.iter (fun a -> Array.iter (printf "%f,") a;
                print_newline ()) (Array.map snd arr);
        print_newline ()
      end else begin
        Array.iter (printf "accumHits = %f\n") (Array.map fst arr);
        printf "--\n";
      end
    in
    (* Array.iter (printf "%d, ") trace; print_newline (); *)
    List.iter aux [SM.runTraceMultiLevelProb;
      SM.runNTraceMultiLevelProb 1;
      SM.runNTraceMultiLevelProb 5;
      SM.runNTraceMultiLevelProb 50;
      SM.runNTraceMultiLevelProb 500;
      (* SM.runNTraceMultiLevelProb 5000; SM.runNTraceMultiLevelProb       *)
      (* 50000;                                                            *)
      SPM.runTraceMultiLevelProb;
      SPB.runTraceMultiLevelProb;
      SPPrB.runTraceMultiLevelProb;] in
  let testRunTraceHistogram trace size =
    printf "testRunTraceHistogram\n";
    let reference = List.hd runTracesLessLess size trace in
    let zero _ array = Array.map (fun _ -> 0.) array in
    let aux f =
      let n' = float (Array.length reference) in
      List.iter (fun (e, i) -> printf "%f " (float i /.n'))
        (Historgram.diff 100. reference (f size trace));
      print_newline () in
    List.iter aux (zero :: List.tl runTracesLessLess) in
  let testVariance trace size =
    printf "testVariance\n";
    let lambdas = Lambda.trace_to_lambdas 0.01 50000 trace in
    let aux f =
      let miu, var = f epsilon size trace in
      let std = var ** 0.5 in
      printf "miu = %f, miu-miu^2 = %f, std = %f, 3*std = %f\n"
        miu (miu -. miu ** 2.) std (3. *. std) in
    let test_dxi epsilon size trace =
      let arr = complement (SM.runNTrace 500 epsilon size trace) in
      average arr,
      average (Array.map (fun e -> e -. e *.e) arr) /. float (Array.length trace) in
    List.iter aux [
      (* SM.runNTraceVariance 5000; *)
      SM.runNTraceVariance 500;
      SM.runNTraceVariance 5;
      SM.runNTraceVariance 50;
      (* SM.runTraceApproxVariance; *)
      test_dxi;
      (* SPM.runTraceApproxVariance lambdas false; SPM.runTraceApproxVariance    *)
      (* lambdas true;                                                           *)
      SI.runTraceApproxVariance lambdas false;
      SPB.runTraceApproxVariance lambdas false;
      SPB.runTraceApproxVariance lambdas true;
      SPRPrB.runTraceApproxVariance lambdas false;
      SPRPrB.runTraceApproxVariance lambdas true;
      (* SPRPrB.runTraceApproxVarianceComplex lambdas true; *)
      SPNM.runTraceApproxVariance lambdas false;
      SPNM.runTraceApproxVariance lambdas true;
      ] in
  let test_lambda print_lambdas trace size =
    let fs = [
      Lambda.trace_to_lambdas 0.01 50000;
      Lambda.trace_to_lambdas 0.01 500;
      Lambda.trace_to_lambdas 0.05 50;
      Lambda.trace_to_lambdas 0.01 8;
      Lambda.trace_to_lambdas 0.5 8; ] in
    if print_lambdas then begin
      let aux f =
        Array.iter (printf "%d ") (f trace);
        printf "\n" in
      List.iter aux ((fun x -> x):: fs)
    end else begin
      let arrs = List.map (fun f -> f trace) fs in
      let results = List.map (Lambda.run size) arrs in
      let reference = SPB.runTrace epsilon size trace in
      List.iter (fun arr ->
              Historgram.print (sprintf "%f,")
                (Historgram.diff 1000. reference arr)) results;
      Array.iteri (fun i arr ->
              let fname = sprintf "bz%d" i in
              let oc = open_out fname in
              Array.iter (fprintf oc "%d ") arr;
              close_out oc;
              assert (0 == Sys.command (sprintf "ls -l %s" fname));
              assert (0 == Sys.command (sprintf "bzip2 -9 %s" fname));
              assert (0 == Sys.command (sprintf "ls -l %s.bz2" fname));
              assert (0 == Sys.command (sprintf "bunzip2 %s.bz2" fname));
        ) (Array.of_list (trace :: arrs));
      List.iter (fun arr -> printf "hitRatio = %f\n" (average arr)) results
    end in
  let test_lambda_approx_hitRatio show_e trace size =
    let lambdas = Lambda.trace_to_lambdas 0.01 5000 trace in
    let hist = Historgram.of_array lambdas in
    Historgram.print (if show_e then sprintf "%d," else fun _ -> "") hist;
    printf "weighted_average = %f\n" (Historgram.weighted_average
          (List.map (fun (a, b) -> float a, b)
              (List.filter (fun (a, b) -> a <> 0) hist)));
    List.iter (fun (a, b) -> printf "Berg hit ratio = %f,%f\n" a b)
      (Historgram.approx_hit_ratio hist size 1000);
  in
  let test_background trace size =
    let backgrounds = [| 0.; 0.01; 0.02; 0.04; 0.1; 0.2; 0.4; 0.8; 1.0;|] in
    backgrounds in
  let test_fixedpoint size =
    let pats = [|"abc";"abcd";"abcde";"abcdef";"abcdefg";"abcdefgh";"abcdefghi";|] in
    let ratio = 1. -. 1. /. float size in
    let beta = log ratio in
    Array.iteri (fun i pat ->
            let exi = MonteCarloCache.fixedpoint pat size in
            let a = log (1.-.exi) /.log ratio in
            let b = float (i + 2) *.exi in
            let c = float (i + 2) *.(exi -.exi *.exi) in
            let d = float (i + 2) *.(exi -.exi *.exi *.exi) in
            printf "%f,%f,%f,%f,%f,%f\n"
              a b (a -.b) c d (-.c *.beta *.0.5 -.d *.beta *.beta /.6.)
      )
      pats in
  let work trace size =
    printf "size = %d\n" size;
    (* testRunTrace ~print_trace: true ~less: true trace size; *)
    testVariance trace size;
  in
  let inc_variances size trace =
    let difference a =
      Array.init (Array.length a - 1) (fun i -> a.(i + 1) -. a.(i)) in
    Array.iter (printf "%f,")
      (Array.map (fun hit -> hit -. hit *.hit) (SM.runNTrace 500 epsilon size trace));
    printf "\n";
    Array.iter (printf "%f,")
      (difference
          (Array.map (fun a -> variance (Array.map float a)) (transpose
                  (Array.init 10000
                      (fun _ -> MonteCarloCache.zetas ~total: true size trace)))));
    printf "\n" in
	let test_polynomial_cache () =
	    Array.iter (fun size ->
	        printf "size = %d\n" size;
	        let arr = SM.runTracePolynomials 6 traces.(1) in
	        Array.iter FloatPolynomial.print arr;printf "\n") [|2;|]
	(*        4;8;16;32;64|]*) in
  (* inc_variances 2 traces.(5) test_fixedpoint 2; let hists =               *)
  (* Historgram.process_array (Array.init 10000 (fun _ ->                    *)
  (* MonteCarloCache.zetas ~total:true 2 traces.(0))) in Array.iter          *)
  (* (Historgram.print (if true then sprintf "%d," else fun _ -> "")) hists; *)
(*  Array.iter (fun size -> Array.iter (fun trace -> work trace size) traces) sizes;*)
 Array.iter (testSizeChange true) traces;
(* testRunTraceHistogram          *)
(*    testSizeChange true traces.(6)*)
(* (trace_of_file 7 "linpack.trace") 2; testRunTraceHistogram              *)
(* (trace_of_file 7 "facerec.trace") 4; testRunTraceHistogram              *)
(* (trace_of_file 8 "swim.trace") 4; testRunTraceHistogram (trace_of_file  *)
(* 7 "wupwise.trace") 16; printf "%f\n" (average (SPM.runTrace epsilon 4   *)
(* (trace_of_file 7 "var.trace"))); *)
(* Array.iter (fun i -> testRunTrace ~print_trace: true ~less:true traces.(6) i)*)
(*    [|2;4;8;16;32;64;128|];                                                   *)
(*    test_polynomial_cache ()*)
(* testRunTrace ~print_trace: false ~less: true traces.(1) 4;*)
(* testVariance traces.(1) 4; testRunTraceMultiLevel true;   *)
(* testRunTraceMultiLevel false; testHitRatioMultiLevel ();                *)
(* testRunTraceHistogram (); testVariance (); test_lambda false;           *)
(* test_lambda_approx_hitRatio false;                                      *)
(*    ignore (SPRPrB.runTrace epsilon (int_of_string Sys.argv.(1)) (trace_of_file 7 "linpack.bin"))*)
(*    ignore (SM.runNTrace 5 epsilon (int_of_string Sys.argv.(1)) (trace_of_file 7 "linpack.bin"))*)
(*    let aux = SPB.runNStream 1 in*)
(*    let aux = SPRPrB.runNStream 1 in                 *)
(*(*    let aux = SM.runNStream 50 in*)                *)
(*    ignore (aux epsilon (int_of_string Sys.argv.(1)) *)
(*        (stream_of_file  50000000 7 "linpack00.bin"))*)
(*    let testConcurrentStream () =                                              *)
(*        let cycles,misses = SM.runConcurrentStreams epsilon 4 10               *)
(*            (Array.of_list (list_take (int_of_string Sys.argv.(1)) streams)) in*)
(*         printf "cycles=%d,misses=%d\n" cycles misses in                       *)
(*   testConcurrentStream ()                                                     *)
    