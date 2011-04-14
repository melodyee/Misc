(*
 *
 * Copyright (c) 2009-, 
 *  Shuchang Zhou    <zhoushuchang@ict.ac.cn>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open List
open Printf

type address = int
module type FIELD = sig
  type t
  val (+.) : t -> t -> t
  val (-.) : t -> t -> t
  val ( *.) : t -> t -> t
  val (/.) : t -> t -> t
  val ( **) : t -> t -> t
  val log : t -> t
  val ofFloat : float -> t
  val zero : t
  val one : t
end
module Float = struct
  type t = float
  let lift2 f = f
  let (+.) = lift2 (+.)
  let (-.) = lift2 (-.)
  let ( *.) = lift2 ( *.)
  let (/.) = lift2 (/.)
  let ( **) = lift2 ( **)
  let log = log
  let ofFloat f = f
  let zero = 0.
  let one = 1.
end

(* assume all positive *)
module Interval (F: FIELD) = struct
  type t = F.t * F.t
  let norm (a, b) = if a > b then (b, a) else (a, b)
  let lift f (a, b) = norm (f a, f b)
  let lift2 f (a, b) (c, d) = norm (f a c, f b d)
  let (+.) = lift2 F.(+.)
  let (-.) (a, b) (c, d) = norm (F.(-.) a d, F.(-.) b c)
  let ( *.) (a, b) (c, d) =
    let min2 a (c, d) = min (F.( *.) a c) (F.( *.) a d) in
    let max2 a (c, d) = max (F.( *.) a c) (F.( *.) a d) in
    (min (min2 a (c, d)) (min2 b (c, d)), max (max2 a (c, d)) (max2 b (c, d)))
  let (/.) (a, b) (c, d) = ( *.) (a, b) (norm (F.(/.) F.one c, F.(/.) F.one d))
  let ( **) = lift2 F.( **)
  let log = lift log
  let ofFloat f = (f, f)
  let zero = ofFloat 0.
  let one = ofFloat 1.
end
module IntervalFloat = Interval(Float)

module type MAP = sig
  type t
  val create : int -> float -> t (* create size epsilon *)
  val get : t -> address -> float option
  val set : t -> address -> float -> unit
  val addOffset : t -> float -> unit
  val remove : t -> address -> unit
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
  let remove t addr = Hashtbl.remove t.map addr
end
module BoundMap(F: FIELD) = struct
  type t = {
    mutable h' : (address, F.t) Hashtbl.t;
    mutable h'' : (address, F.t) Hashtbl.t;
    mutable offset : F.t;
    threshold : F.t;
    mutable c : F.t;
  }
  open F
  let create n epsilon = {
    h' = Hashtbl.create n;
    h'' = Hashtbl.create n;
    offset = zero;
    threshold = log epsilon /. (log (one -. one /. ofFloat (float n)));
    c = zero;
  }
  let get t addr =
    try Some (Hashtbl.find t.h' addr +. t.offset)
    with Not_found ->
        try Some (Hashtbl.find t.h'' addr +. t.offset)
        with Not_found -> None
  
  let set t addr value =
    Hashtbl.replace t.h' addr (value -. t.offset)
  
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
  let remove t addr =
    Hashtbl.remove t.h' addr;
    Hashtbl.remove t.h'' addr;
end
module type CACHE = sig
  type t
  val create : float -> int -> t
  val miss : t -> address -> float
end

module type CACHE2 = sig
  type t
  val create : float -> int -> int -> t
  val miss : t -> address -> float * float
end

module ProbCache : CACHE = struct
  module M = BoundMap(Float)
  type t = {
    map : M.t;
    miss : address -> float;
  }
  let create epsilon size =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    { map = m;
      miss = fun addr ->
            let exi = match M.get m addr with
              | Some esum -> 1. -. ratio ** esum
              | None -> 1. in
            M.addOffset m exi;
            M.set m addr 0.;
            exi
    }
  let miss t address = t.miss address
end

module ProbCache4 : CACHE = struct
  module M = BoundMap(Float)
  type t = {
    map : M.t;
    miss : address -> float;
  }
  let create epsilon size =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    { map = m;
      miss = fun addr ->
            let exi = match M.get m addr with
              | Some esum -> 1. -. ratio ** esum
              | None -> 1. in
            M.addOffset m (-. log (1. +. exi /. (float size -. 1.)) /. log ratio);
            M.set m addr 0.;
            exi
    }
  let miss t address = t.miss address
end

let argmin s e skip f g =
  let m = ref 1. in
  for i = s to e do
    let v = if i <> skip then f i else g in
    if v < !m then m := v
  done;
  !m

module LowerCache : CACHE = struct
  module M = BoundMap(Float)
  module FM = FullMap
  type t = {
    map : M.t;
    l : FM.t;
    c : FM.t;
    miss : address -> float;
  }
  let create epsilon size =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    let l = FM.create size epsilon in
    let c = FM.create size epsilon in
    { map = m;
      l = l;
      c = c;
      miss = fun addr ->
            let exi = match M.get m addr with
              | Some esum ->
                  (match FM.get l addr, FM.get c addr with
                    | Some l_i, Some c_i ->
                        printf "%f,%f,%f\n" l_i c_i esum;
                        (* if abs_float(l_i -. c_i) < epsilon_float then 1. -. ratio ** c_i else *)
                        let f n =
                          ((ratio ** c_i -. ratio ** (float (n + 1)))
                            *. (l_i -. esum) /. (l_i -. float n)
                            +. ratio ** (float (n + 1))) in
                        1. -. argmin (int_of_float (floor c_i)) (int_of_float (ceil l_i))
                          (int_of_float esum) f (ratio ** c_i)
                    | _ -> assert false
                  )
              | None ->
                  (FM.addOffset c 1.;
                    1.)
            in
            printf "exi = %f\n" exi;
            M.addOffset m exi;
            M.set m addr 0.;
            FM.addOffset l 1.;
            FM.set l addr 0.;
            FM.set c addr 0.;
            exi
    }
  let miss t address = t.miss address
end

module ExclusiveCache = struct
  module FM = FullMap
  type t = {
        h1 : FM.t;
        h2 : FM.t;
        miss : address -> float * float;
    }
  let create epsilon size1 size2 =
    let ratio1 = 1. -. 1. /. float size1 in
    let ratio2 = 1. -. 1. /. float size2 in
    let h1 = FM.create size1 epsilon in
    let h2 = FM.create size2 epsilon in
    let alpha = ref 0. in
    let miss addr =
        let exi1, exi2 = match FM.get h1 addr with
          | Some esum1 ->
            let exi2 = (match FM.get h2 addr with
		          | Some esum2 -> 1. -. ratio1 ** esum2 
		            -. !alpha *. ratio2 ** esum1 
		          | None -> 1.) in 
            1. -. ratio1 ** esum1, exi2
          | None -> 
		        let exi2 = (match FM.get h2 addr with
		          | Some esum2 -> begin 
                alpha := !alpha +. 1. /. float size1;
                M.remove h2 addr;
                end
		          | None -> 
                1.,1.) in
            1. in
        M.addOffset h1 exi1;
        M.set h1 addr 0.;
        exi1,exi2 in
    { h1 = h1;
      h2 = h2;
      miss = miss;
      }
  let miss t address = t.miss address
  end

(* Monte Carlo simulation *)
module MonteCarloCache = struct
  
  type cache = {
    content : address option array;
    presence : (address, unit) Hashtbl.t;
    miss : address -> bool;
  }
  type t = cache array
  exception Found
  let create_cache n =
    let presence = Hashtbl.create n in
    let cache = Array.init n (fun _ -> None) in
    let evict k =
      match cache.(k) with
      | Some a -> Hashtbl.remove presence a
      | None -> () in
    let miss addr =
      try Hashtbl.find presence addr; false
      with Not_found -> begin
            let k = Random.int n in
            evict k;
            cache.(k) <- Some addr;
            Hashtbl.add presence addr ();
            true
          end in
    { content = cache;
      presence = presence;
      miss = miss;
    }
  let create epsilon n =
    let copies = int_of_float (1. /. epsilon) in
    Printf.printf "%d copies.\n" copies;
    Array.init copies (fun _ -> create_cache n)
  let miss t address =
    let avg arr = Array.fold_left (+.) 0. arr /. (float_of_int (Array.length arr)) in
    avg (Array.map (fun cache -> if cache.miss address then 1.0 else 0.) t)
end

module InclusiveMonteCarloCache :CACHE2 = struct
  type cache = {
    l1 : MonteCarloCache.cache;
    l2 : MonteCarloCache.cache;
    miss : address -> bool * bool;
  }
  type t = cache array
  let create epsilon n n2 =
    let copies = int_of_float (1. /. epsilon) in
    Printf.printf "%d copies.\n" copies;
    flush_all ();
    let create_cache () =
      let l1 = MonteCarloCache.create_cache n in
      let l2 = MonteCarloCache.create_cache n2 in
      let miss address =
        let m1 = l1.MonteCarloCache.miss address in
        let m2 = l2.MonteCarloCache.miss address in 
        (m1, m1 && m2) in
      { l1 = l1;
        l2 = l2;
        miss = miss;
      } in
    Array.init copies (fun _ -> create_cache ())
  let float_of_bool b = if b then 1.0 else 0.
  let miss t address =
    let avg arr =
      let (x, y) = Array.fold_left (fun (a, b) (c, d) -> (a +.c, b +.d)) (0., 0.) arr in
      let alpha = 1. /. (float_of_int (Array.length arr)) in
      (alpha *. x, alpha *.y) in
    avg (Array.map (fun cache -> 
      let (a,b) = cache.miss address in (float_of_bool a, float_of_bool b)) t)
end

module ExclusiveMonteCarloCache :CACHE2 = struct
  type cache = {
    content : address option array; (*keep L1 content*)
    presence : (address, int) Hashtbl.t;
    miss : address -> float * float;
  }
  type t = cache array
  exception Found
  let create epsilon n n2 =
    let copies = int_of_float (1. /. epsilon) in
    Printf.printf "%d copies.\n" copies;
    flush_all ();
    let create_cache () =
      let presence = Hashtbl.create n in
      let cache = Array.init n (fun _ -> None) in
      let evict k =
        match cache.(k) with
        | Some a -> Hashtbl.replace presence a 1
        | None -> () in
      let miss addr =
        try
          if Hashtbl.find presence addr = 0 then (0., 0.) else begin
            (* L1 miss, L2 hit *)
            let k = Random.int n in
            evict k;
            cache.(k) <- Some addr;
            Hashtbl.replace presence addr 0;
            (1., 0.)
          end
        with Not_found -> begin
              let k = Random.int n in
              evict k;
              cache.(k) <- Some addr;
              Hashtbl.replace presence addr 0;
              (1., 1.)
            end in
      { content = cache;
        presence = presence;
        miss = miss;
      } in
    Array.init copies (fun _ -> create_cache ())
  let miss t address =
    let avg arr =
      let (x, y) = Array.fold_left (fun (a, b) (c, d) -> (a +.c, b +.d)) (0., 0.) arr in
      let alpha = 1. /. (float_of_int (Array.length arr)) in
      (alpha *. x, alpha *.y) in
    avg (Array.map (fun cache -> cache.miss address) t)
end

module Sim(C: CACHE) = struct
  let run_trace epsilon size trace =
    let cache = C.create epsilon size in
    map (C.miss cache) trace
end

module Sim2(C: CACHE2) = struct
  let run_trace epsilon size size2 trace =
    let cache = C.create epsilon size size2 in
    map (C.miss cache) trace
end

module SMC = Sim(MonteCarloCache)
module SP = Sim(ProbCache)
module SP4 = Sim(ProbCache4)
module SL = Sim(LowerCache)

module S2EMC = Sim2(ExclusiveMonteCarloCache)
module S2IMC = Sim2(InclusiveMonteCarloCache)

let print_floats arr =
  iter (printf "%f,") arr; print_newline ()

let print_float_pairs arr =
  iter (fun (a, b) -> printf "%f %f," a b) arr; print_newline ()

let cycle n l =
  let rec work n acc = if n = 0 then acc else work (n - 1) (List.rev l @ acc) in
  work n []

let () =
  let eps, size = 0.0001, 2 in
  let size2 = 4 in
  let trace = map int_of_char ['a';'b';'a';'c';'d';'b'] in
  (* let trace = map int_of_char                                             *)
  (* ['a';'b';'a';'c';'d';'b';'e';'b';'a';'b';'f';'g';'d'] in let trace =    *)
  (* [1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5;] in let   *)
  (* trace = cycle 20 [1;2;3;4;5;6;7;8;9] in let trace = cycle 20 [1;2;3] in *)
  iter print_floats [SMC.run_trace eps size trace;
    SP.run_trace eps size trace;
    SP4.run_trace eps size trace;
    (* SL.run_trace eps size trace; *)
    ];
  iter print_float_pairs [S2EMC.run_trace eps size size2 trace;
    S2IMC.run_trace eps size size2 trace;
    ]