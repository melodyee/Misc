open Printf
open List
(* let cmd = map succ [1;3;2] module S = struct let f = succ end *)

(* let queens n = *)

module IntegralPolynomial = struct
  type t = int list
  let z t = 0 :: t
  let mergeWith op t t' =
    let rec work t t' = match t, t' with
      | [], t' -> t'
      | t,[] -> t
      | x:: xs, y:: ys -> op x y :: work xs ys in
    work t t'
  let add t t' = mergeWith (+) t t'
  let scale k t =
    List.map (( * ) k) t
  let rec mul t t' = match t with
    | [] -> []
    | _ -> add (scale (List.hd t) t') (z (mul (List.tl t) t'))
  let print t =
    List.iter (Printf.printf "%d ") t;
    printf "\n"
end

module FloatPolynomial = struct
  type t = float list
  let z t = 0. :: t
  let mergeWith op t t' =
    let rec work t t' = match t, t' with
      | [], t' -> t'
      | t,[] -> t
      | x:: xs, y:: ys -> op x y :: work xs ys in
    work t t'
  let add t t' = mergeWith (+.) t t'
  let scale k t =
    List.map (( *. ) k) t
  let rec mul t t' = match t with
    | [] -> []
    | _ -> add (scale (List.hd t) t') (z (mul (List.tl t) t'))
  let print t =
    List.iter (Printf.printf "%f ") t;
    printf "\n"
end

let lagrange xys x =
  let aux arr j x =
    let p = ref 1. in
    Array.iteri (fun i xi -> if i <> j then p := !p *. (x -. xi)) arr;
    !p in
  let xs = Array.map fst xys in
  Array.fold_left (+.) 0. (Array.mapi
        (fun i xi -> snd (xys.(i)) *. aux xs i x /. aux xs i xi) xs)

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

let print_matrix mat =
  printf "[";
  Array.iter (fun arr -> Array.iteri (fun i e -> if i < (Array.length arr - 1) then
                    printf "%f," e else printf "%f" e) arr; printf ";") mat;
  printf "]\n"

let xmatrix n arr =
  Array.map (fun xi -> Array.init n (fun i -> xi ** (float i))) arr

(* let pre s = print_matrix (xmatrix ( Array.map float_of_string           *)
(* (Array.of_list (splitChar s ','))))                                     *)

module C = Complex
let variance size array =
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
          (* printf "dxi_sum = %f\n" !dxi_sum; *)
          let del = (aux m1 e +. aux m2 e -. 2. *. (aux mi e)) /. 2. *. m *. m in
          (* printf "del = %f\n" del; *)
          !dxi_sum +. del) array

let transpose a =
  if a ==[||] then [||] else
    let m = Array.length a in
    let n = Array.length a.(0) in
    let a' = Array.init n (fun i -> Array.init m (fun j -> a.(j).(i))) in
    a'
let map f a =
  Array.map (Array.map f) a
let zipWith f a a' =
  let zipWith' f a a' =
    Array.mapi (fun i e -> f e a'.(i)) a in
  Array.mapi (fun i e -> zipWith' f e a'.(i)) a
let complement a = map (fun i -> not i) a
let mul a a'=
  Array.init (Array.length a) (fun i ->
          Array.init (Array.length a'.(0)) (fun k ->
                  Array.fold_left (||) false
                    (Array.init (Array.length a.(0)) (fun j -> a.(i).(j) && a'.(j).(k)))
            )
    )
let eye a = Array.mapi (fun i e -> Array.mapi (fun j _ -> i = j) e) a
let right_inverse a =
  complement (mul (complement (eye a)) (transpose a))

let a = [|
  [| true; true |];
  [| true; false |];
  |]

let s = "49, 3
50, 38
51, 135
52, 436
53, 881
54, 1505
55, 1994
56, 1942
57, 1493
58, 928
59, 451
60, 144
61, 39
62, 11"
let round f = int_of_float (floor (f +.0.5))
let fsum arr = Array.fold_left ((+.)) 0. arr
let sum arr = Array.fold_left ((+)) 0 arr
let reverse a =
  let n = Array.length a in
  Array.init n (fun i -> a.(n - 1 - i))
let mirror a = Array.append a (reverse a)
let dot a a' = Array.mapi (fun i e -> a.(i) * e) a'
let average arr = fsum arr /. float (Array.length arr)
let variance arr =
  let miu = average arr in
  average (Array.map (fun e -> (e -. miu) *. (e -. miu)) arr)
let to_array hist =
  let rec repeat n x =
    if n <= 0 then [] else x:: repeat (n - 1) x in
  let rec work acc l = match l with
    | [] -> acc
    | (e, n):: xs -> work (repeat n e@acc) xs in
  Array.of_list (work [] hist)

let of_string c1 c2 s =
  let acc = ref [] in
  List.iter (fun s -> match splitChar s c2 with
          | a:: b:: _ ->
              acc := (a, b)::!acc
          | _ -> ()
    ) (splitChar s c1);
  List.rev (!acc)

let norm_dist n miu variance =
  let pi = 4. *. atan 1. in
  let coeff = 1. /. sqrt (2. *. pi *. variance) in
  let a = Array.init n (fun i ->
            let i' = 0.5 *. float i in
            coeff *. exp (-. (i'*.i') /. variance /. 2.)) in
  Array.of_list (List.rev (List.tl (Array.to_list a)) @ (Array.to_list a))

let comb n k =
  let fact m n =
    let rec work m acc =
      if m < n then acc else work (m -. 1.) (m *. acc) in
    work m 1. in
  let n' = float n in
  let k' = float k in
  (fact n' (n' -. k' +. 1.) /. fact k' 1.)

let binomial n p k =
  comb n k *. p ** float k *. (1.-.p) ** float (n - k)

module Histogram = struct
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
end

(* let () = let hist = of_string s in let a = Array.map float (to_array    *)
(* hist) in Array.iter (printf "%f\n") a; let miu = average a in let var = *)
(* variance a in printf "miu=%f, var=%f\n" miu var; let a' = norm_dist     *)
(* (round (6.*.sqrt var)) miu var in List.iter (printf "%f\n") (List.map   *)
(* (fun (e,n) -> float n /. float (Array.length a)) hist); printf "--\n";  *)
(* Array.iter (printf "%f\n") a';                                          *)

let s = "8,6 9,103 10,730 11,2091 12,3132 13,2668 14,1111 15,159"

let normalize hist =
  let total = float (Histogram.count hist) in
  List.map (fun (e, n) -> (e, float n /. total)) hist

let print_array print_e a =
  Array.iter print_e a;
  printf "\n"

(* let () = let l = List.map (fun (a,b) -> int_of_string a,int_of_string   *)
(* b) (of_string ' ' ',' s) in printf "%d\n" (Histogram.sum l); let n = 15 *)
(* in print_array (fun (a,b) -> printf "%d,%f " a b) (Array.of_list        *)
(* (normalize l))                                                          *)

let s = "0.
0.
0.
0.
0.
0.
0.
0.000600
0.010300
0.073000
0.209100
0.313200
0.266800
0.111100
0.015900"

(* let () = let a = Array.of_list (List.map float_of_string (splitChar s   *)
(* '\n')) in let n = 16 in let n' = float n in let s = Array.fold_left     *)
(* (+.) 0. (Array.mapi (fun i e -> float i *. e) a) in let p = s /. n' in  *)
(* let var = Array.fold_left (+.) 0. (Array.mapi (fun i e -> e *. (float i *)
(* -. s) *. (float i -. s)) a) in printf "p=%f, miu = %f, var = %f, std =  *)
(* %f\n" p s var (sqrt var); print_array (printf "%f\n") (norm_dist (n/2)  *)
(* s var); Array.iter (fun p -> print_array (printf "%f\n") (Array.init n  *)
(* (binomial n p))) (Array.init 5 (fun i -> float i *. 0.08 +. 0.6));      *)

(* let predator_prey ?(alpha=0.2) ?(beta=0.002) ?(delta=0.001)             *)
(* ?(gamma=0.1) n (ix,iy) = let evolve (x,y) = let dx = alpha *. x -. beta *)
(* *. x *. y in let dy = delta *. x *. y -. gamma *. y in x+.dx,y+.dy in   *)
(* let x,y = ref ix,ref iy in Array.init n (fun _ -> let (x',y') = evolve  *)
(* (!x,!y) in x := x'; y := y';(x',y'))                                    *)

let psum arr =
  let s = ref 0. in
  Array.map (fun e ->
          s := !s +. e;
          !s) arr

let () =
 let n = 700 in let a = Array.init (2*n) (fun _ -> float (Random.int (1+Random.int 100))) in
(*  let n = 3 in                    *)
(*  let a = [| 1; 0; 1; 1 ;1; 0|] in*)
  let dummy_fp a =
    let coeff n j =
      mirror (Array.init n (fun i -> min (i + 1) j)) in
    Array.init n (fun i ->
            let c = coeff n (i + 1) in
            float (sum (dot c a)) /. float (sum c)
      ) in
  let fingerprint a =
    let width a =
      let left = Array.init n (fun i -> a.(n - 1 - i)) in
      let right = Array.init n (fun i -> a.(n + i)) in
      let left_ps, right_ps = psum left, psum right in
      Array.init n (fun i -> left_ps.(i) +. right_ps.(i)) in
    let s = psum (reverse (width a)) in
    Array.init n (fun i ->
      let g n = n*(n+1) in
            s.(i) /. float (g n - g (n-i-1))) in
  let print_floatarray a =
    Array.iter (fun e -> printf "%f, " e) a;
    printf "\n" in
  let print_intarray a =
    Array.iter (fun e -> printf "%d, " e) a;
    printf "\n" in  
  print_floatarray a;
  print_floatarray (fingerprint a);
(*  print_floatarray (dummy_fp a);*)