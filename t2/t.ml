open Printf
open List

(* let ( *@) f g = fun x -> f (g x) let (@$) f x = f x let toInt c =       *)
(* int_of_char c - int_of_char '0'                                         *)
let (<+>) s s2 =
  if s ="" then s2 else
  if s2 ="" then s else
    s^" "^s2
let cflags =
  "-O2 -ipa"

let edge_profile =
  "-fb_create fb.mid -fb_phase=4 -fb_type=2"

let opt_profile =
  "-fb_opt fb.mid"

(* let () = Sys.command *)
open Array
let dims ma = (length ma, length ma.(0))
let init_matrix m n f = init m (fun i -> init n (fun j -> f i j))
let mul ma ma' =
  let dotProduct ma i ma' j =
    let s = ref 0. in
    for k = 0 to length ma.(i) - 1 do
      s := !s +. (ma.(i).(k) *. ma'.(k).(j))
    done;
    !s in
  let (m, n) = dims ma in
  let (m', n') = dims ma' in
  init_matrix m n' (fun i j -> dotProduct ma i ma' j)

let a01 = [|
    [|0.;0.;0.;0.;0.;0.|];
    [|1.;1.;0.5;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.|];
    [|0.;0.;0.5;1.;0.;0.|];
    [|0.;1.;0.;1.;1.;0.|];
    [|1.;0.;1.;0.;0.;1.|]
    |]

let a10 = [|
    [|0.;0.;0.;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.|];
    [|1.;0.5;1.;0.;0.;0.|];
    [|0.;0.5;0.;1.;0.;0.|];
    [|0.;0.;1.;1.;1.;0.|];
    [|1.;1.;0.;0.;0.;1.|]
    |]

let pow a n =
  assert (n>=1);
  let rec work i p =
    if i=0 then p else work (i-1) (mul a p) in
  work (n-1) a

let sum l = List.fold_left (+) 0 l

let l = [134521; 0; 1090865; 152457; 382675; 48771; 278455; 335197; 658447; 78718; 76780; 1361131; 25031; 1154248; 419254]

  let rec take n l =
    match (n, l) with
      (0, _) -> []
    | (n, (a :: b)) -> a :: (take (n - 1) b)
    | _ -> []

let () =
   printf "ratio = %f\n" (float (sum (take 8 (List.sort (fun a b -> compare b a) l))) /. float (sum l))


let files = "implicit.c mcf.c mcfutil.c output.c pbeampp.c pbla.c pflowup.c psimplex.c pstart.c readmin.c treeup.c"
