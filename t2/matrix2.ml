open Linda
open Linda.Algebra

let show_array show a = show_list show (Array.to_list a)
module Matrix (R:R) = struct
  open Array
  open ExtArray
  module R = Ring(R)
  exception WrongDimension
  type status = Plain | Transposed
  let flipStatus = function
    | Plain -> Transposed
    | Transposed -> Plain
  type width = int
  type matrix = {
    height : int;
    width : int;
    status : status;
    a : R.t array}
  type t = matrix

  let dims ma = ma.height,ma.width
  let of_array k a = 
    {height=k;
    width=Array.length a/k;
    status = Plain;
    a=a}
  let make = of_array
  let get ma i j = ma.a.(i*ma.width+j)
  let set ma i j v = ma.a.(i*ma.width+j) <- v
  let setRow ma i vs =
    iteri (fun j v -> set ma i j v) vs
  let setCol ma j vs =
    iteri (fun i v -> set ma i j v) vs
  let getRow ma i =
    init (snd @$ dims ma) (fun j -> get ma i j)
  let getCol ma j =
    init (fst @$ dims ma) (fun i -> get ma i j)
  let init m n f = of_array m (init (m*n) (fun i -> f (i/n) (i mod n)))
  let swap ma i j i' j' =
    let t = get ma i j in
    set ma i j (get ma i' j');
    set ma i' j' t
  let swapRow ma i i' =
    let t = getRow ma i in
    setRow ma i (getRow ma i');
    setRow ma i' t
  let swapCol ma j j' =
    let t = getCol ma j in
    setCol ma j (getCol ma j');
    setCol ma j' t
  let zipWith f (ma:t) (ma':t) : t =
    make ma.width @$ zipWith f ma.a ma'.a
  let add = zipWith R.add
  let sub = zipWith R.sub
  let pointMul = zipWith R.mul
  let of_int i = (1,[|i|])
(*  let copy ma = {make ma.width (Array.copy ma.a) with status = ma.status}                 *)
(*  let transpose ma = {make ma.width (Array.copy ma.a) with status = flipStatus ma.status} *)
  let show ma = show_array R.show ma.a
(*  let row ((k,a):t) i : R.t Subarray.t = Subarray.make (i*(length a/k)) a*)
end
 

(*module Submatrix (R:R) = struct         *)
(*  module M = Matrix(R)                  *)
(*  type submatrix = (int * int ) * matrix*)
(*end                                     *)

module MI = Matrix(Int)
let () = 
  let a = MI.make 1 [|1;2;3|] in
  let a' = MI.make 3 [|1;2;3|] in
  print_endline @$ MI.show @$ a';
  print_endline @$ MI.show @$ MI.add a a';
(*  print_endline @$ MI.show @$ MI.mul a a';*)
(*  print_endline @$ MI.show @$ MI.mul a' a;*)
