open Linda
open Linda.Algebra
open ExtList

(*let () = print_int 3*)
let show_array show a = show_list show (Array.to_list a)
module Matrix (R:R) = struct
  open Array
  open ExtArray
  module R = Ring(R)
  exception WrongDimension
  type matrix = (int * R.t array)
  type t = matrix
  let dims (k,a) = k,length a/k
  let of_array k a = k,a
  let make i a : t = (i,a)

  let get ((k,a):t) i j = a.(i*(length a/k)+j)
  let set ((k,a):t) i j v = a.(i*(length a/k)+j) <- v
  let setRow ma i vs =
    iteri (fun j v -> set ma i j v) vs
  let setCol ma j vs =
    iteri (fun i v -> set ma i j v) vs
  let getRow ma i =
    init (snd @$ dims ma) (fun j -> get ma i j)
  let getCol ma j =
    init (fst @$ dims ma) (fun i -> get ma i j)
  let init m n f = make m (init (m*n) (fun i -> f (i/n) (i mod n)))
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
    make (fst ma) @$ zipWith f (snd ma) (snd ma')
  let add = zipWith R.add
  let sub = zipWith R.sub
  let pointMul = zipWith R.mul
  let of_int i = (1,[|i|])
(*  let findInCol f ma j =                *)
(*    mplus map f @$ to_list (getCol ma j)*)
(*    let m,n = dims ma in                *)
(*    for i =0 to n-1 do                  *)
       
(*  let foldRowCol f ma i ma' j =*)
(*    for k = 0 to               *)
(*  open Subarray                                                                         *)
(*  let mul ma ma' =                                                                      *)
(*    let dot fwd a n a' n' =                                                             *)
(*      let work sa sa' = foldl R.add R.zero @$ zipWith R.mul sa sa' in                   *)
(*      let sa = of_array a in                                                            *)
(*      let sa' = of_array a' in                                                          *)
(*      if fwd then Array.init (n/n') (fun i -> work (drop (i*n') sa) sa')                *)
(*      else Array.init (n'/n) (fun i -> work (drop (i*n) sa') sa) in                     *)
(*    let m,n = dims ma in                                                                *)
(*    let m',n' = dims ma' in                                                             *)
(*    if n mod m' =0 then                                                                 *)
(*      let n'' = n/m'*n' in                                                              *)
(*(*      Array.init (m*n'') (fun i -> (dot true (getRow ma i) n (getCol ma' j) m').(i))*)*)
(*      init m n'' (fun i j -> (dot true (getRow ma i) n (getCol ma' j) m').(0))          *)
(*    else raise WrongDimension                                                           *)

  let mul ma ma' =
    let m,n = dims ma in
    let m',n' = dims ma' in
    if n<>m' then raise WrongDimension else
      init m n' (fun i j ->
        let s = ref R.zero in
        for k = 0 to n-1 do
          s:= R.add !s @$ R.mul (get ma i k) (get ma' k j)
        done;
        !s)
        
(*    type submatrix = (int * int) *    *)
    module Submatrix = struct
      type t = (int*int)* matrix
      let of_matrix i j ma = (i,j),ma
      let dims = fst
(*      let getRow  *)
    end
    
  let schmidt x a a' =
    let lam = R.neg @$ R.div a.(x) a'.(x) in
    ExtArray.zipWith R.add a (map (R.mul lam) a')
(*  let select i j ma = *)
(*                                                                                        *)
  let show ma = show_array R.show (snd ma)
  let gaussElim ma =
    let m,n = dims ma in
    if m=0 || n=0 then raise WrongDimension else
      let work x y =
        let maxv = ref (get ma x y) in
        let maxpos = ref x in
        for i = x+1 to m-1 do
          if R.gt (get ma i y) !maxv then begin
            maxv := get ma i y;
            maxpos := i
          end
        done;
(*        print_endline @$ "be:"^show ma;*)
        swapRow ma !maxpos x;
(*        print_endline @$ "af:"^show ma;*)
        for i = 0 to m-1 do
          if i<>x then
            setRow ma i (schmidt y (getRow ma i) (getRow ma x))
        done in
    for i = 0 to m-1 do
      work i i
    done 
  let eye n = init n n (fun i j -> if i=j then R.one else R.zero)
  let concat ma ma' =
    let m,n = dims ma in
    let m',n' = dims ma' in
    init m (n+n') (fun i j -> if j >=n then get ma' i (j-n) else get ma i j)
  let inv ma = 
    let m,n = dims ma in
    let ma' = concat ma (eye m) in
    let m',n' = dims ma' in
    gaussElim ma';
    for i = 0 to m'-1 do
      let lam = (get ma' i i) in
      for j = i to n'-1 do
        set ma' i j (R.div (get ma' i j) lam)
      done
    done;
    init m m (fun i j -> get ma' i (j+m)) 

(*  let row ((k,a):t) i : R.t Subarray.t = Subarray.make (i*(length a/k)) a*)
end

module MI = Matrix(Int)
module MF = Matrix(Float)
let () = 
  let a = MI.make 1 [|1;2;3|] in
  let a' = MI.make 3 [|1;2;3|] in
  let b = MI.make 3 [|-1;0;0;0;1;0;0;0;-1|] in
  let c = MF.make 3 [|1.;2.;3.;
                      4.;5.;6.;
                      7.;8.;10.;|] in                      
  print_endline @$ MI.show @$ a';
  print_endline @$ MI.show @$ MI.add a a';
  print_endline @$ MI.show @$ MI.mul a a';
  print_endline @$ MI.show @$ MI.mul a b;
  print_endline @$ MF.show @$ c;
  print_endline @$ MF.show @$ MF.inv c;
(*  MF.gaussElim c;               *)
(*  print_endline @$ MF.show @$ c *)
(*  print_endline @$ MI.show @$ MI.mul a' a;*)

(*module B = Linda.Algebra.BigInt                       *)
(*let () = print_endline @$ B.show @$ (B.factorial 1000)*)