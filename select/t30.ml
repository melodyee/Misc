open Printf
open Linda

let a2 = 
  "
    **
   *  *
   *  *
     *
    *****
  "

let a1 =
  "
      *
      *
      *
      *
  " 



open ExtArray    
let toRectange zero t =
  let n = maximum (map length t) in
  map (fun a -> init n (fun i -> if i < length a then a.(i) else zero)) t
let string_of_char = String.make 1  

module Matrix = struct
  open ExtArray
  let map f t = map (map f) t
  let mapij f t =
    mapi (fun i -> mapi (fun j e -> f i j e)) t
  let dims t = 
    let m = length t in
    m,if m=0 then 0 else length t.(0)
  let fold f r t =
    foldr (fun a r -> foldr (fun a r -> f a r) r a) r t
  let zipWith f ma ma' =
    zipWith (zipWith f) ma ma'
  end 

(*let hough ma =                 *)
(*    let m,n = Matrix.dims ma in*)
(*module P = Point(Int)      *)
let sqr x = x * x
let circle f ma =
  let m,n = Matrix.dims ma in
  init (max m n) @$ fun r ->
    init_matrix m n @$ fun x y ->
         Matrix.fold (+) 0 (Matrix.mapij (fun i j e -> 
          if sqr (i-x) + sqr (j-y) = sqr r then f i j e else 0) ma)

let dot a a' =
  Algebra.IntArray.sum @$ zipWith ( * ) a a'

let similarity a a' =
  float (dot a a') /. float (dot a a) /. float (dot a' a') 

let circleSimilarity f ma ma' =
  Matrix.fold (+.) 0. @$ Matrix.zipWith similarity (circle f ma) (circle f ma') 

(*let () =                                                                                       *)
(*  let ma = toRectange ' ' (map (of_list*@ExtString.to_list) @$ of_list @$ ExtString.lines a) in*)
(*  printf "%s\n" @$ show_matrix string_of_char @$ ma;                                           *)
(*  iter (printf "%s\n" *@ show_matrix string_of_int) @$                                         *)
(*    circle (fun i j e -> int_of_bool @$ (e<>' ')) ma                                           *)
        