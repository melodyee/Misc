open Printf
open Bigarray

module FloatMatrix = struct
  open Array
    exception WrongDimension
    type t = float array array
    let checkDim b = if not b then raise WrongDimension
    let dims ma = (length ma, length ma.(0))
    let checkSquare ma =
      let (m, n) = dims ma in checkDim (m = n)
    let fold_col f a ma j =
      let r = ref a in
      for i = 0 to snd (dims ma) - 1 do
        r:= f i !r ma.(i).(j)
      done;
      !r
    let init_matrix m n f =
      Array.init m (fun i -> Array.init n (fun j -> f i j))
    let swapCol ma j j' =
      let m, n = dims ma in
      for i = 0 to n - 1 do
        let t = ma.(i).(j) in
        ma.(i).(j) <- ma.(i).(j');
        ma.(i).(j') <- t
      done
    let zipWith op ma ma' =
      let m,n = dims ma in
      let m',n' = dims ma' in
      init_matrix (min m m') (min n n') (fun i j -> op ma.(i).(j) ma'.(i).(j))
    let add ma ma' = zipWith (zipWith (+.)) ma ma'
    let sub ma ma' = zipWith (zipWith (-.)) ma ma'
    let mul ma ma' =
      let dotProduct ma i ma' j =
        let s = ref 0. in
        for k = 0 to length ma.(i) - 1 do
          s := !s +. ma.(i).(k) *. ma'.(k).(j)
        done;
        !s in
      let (m, n) = dims ma in
      let (m', n') = dims ma' in
      checkDim (n = m');
      init_matrix m n' (fun i j -> dotProduct ma i ma' j)
    let mulv ma v =
      let m,_ = dims ma in
      Array.init m (fun i -> Array.fold_left (+.) 0. 
        (Array.mapi (fun j e -> ma.(i).(j) *. e) v))
    let eye n = init_matrix n n (fun i j -> if i = j then 1. else 0.)
    let diag a =
      let n = length a in
      init_matrix n n (fun i j -> if i = j then a.(i) else 0.)
    let row_vector a = [| a |]
    let col_vector a = init_matrix (length a) 1 (fun i j -> a.(i))
    
    (* horizontally join two matrices *)
    let jux ma ma' =
      let m, n = dims ma in
      let m', n' = dims ma' in
      checkDim (m = m');
      init_matrix m (n + n') (fun i j -> if j < n then ma.(i).(j) else ma'.(i).(j - n))
    let inverse ma =
      checkSquare ma;
      let n,_ = dims ma in
      let ma = jux ma (eye (Array.length ma)) in
      for i = 0 to n - 1 do
      (* print_endline @$ show_matrix R.show ma; *)
        let mi = ref i in
        let mv = ref (abs_float ma.(i).(i)) in
        for j = i + 1 to n - 1 do
          let v = abs_float ma.(j).(i) in
          if v > !mv then begin
            mv := v;
            mi := j
          end
        done;
        if !mi <> i then begin
          let t = ma.(!mi) in
          ma.(!mi) <- ma.(i);
          ma.(i) <- t
        end;
        let k = 1. /. ma.(i).(i) in
        for j = i to 2 * n - 1 do
          ma.(i).(j) <- ma.(i).(j) *. k
        done;
        for k = 0 to n - 1 do
          if k <> i then
            let kk = -. ma.(k).(i) in
            for j = i to 2 * n - 1 do
              ma.(k).(j) <- ma.(k).(j) +. (kk *. ma.(i).(j))
            done
        done;
      done;
      Array.init n (fun i -> Array.init n (fun j -> ma.(i).(j + n)))
    let recip = inverse
  end

(*let fun_of_file fn i =                                     *)
(*  let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in           *)
(*  let str = Array1.map_file fd int32 c_layout false (-1) in*)
(*  Unix.close fd;                                           *)
(*  Int32.to_int (Array1.get str i)                          *)
(*                                                           *)
(*                                                           *)
(*let sim stream =                                           *)
(*  let events = [] in                                       *)
(*  let content =                                            *)
(*  let rec work =                                           *)
     
  
(*let () =                                               *)
(*  let fn = "linpack.bin" in                            *)
(*  let f = fun_of_file fn in                            *)
(*  Array.iter (fun i -> printf "%d\n" (f i)) [|0;1;2;3|]*)

let collatz x =
  let rec work acc n =
    if n <=1 then List.rev acc else
      work (n::acc) (if n mod 2 =0 then (n/2) else n*3+1) in
  work [] x
  
module FM = FloatMatrix

let binary_search x arr =
  let n = Array.length arr in
  let rec work i i' =
    if i'-i<=1 then i else
      let i'' = i+(i'-i)/2 in
      if arr.(i'')=x then i'' else
        if arr.(i'')<x then work i'' i' else
          work i i'' in
  work 0 (n-1)

let spline xs ys =
  let n = Array.length xs in
  let hs = Array.init (n-1) (fun i -> xs.(i+1) -. xs.(i)) in
  Array.iter (fun e -> if e<0. then assert false) hs;
  let aux = Array.init (n-1) (fun i -> (ys.(i+1) -. ys.(i)) /. hs.(i)) in
  let ma = Array.init (n-2) (fun i -> 
    Array.init (n-2) (fun j -> 
      if i=j then 2.*.(hs.(i)+.hs.(i+1)) else 
        if i=j+1 then hs.(i) else
          if i=j-1 then hs.(i+1) else
             0.)
    ) in
  let b = Array.init (n-2) (fun i -> 
    6. *. (aux.(i+1) -. aux.(i))) in
  let ss = Array.init n (fun i -> if i=0 || i=n-1 then 0. else
    (FM.mulv (FM.inverse ma) b).(i-1)) in
  let arr = Array.init (n-1) (fun i -> 
    (ss.(i+1)-.ss.(i))/.6./.hs.(i),
    ss.(i)/.2.,
    aux.(i) -. hs.(i) *. (2.*.ss.(i)+.ss.(i+1)) /. 6.,
    ys.(i)
    ) in
  fun xin ->
    let i = binary_search xin xs in
    let a,b,c,d=arr.(i) in
    let x = xin -. xs.(i) in
(*    (a,b,c,d)*)
    a*.x**3.+.b*.x**2.+.c*.x+.d
    
let f = spline [|0.;1.;2.|] [|0.;1.;4.|];;
  
let median arr =
  Array.sort compare arr; 
  arr.(Array.length arr/2)
       
(*let alon_distinct n arr =                                 *)
(*  let right_bits e =                                      *)
(*    let rec work acc e =                                  *)
(*      if e land 1 != 0 then acc else work (acc+1) (e/2) in*)
(*    if e=0 then 0 else work 0 e in                        *)
(*  let aux () =                                            *)
(*    let a = Random.bits () in                             *)
(*    let b = Random.bits () in                             *)
(*    let max_array = Array.init n (fun _ -> 0) in          *)
(*    Array.iter (fun a_i ->                                *)
(*	    let r = right_bits (a*a_i+b) in                     *)
(*      let i = Random.int n in                             *)
(*      if r > max_array.(i) then max_array.(i) <- r) arr;  *)
(*    Array.fold_left max 0 max_array in                    *)
(*  1 lsl (aux ())                                          *)

let test m n =
  let gen () =
    (if Random.int 2=1 then (-1.) else 1.) *. (Random.float 1.0) in
  let max_subsum arr =
    let max = ref 0. in
    let psum = ref 0. in
    for i =0 to Array.length arr -1 do
      if !psum +. arr.(i)<0. then psum := 0. else begin
        psum := !psum +. arr.(i);
        if !psum > !max then max := !psum
       end
    done;
    !max in
  Array.fold_left (+.) 0. 
    (Array.init m (fun _ -> max_subsum (Array.init n (fun _ -> gen ())))) /. float m
    
(*let s = "                               *)
(*rA0=*pA0; rB0=*pB0; rA1=*pA1; rB1=*pB1; *)
(*rA2=*pA2; rB2=*pB2; rA3=*pA3; rB3=*pB3; *)
(*for (k=i+1;k<M;k+=2) {                  *)
(*}                                       *)
(*"                                       *)
(*let p = "ra%i = pA%i[incA]; rC%i%i"     *)