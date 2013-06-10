open Linda

module DistributionMonad = struct
  type 'a t = ('a * float) list
  let one_of (l:'a t) =
    let p = Random.float 1. in
    let rec work acc = function
      | [] -> failwith "one_of"
      | (e, p') ::xs -> if acc +. p' > p then e else work (acc +. p') xs in
    work 0. l
  let normalize (l:'a t) : 'a t =
    let work l = 
      let total = List.fold_left (+.) 0. @$ List.map snd l in
      List.map (fun (e, p) -> e, p /. total) l in
    work @$
      List.map (fun l -> fst (List.hd l), List.fold_left (+.) 0. (List.map snd l)) @$
        ExtList.groupBy (fun a a' -> fst a = fst a') @$
          List.sort (fun a a' -> compare (fst a) (fst a')) l
  let expectation l = List.fold_left (+.) 0. @$ List.map (fun (e, p) -> float_of_int e *. p) l
  let joint d f =
    List.flatten (List.map (fun (e, p) -> List.map (fun (e', p') -> ((e, e'), p *. p')) (f e)) d)
  let fmap f d = List.map (fun (e, p) -> f e, p) d
  let bind d f = normalize @$ fmap snd (joint d f)
  let return x : 'a t = [x, 1.0]
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= (fun x -> x)
  let liftM f m = m >>= fun x -> return (f x)
  let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return (f x x')
  let liftM3 f m m' m'' =
    m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return (f x x' x'')
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return (f x)
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])
  let bayes d f =
    let joint_d = joint d f in
    let d' = normalize @$ fmap snd joint_d in
    fun e'' ->
      List.map (fun ((e, _), p) -> (e, p /. (List.assoc e'' d'))) @$
        List.filter (fun ((e, e'), _) -> e' = e'') joint_d
   let replicateM n m =
    let replicate n e =
      let rec loop stk i = if i = 0 then stk else loop (e:: stk) (i - 1) in
      loop [] n in
    sequence (replicate n m)
  let uniform l : 'a t = let p = 1. /. float (List.length l) in
    List.map (fun e -> e, p) l
  let bernoulli p = [1, p; 0, 1. -. p]
  (* let binomial2 n p = normalize @$ fmap (List.fold_left (+) 0) @$ replicateM n (bernoulli p) *)
  let binomial n p =
    let ratio = p /. (1. -. p) in
    let work (l, x) k =
      let x' = x *. ratio /. (float k) *. (float (n - k + 1))  in
      ((k - 1, x)::l, x') in
    List.rev @$ fst @$ List.fold_left work ([], (1. -. p) ** (float n)) (ExtList.range 1 (n + 2))
    
  (*   funPowers n (fun x -> x *. ratio *. (n ) @$ (1. -. p) ** n  *)
  let categorical ps : int t = ExtList.mapi (fun i p -> i + 1, p) ps
end
let show l = show_list (fun (e, p) -> Printf.sprintf "(%d,%f)" e p) l
open DistributionMonad
let () =
  ignore (Printf.printf "%s" @$ show @$
    (uniform [1; 2; 3] >>= fun e -> uniform [e + 1; e + 2; e + 3]))
    (* return (Printf.printf "%d\n" x)) *)

let () =
  Printf.printf "\n";
  ignore (sequence [uniform [1;2;3];uniform [4;5;6];uniform [7;8];] >>= fun x ->
    return @$ Printf.printf "%s\n" @$ show_list string_of_int x)

let () =
  Printf.printf "\n";
  Printf.printf "%s\n" @$ show_list string_of_int @$ one_of @$
    (sequence [uniform [1;2;3];uniform [4;5;6];uniform [7;8];])

let () =
  Printf.printf "%s\n" @$ show @$
    join @$ uniform [uniform [1;2;3];uniform[4;5;6]]

let () =
  let f = function
    | "Rare" -> ["Pattern", 0.98;"NoPattern", 0.02]
    | "Common" -> ["Pattern", 0.05;"NoPattern", 0.95] in
  let show l = show_list (fun (e, p) -> Printf.sprintf "(%s,%f)" e p) l in
  Printf.printf "%s\n" @$ show @$ bayes ["Rare", 0.001; "Common", 0.999] f "Pattern"

let () =
  let f = function
    | "User" -> ["+", 0.99;"-", 0.01]
    | "Non-user" -> ["+", 0.01;"-", 0.99] in
  let show l = show_list (fun (e, p) -> Printf.sprintf "(%s,%f)" e p) l in
  Printf.printf "%s\n" @$ show @$ bayes ["User", 0.005; "Non-user", 0.995] f "+"
