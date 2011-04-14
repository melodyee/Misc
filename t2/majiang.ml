open List
open Printf

let testString = "Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Zhong Zhong,true
Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Wan1 Wan2 Wan3 Zhong Fa,false
Wan1 Wan1 Wan1 Wan1 Wan2 Wan3 Wan2 Wan3 Wan4 Wan9 Wan9 Wan9 Bai Bai,true
Wan1 Wan2 Wan3 Suo4 Suo4 Suo4 Bing7 Bing8 Bing9 Dong Dong Dong Zhong Zhong,true
Wan1 Wan1 Dong Dong Nan Nan Xi Xi Bei Bei Zhong Zhong Fa Fa,true"

let lines s =
  let rec work i acc =
    try
      let i' = String.index_from s i '\n' in
      work (i'+ 1) (String.sub s i (i'- i):: acc)
    with Not_found -> List.rev (String.sub s i (String.length s - i):: acc) in
  work 0 []

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

type dir =
  | Dong
  | Xi
  | Nan
  | Bei
type arrow =
  | Zhong
  | Fa
  | Bai
type pai =
  | Wan of int
  | Bing of int
  | Suo of int
  | Feng of dir
  | Jian of arrow
let toInt c =
  int_of_char c - int_of_char '0'
let parsePai = function
  | "Zhong" -> Jian Zhong
  | "Fa" -> Jian Fa
  | "Bai" -> Jian Bai
  | "Dong" -> Feng Dong
  | "Xi" -> Feng Xi
  | "Nan" -> Feng Nan
  | "Bei" -> Feng Bei
  | s ->
      if s.[0]='W' then Wan (toInt s.[3]) else
      if s.[0]='S' then Wan (toInt s.[3]) else
      if s.[0]='B' then Wan (toInt s.[4]) else
        failwith "parsePai"
let parse s = List.map (fun ln ->
          match splitChar ln ',' with
          | [s; r] ->
              let res = r ="true" in
              let l = splitChar s ' ' in
              List.map parsePai l, res
          | _ -> failwith "parse"
    ) (lines s)

let pickOne f l =
  let rec work acc = function
    | x:: l -> if f x then Some (x, acc@l) else work (x:: acc) l
    | [] -> None in
  work [] l

let delOne x l = pickOne ((=) x) l

let findPair l =
  let rec work pre post acc = match post with
    | [] -> acc
    | x:: l ->
        work (x:: pre) l (
            match delOne x l with
            | None -> acc
            | Some (x', l') -> (x', List.sort compare (pre@l')):: acc) in
  work [] l []

let next = function
  | Wan i when i < 9 -> Some (Wan (i + 1))
  | Suo i when i < 9 -> Some (Suo (i + 1))
  | Bing i when i < 9 -> Some (Bing (i + 1))
  | _ -> None

let bind m k = match m with
    None -> None
  | Some x -> k x
let return x = Some x
let (>>=) = bind

let rec checkThree = function
  | x:: ((y:: z:: l') as l) ->
      (x = y && y = z && checkThree l') ||
      (match
        next x >>= fun y' ->
            delOne y' l >>= fun (_, l'') ->
                next y' >>= fun z' ->
                    delOne z' l'' >>= fun (_, l''') ->
                        return l'''
        with
        | Some l -> checkThree l
        | None -> false)
  | [] -> true
  | _ -> false

let rec isSevenPair = function
  | x:: y:: l -> x = y && isSevenPair l
  | [] -> true
  | _ -> false

let check l =
  isSevenPair l ||
  List.exists (fun x -> x) (List.map (fun x -> checkThree (snd x)) (findPair l))

let () =
  List.iter (fun (l, r) ->
          print_endline (if r <> check l then "bad" else "good")) (parse testString)