open Printf
open List

let ( *@) f g = fun x -> f (g x)
let (@$) f x = f x
let toInt c =
  int_of_char c - int_of_char '0'

type position = int * int

type color =
  | Red
  | Green
  | Blue
  | Yellow

exception FoundNone
let wedge n i = if i < 3 * n then 1 + i else 0
let hill n i = if i < n then 0 else 4 * n - i
let width n i = max (wedge n i) (hill n i)
let positionToIndex n (x, y) = y, (x + width n y - 1) / 2
let indexToPosition n (i, j) = (- (width n i - 1) + 2 * j), i
module Dir = struct
  type t = int
  let show : t -> string = function
    | 0 -> "E"
    | 1 -> "SE"
    | 2 -> "SW"
    | 3 -> "W"
    | 4 -> "NW"
    | 5 -> "NE"
    | _ -> assert false
  
  let all = [0; 1; 2; 3; 4; 5]
  let opposite : t -> t = fun t -> (t + 3) mod 6
  let clock : t -> t = fun t -> (t + 1) mod 6
  let counterClock : t -> t = fun t -> (t + 5) mod 6
end

module Board = struct
  type 'a t = 'a option array array
  let isEmpty cell = cell == None
  let create n init =
    Array.init (4 * n) (fun i -> Array.init (width n i) (fun j -> init i j))
  let dim board =
    Array.length board / 4
  let inside (i, j) board =
    i >= 0 && i < Array.length board && j >= 0 && j < Array.length board.(i)
  let neighbours (i, j) board =
    let n = dim board in
    let (x, y) = indexToPosition n (i, j) in
    List.filter (fun (i, j) -> inside (i, j) board)
      (List.map (positionToIndex n) [x - 2, y; x + 2, y; x - 1, y + 1; x + 1, y + 1; x - 1, y - 1; x + 1, y - 1])
  let jump t stone board =
    let n = dim board in
    let (x, y) = indexToPosition n t in
    let (x', y') = indexToPosition n stone in
    positionToIndex n (2*x'-x,2*y'-y)
  let canStep t board =
    List.filter (fun (i,j) -> isEmpty board.(i).(j)) (neighbours t board)
  let canJump t board =
    let cantStep = List.filter (fun (i,j) -> not (isEmpty board.(i).(j))) (neighbours t board) in
      List.filter (fun (i,j) -> isEmpty board.(i).(j))
        (List.map (fun stone -> jump t stone board) cantStep)
end

let showColor c = match c with
  | Red -> "R"
  | Green -> "G"
  | Blue -> "B"
  | Yellow -> "Y"

type cell = color option

let showCell = function
  | None -> " "
  | Some s -> showColor s

type board = cell array array

type command = position * position

type state = {
  turns : color list;
  board : board
}

let show_list ?(brackets ="[","]") ?(sep =";") show l =
  fst brackets ^ (fun s -> (String.concat sep) (List.map show s)) l ^ snd brackets
let show_array ?(brackets ="[|","|]") ?(sep =";") show a =
  fst brackets ^ (fun s -> (String.concat sep) (List.map show s)) (Array.to_list a) ^ snd brackets
let show_matrix ?(brackets ="[|","|]") ?(sep =";") show ma =
  String.concat "\n" (Array.to_list (Array.map (fun a -> show_array ~brackets ~sep show a) ma))

module type STATE = sig
  type t
  type action
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> (t * action) list
  val final : t -> bool
end

module Search(S: STATE) = struct
  exception Found of S.action list
  exception Stop of S.t
  module T = Weak.Make(S)
  module M = Map.Make(S)
  let dfs s =
    let visited = T.create 3 in
    let rec work s acc =
      T.add visited s;
      if S.final s then raise (Found acc) else
        iter (fun (s', a) -> work s' (a:: acc)) (filter (not *@T.mem visited *@fst) @$ S.next s) in
    try work s []; None
    with Found acc -> Some acc
  let bfs s =
    let trace s h =
      let rec work s acc =
        try
          let (s', a) = Hashtbl.find h s in
          work (s') (a:: acc)
        with Not_found -> acc in
      work s [] in
    let h = Hashtbl.create 3 in
    let q = Queue.create () in
    Queue.push s q;
    try
      while true do
        let s = Queue.pop q in
        if S.final s then raise (Stop s) else
          iter (fun (s', a) -> Queue.add s' q; Hashtbl.replace h s' (s, a)) (S.next s)
      done;
      None
    with Stop s -> Some (trace s h)
    | Queue.Empty -> None
end

let main () =
  let board = Board.create 4 (fun _ _ -> None) in
  let target = ref (0, 0) in
  let module SearchReach = Search (struct
      type t = int * int
      type action = unit
      let equal = (=)
      let compare = Pervasives.compare
      let hash = Hashtbl.hash
      let next t =
        List.map (fun e -> e, ()) (Board.canStep t board @Board.canJump t board)
      let final = (=) (!target)
    end) in
  ()