open Printf

let toInt c =
  int_of_char c - int_of_char '0'

type position = int * int

type shape =
  | A
  | B
  | C
  | D

type color =
  | Red
  | Green
  | Blue
  | Yellow

type symbol =
  | Symbol of color * shape
  | Kill
  | MatchAll

exception FoundNone

module Board = struct
  type 'a t = 'a option array array
  let create width height init =
    Array.init height (fun i -> Array.init width (fun j -> init i j))
  let dims board =
    Array.length board.(0), Array.length board
  let row i board =
    let w, h = dims board in
    Array.to_list (Array.init w (fun j -> (i, j)))
  let rows board =
    Array.to_list (Array.mapi (fun i _ -> row i board) board)
  let column j board =
    Array.to_list (Array.mapi (fun i _ -> (i, j)) board)
  let columns board =
    Array.to_list (Array.init (Array.length board.(0)) (fun j -> column j board))
  let neighbours (i, j) board =
    let w, h = dims board in
    (if i > 0 then [i - 1, j] else []) @
    (if j > 0 then [i, j - 1] else []) @
    (if i < w - 1 then [i + 1, j] else []) @
    (if j < h - 1 then [i, j + 1] else [])
  let clear clearCell line board =
    List.iter (fun (i, j) -> clearCell board.(i).(j)) line
  let isFull isCellEmpty line board =
    try List.iter (fun (i, j) -> if isCellEmpty board.(i).(j) then raise FoundNone) line; true
    with FoundNone -> false
  let toClear isCellEmpty board =
    List.concat (List.filter (fun l -> isFull isCellEmpty l board) (rows board @ columns board))
end

let testString =
  "RARBRCGC
RARBYCGC
RA RCGC
RARBRC "

let lines s =
  let rec work i acc =
    try
      let i' = String.index_from s i '\n' in
      work (i'+ 1) (String.sub s i (i'- i):: acc)
    with Not_found -> List.rev (String.sub s i (String.length s - i):: acc) in
  work 0 []

let showColor c = match c with
  | Red -> "R"
  | Green -> "G"
  | Blue -> "B"
  | Yellow -> "Y"

let showShape s = match s with
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"

let parseLine s =
  let parseColor c = match c with
    | 'R' -> Red
    | 'G' -> Green
    | 'B' -> Blue
    | 'Y' -> Yellow
    | _ -> assert false in
  let parseShape s = match s with
    | 'A' -> A
    | 'B' -> B
    | 'C' -> C
    | 'D' -> D
    | _ -> assert false in
  let n = String.length s in
  assert(n mod 2 = 0);
  Array.init (n / 2) (fun i ->
          if s.[2 * i]=' ' && s.[2 * i + 1]=' ' then None else
            Some (Symbol (parseColor s.[2 * i], parseShape s.[2 * i + 1])))

let parse s =
  Array.of_list (List.map parseLine (lines s))

(* module TurnConsoleGame = struct let create = end *)

type cell = symbol option

let showSymbol = function 
  | Symbol (c, s) -> showColor c^showShape s
  | MatchAll -> "**"
  | Kill -> "XX"
  

let showCell = function
  | None -> " "
  | Some s -> showSymbol s

type board = cell array array

type command =
  | Discard
  | Place of position

type state = {
  leeway : int;
  board : board
}

let matchSymbol s s' = match s,s' with
  | Symbol (c,s), Symbol (c',s') -> c = c' || s = s'
  | Symbol _ , MatchAll -> true
  | MatchAll, Symbol _ -> true
  | _ -> assert false

let matchCell cell cell' = match cell, cell' with
  | Some s, Some s' -> matchSymbol s s' 
  | None, Some _
  | Some _, None
  | None, None -> true

let isEmpty cell = cell == None
let show_list ?(brackets ="[","]") ?(sep =";") show l =
  fst brackets ^ (fun s -> (String.concat sep) (List.map show s)) l ^ snd brackets
let show_array ?(brackets ="[|","|]") ?(sep =";") show a =
  fst brackets ^ (fun s -> (String.concat sep) (List.map show s)) (Array.to_list a) ^ snd brackets
let show_matrix ?(brackets ="[|","|]") ?(sep =";") show ma =
  String.concat "\n" (Array.to_list (Array.map (fun a -> show_array ~brackets ~sep show a) ma))

let run leewayCap state commands symbols =
  let runOne command state s = match command, state.leeway with
    | Discard, 0 -> print_endline "No more leeway."; None
    | Discard, n -> Some { state with leeway = n-1 }
    | Place (i, j), n -> begin
       if s = Kill then
        if isEmpty state.board.(i).(j) then begin
          print_endline "Empty cell."; None
        end else begin
          state.board.(i).(j) <- None;
          Some { leeway = leewayCap (n+1); board = state.board }
        end    
       else
        if not (isEmpty state.board.(i).(j)) then begin
          print_endline "Non-empty cell."; None
        end else
          if [] == List.filter (fun (i, j) -> not (matchCell (Some s) state.board.(i).(j)))
            (Board.neighbours (i, j) state.board) then begin
            state.board.(i).(j) <- Some s;
            Some { leeway = leewayCap (n+1); board = state.board }
          end else begin
            print_endline "Non-matching symbol"; None
          end
      end in
  let rec work state =
    let Some symbol = Stream.peek symbols in
    print_endline (show_matrix showCell state.board);
    print_endline (show_array (fun x -> x) (Array.make state.leeway "_"));
    print_endline (showSymbol symbol);
    try begin match runOne (Stream.next commands) state symbol with
        | Some state' -> Stream.junk symbols;work state'
        | None -> work state
      end with Stream.Failure -> ()
  in
  work state

let symbols =
  let pickOne arr = arr.(Random.int (Array.length arr)) in
  Stream.from (fun _ ->
          let c = pickOne [| Red; Green; Blue; Yellow |] in
          let s = pickOne [| A; B; C; D |] in
          Some (Symbol (c, s))
    )

let commands = Stream.from (fun _ ->
          let s = input_line stdin in
          if s.[0] = 'd' then Some Discard else
          if s.[0] = 'p' then Some (Place (toInt s.[2], toInt s.[3])) else None
    )

let main () =
  run (min 3) {leeway = 3; board = Board.create 9 8 (fun _ _ -> None)} commands symbols