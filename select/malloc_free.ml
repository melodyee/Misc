open Linda
open StateMonad

type heap = {
  meminfo : (int * int) StringMap.M.t
  ;blocks : (int * int) list}

let meminfo v heap =
  StringMap.M.find v heap.meminfo

let rec malloc var n =
  get >>= fun heap ->
    let rec work = function
      | [] -> failwith "Not enough memory."
      | x::xs ->
        if snd x >= n then
          let xs' = (List.filter (fun x -> snd x > 0) [fst x + n, snd x - n]) @ xs in
          put {meminfo = StringMap.M.add var (fst x, n) heap.meminfo;blocks = xs'}
        else work xs in
    work heap.blocks

let rec free_block (addr, n) =
  get >>= fun heap ->
    let rec merge = function
      | [] -> []
      | [x] -> [x]
      | x::y::xs ->
        if fst x + snd x = fst y then merge ((fst x, snd x + snd y)::xs)
        else x:: merge (y::xs) in
    put {heap with blocks = merge @$ ExtList.sortByMap compare fst @$ (addr, n)::heap.blocks}

let free var =
  get >>= fun heap ->
    free_block @$ meminfo var heap >>= fun _ ->
      get >>= fun heap ->
        put {heap with meminfo = StringMap.M.remove var (heap.meminfo)}

let () =
  let ms = [
    malloc "a" 3
    ;malloc "b" 4
    ;free "a"
    ;malloc "c" 2
    ;free "b"
    ;malloc "d" 4
    ] in
  let ms' = List.map (fun m -> m >>= (fun _ -> gets id)) ms in
  let init = {meminfo = StringMap.M.empty; blocks = [0, 100]} in
  let (a, s) = runState (sequence ms') init in
  Printf.printf "%s\n" @$ String.concat "\n" @$ List.map Show.dump a;
  Show.print "final";
  Show.print s
  
(* [(0, "a", (0, 3), 0, 1); (3, 97)]                              *)
(* [(0, "a", (0, 3), (0, "b", (3, 4), 0, 1), 2); (7, 93)]         *)
(* [(0, "b", (3, 4), 0, 1); (0, 3); (7, 93)]                      *)
(* [(0, "b", (3, 4), (0, "c", (0, 2), 0, 1), 2); (2, 1); (7, 93)] *)
(* [(0, "c", (0, 2), 0, 1); (2, 98)]                              *)
(* [(0, "c", (0, 2), (0, "d", (2, 4), 0, 1), 2); (6, 94)]         *)
(* "final"                                                        *)
(* [(0, "c", (0, 2), (0, "d", (2, 4), 0, 1), 2); (6, 94)]         *)