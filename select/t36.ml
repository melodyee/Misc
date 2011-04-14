open Linda
open Printf

(*open BitSet                                    *)
(*                                               *)
(*let a = of_bool_array [|true;false;true;true|] *)
(*                                               *)
(*let a' = of_bool_array [|false;true;true;true|]*)
(*                                               *)
(*let () =                                       *)
(*  Random.self_init ();                         *)
(*  print_endline @$ show @$ crossover [|a;a'|]  *)
(*                                               *)
  
(*let () =                      *)
(*  let s = ExtUnix.exec "ls" in*)
(*  ()*)
(*  print_endline @$ string_of_bool @$ List.mem "testlib.b" @$ ExtString.lines s*)

(*let () =                                            *)
(*(*  Scanf.scanf "%d\n%d" (fun i j -> print_int j);*)*)
(*(*  Scanf.scanf "%d" print_int*)                    *)
(*    let s = input_line stdin in                     *)
(*    print_int @$ int_of_string @$ ExtString.trim s; *)
(*    let s = input_line stdin in                     *)
(*    print_int @$ int_of_string @$ ExtString.trim s  *)
open Tree    
open ExtList
let rec dump = function
  | Leaf -> "."
  | Branch(e,l) -> sprintf "%d %s" e (show_list ~brackets:("","") ~sep:" " id (map dump l))    

let () =
  print_endline @$ dump (Branch(1,[Leaf;Branch(3,[])]))