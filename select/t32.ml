open Linda
open Printf



open LazyList
open LazyListMonad
let combinations l =
  let rec work s =
    if null s then return mzero else
      let x,xs = headTail s in
      let s' = work xs in
	    mplus (map (cons x) s') s' in
  work (of_list l)


  
let () =
(*  print_endline @$ show_intlist @$ take 10 @$ drop 10 @$ map Algebra.Int.sqr (enum 0 2);*)
    let l = ExtList.range 0 1000 in
(*    print_endline @$ show_list show_intlist @$ ExtList.take 10 @$ ExtList.combinations l *)
    print_endline @$ show_list (IntList.show*@to_list) @$ take 10 @$ combinations l
(*  try                                                      *)
(*    printf "%s\n" @$ ExtUnix.exec "du -a / >/dev/null 2>&1"*)
(*  with _ -> printf "killed\n"                              *)