open Printf

let splitChar s c =
      let n = String.length s in
      let rec work i acc =
        if i>=n then List.rev acc else
          try
              let i' = String.index_from s i c in
                work (i'+1) (String.sub s i (i'-i)::acc)
          with Not_found -> List.rev (String.sub s i (n-i)::acc)
      in
      work 0 []

let s = "c Example
c
p cnf 4 3
1 3 -4 0
4 0 2
-3"

let l= splitChar s '\n'

(*let parse sl =*)
  

(*let () =*)
(*  printf "%s\n" s*)
  