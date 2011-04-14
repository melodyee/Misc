open Linda
open Printf
open Algebra

module Primes(R:R) = struct  
  module R = Ring(R)
  open R
  
(*  let primesBetween m n =*)
(*    let rec work         *)

  end
  
(*let () =                                                           *)
(*    let seed = [2; 3; 5; 7; 11; 13; 17] in                         *)
(*      let m = product seed in                                      *)
(*      let trim p l = List.filter (fun x -> x mod p <> 0) l in      *)
(*      let ws = ExtList.foldr trim (ExtList.range 1 (m + 1)) seed in*)
(*    IntList.print @$ ws                                            *)

    let isPrime n =
      let rec work i i2 =
        let i2' = i2+4*i+4 in
        let i' = i+1 in
        i2'>n || n mod i <>0 && work i' i2' in
      n>=2 && n mod 2<>0 && (n=2 || work 3 9)

            
let primesBetween m n =
  let c = ref 0 in
  for i = m to n do
(*    if isPrime i then printf "%d\n" i*)
    if isPrime i then incr c
  done;
  printf "%d\n" !c

let arg i = int_of_string Sys.argv.(i)    
let () =
  primesBetween (arg 1) (arg 2)      