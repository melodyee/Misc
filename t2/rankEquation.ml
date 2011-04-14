open List
open Printf
open Linda
open ExtList
open ListMonad

type equation= 
  | Eq of string*string*int
  | Val of string*int

exception Inconsistent

(*post: s<>s' *)
let simp e = match e with
  | Val _ -> return e
  | Eq(s,s',i) ->
    if s<>s' then return e else
      if i=0 then mzero else raise Inconsistent 

(*pre: e is processed by simp *)
(*post: occurrences of s is elim'ed *)
let applyVal s i e = match e with
  | Val(s',i') ->
    if (s'<>s || i=i') then return e else
      raise Inconsistent 
  | Eq(s',s2',i') -> 
    if s'=s then return @$Val(s2',i-i') else
      if s2'=s then return @$Val(s',i'+i) else
        return e
(*pre: e is processed by simp, s<>s2 *)
(*post: occurrences of s is elim'ed *)        
let applyEq s s2 i e = match e with
  | Val(s',i') -> 
    if s=s' then  [Val(s2,i'-i)] else  [e]
  | Eq(s',s2',i') -> 
    if s=s' then simp (Eq(s2,s2',i'-i)) else
    if s=s2' then simp (Eq(s',s2,i+i')) else
       [e] 

let solve l =
  try
    let rec loop acc = function 
      | [] -> acc
      | x::xs as l' -> begin match pick (function Val _ as x -> Some x | _ -> None) l' with
          | None -> begin match x with
            | Eq(s,s2,i) -> loop acc (join @$ map (applyEq s s2 i) xs)
            | _ -> failwith "impossible"
            end
          | Some (Val(s,i),rest) -> loop ((s,i)::acc) (join @$ map (applyVal s i) rest)
          | _ -> failwith "impossible"
          end in 
     Some (loop [] @$ nub @$ join @$ map simp l)
  with Inconsistent -> None
 
let test () =
  match solve [Eq("a","b",1);Val("b",2);Val("d",1)] with
    | None -> print_endline "Inconsistent" 
    | Some l -> print_list (fun (a,b) -> a^":"^string_of_int b) l