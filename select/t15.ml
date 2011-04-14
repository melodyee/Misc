open Linda
open ExtArray
open Printf
let fan a =
	iteri (fun i v -> iteri (fun j v' -> if i!=j then printf "%d,%d\n" i j) a) a 	
	
let () =
	fan (init 3 id)	

open ExtList
open ListMonad		

module type LEXER = sig
  type token
end
module Function = struct
  let lift2 g f f' = fun x ->
    let i = f x in 
    g i (f' x) 
end
module AsmLexer = struct
  type token = string
  open ExtString
  let tokenize s =
    lines s |>
	    List.filter (not *@ (fun s -> 
	      isBlankString s || 
	      startsWith s "#" ||
	      startsWith s ".loc"
	    ) *@ strip )
      |> List.map (List.hd *@ split ~needle:"#") 
end

module type AST = sig
  type 'a t
  val empty : 'a t
  val make : 'a -> 'a t list -> 'a t
end

module Ast = struct
  type 'a t = 'a Tree.t
  let empty = Tree.empty
  let make e l = Tree.make e l
  let show sh = Tree.show sh
end

module Parse(L:LEXER)(A:AST) = struct		
	open MaybeMonad
	type t = L.token list -> (L.token A.t * L.token list) option
	let (<|>) p p' s =
    match p s with
      | None -> p' s
      | x -> x  
(*  let bind m k =                     *)
(*    fun s ->                         *)
(*      m s >>= fun (a', s') ->        *)
(*        k a' s' >>= fun (a'', s'') ->*)
(*          return (a'', s'')          *)
(*  let (>>=) = bind                   *)
(*  let return x = fun s -> Some (x, s)*)
  let mplus = (<|>)
	let mzero x = Some (A.empty,x)
  let zeroOrOne p = p <|> mzero
	let rec zeroOrMore p = (oneOrMore p) <|> mzero
	and oneOrMore p s = p s >>= fun (_,s') -> zeroOrMore p s'
  let pSat f = function
    | [] -> None
    | x::xs -> if f x then Some (A.make x [],xs) else None
  let pLit l s = pSat ((=) l) s
  let pAny l s = pSat (flip List.mem l) s 
  let rec sequence ms s = match ms with
    | [] -> Some([],s)
    | m::ms' ->
        m s >>= fun (a,s') ->
          sequence ms' s' >>= fun (as',s'') ->
            Some (a::as',s'')
end 	
module Mips = struct
  type reg = int
  type freg = int
  type immed = int
  type addr = immed * reg
  type cmpOp =
    [ `EQ | `NE | `LE | `LT | `GE | `GT]
  let pCmpOp = function
    | "eq" -> `EQ
    | "ne" -> `NE
    | "le" -> `LE
    | "lt" -> `LT
    | "ge" -> `GE
    | "gt" -> `GT
    | _ -> failwith "pCmpOp"
  let showCmpOp = function
    | `EQ -> "eq"
    | `NE -> "ne"
    | `LE -> "le"
    | `LT -> "lt"
    | `GE -> "ge"
    | `GT -> "gt"
  type ternop =
    [ `And | `Or | `Xor | `Nor
    | `Slt
    | `Add | `Sub
    | `Mulg | `Divg | `Modg
    | `Sll | `Srl | `Sra
    | `B of cmpOp | `Bz of cmpOp
    ]
  let pTernOp = function
    | "and" -> `And
    | "or" -> `Or
    | "xor" -> `Xor
    | "nor" -> `Nor
    | "slt" -> `Slt
    | "add" -> `Add
    | "sub" -> `Sub
    | "mult" -> `Mulg
    | "div" -> `Divg
    | "mod" -> `Modg
    | "sll" -> `Sll
    | "sra" -> `Sra
    | "srl" -> `Srl
    | _ -> failwith "pTernOp"
  let showTernOp = function
    | `And -> "and"
    | `Or -> "or"
    | `Xor -> "xor"
    | `Nor -> "nor"
    | `Slt -> "slt"
    | `Add -> "add"
    | `Sub -> "sub"
    | `Mulg -> "mult"
    | `Divg -> "div"
    | `Modg -> "mod"
    | `Sll -> "sll"
    | `Sra -> "sra"
    | `Srl -> "srl"
    | `B c -> "b" ^ showCmpOp c
    | `Bz c -> "b" ^ showCmpOp c ^ "z"
  type binop =
    [ `Mul | `Div
    | `Lui
    | `Mfc0 | `Mtc0
    ]
  let pBinop = function
    | "mult" -> `Mul
    | "div" -> `Div
    | "mfc0" -> `Mfc0
    | "mtc0" -> `Mtc0
    | _ -> failwith "pBinop"
  let showBinop = function
    | `Mul -> "mult"
    | `Div -> "div"
    | `Mfc0 -> "mfc0"
    | `Mtc0 -> "mtc0"
  type uop =
    [ `Sqrt
    | `J
    | `Mflo |`Mfhi | `Mtlo | `Mthi
    | `Bc1t | `Bc1f
    ]
  let pUop = function
    | "sqrt" -> `Sqrt
    | "j" -> `J
    | "mflo" -> `Mflo
    | "mfhi" -> `Mfhi
    | "mtlo" -> `Mtlo
    | "mthi" -> `Mthi
    | "bc1t" -> `Bc1t
    | "bc1f" -> `Bc1f
    | _ -> failwith "pUop"
  type modifier =
    [ `FD | `FS | `PS
    | `B | `H | `W | `D
    | `U
    | `L | `R
    | `AL
    | `G
    | `V | `S32
    ]
  let pMod = function
    | 'b' -> [`B]
    | 'h' -> [`H]
    | 'w' -> [`W]
    | 'd' -> [`D]
    | 'l' -> [`L]
    | 'r' -> [`R]
    | 'g' -> [`G]
    | _ -> []
  let showMod = function
    | `B -> "b"
    | `H -> "h"
    | `W -> "w"
    | `D -> "d"
    | `L -> "l"
    | `R -> "r"
    | `G -> "g"
    | `AL -> "al"
    | `FD -> ".d"
    | `FS -> ".s"
  type instr =
    [ `Load of (reg * addr)
    | `Store of (reg * addr)
    | `TOp of (ternop * reg * reg * reg)
    | `TOpI of (ternop * reg * reg * immed)
    | `BOp of (binop * reg * reg)
    | `BOpI of (binop * reg * immed)
    | `UOp of (uop * reg)
    | `UOpI of (uop * immed)
    | `Attr of modifier list * instr
    ]
  
end

(*  let parse_instr l =                                                     *)
(*(*    let startsWith l s = ExtList.startsWith l (ExtString.to_list s) in*)*)
(*    match l with                                                          *)
(*      | 'l'::xs -> begin                                                  *)
(*                                                                          *)
(*        end                                                               *)
(*  and parse_modifier l = match l with                                     *)
(*    | 'b'                                                                 *)

(*open Parse*)
(*let () =                                               *)
(*  of_string "aaabb" []                                 *)
(*  iter (printf "%s\n" *@ show_stringlist *@ result) @$ *)
        
(*      pairWith cons x @$ *)
(*  foldr (fun s l -> pairWith cons l s) (hd ll) (tl ll)*)

module P=Parse(AsmLexer)(Ast)
open P
open ExtString
open Mips
let () = 
  let tokens = AsmLexer.tokenize "a\nb\n" in
  let token s = startsWith ~needle:s *@ lstrip in
  let pDir = pSat (token ".") in
(*  let pLoc = pSat (token ".loc") in*)
  let pFuncStart = pSat (endsWith ~needle:":"*@rstrip) in
  let pLocLabel = pSat ((fun s -> startsWith s "$" && endsWith s ":") *@ strip) in
  let r = (sequence [pLit "a"; pLit "b"]) tokens in
  match r with
    | None -> failwith "parse fail"
    | Some(t,rest) ->
      List.iter (printf "%s\n" *@ Ast.show id) t
(*let rec pInstr =                                                     *)
(*  sequence [pLit "l";pAny ["b";"h";"w";"d"]; (@* ) pAny ["l";"r";"u"]]*)
(*  <|>                                                                *)


(*open ExtList                                                                               *)
(*let attr l = sort @$ concatMap pMod l                                                      *)
(*                                                                                           *)
(*let pInstr =                                                                               *)
(*    sequence [pAny ["l";"s"];pAny ["b";"h";"w";"d"]; zeroOrOne pMod;pRegAddr]              *)
(*    <|> sequence [zeroOrOne pMod;pAny                                                      *)
(*        ;pRegRegReg<|>pRegRegImmed]                                                        *)
(*    <|> sequence [zeroOrOne pMod;pAny ["mult";"div"];zeroOrOne pMod                        *)
(*        ;pRegReg]                                                                          *)
(*(*let rec pInstr l = match l with                                               *)         *)
(*(*  | ('l'|'s') as x :: ('b'|'h'|'w'|'d') as x' :: ('u'|'l'|'r'|_) as x''::xs ->*)         *)
(*(*    Attr (attr [x';x''],                                                      *)         *)
(*(*    let                                                                       *)         *)
(*(*    if x='s' then Store else Load)                                            *)         *)
(*and pMod = pAny ["l";"r";"u";".ps";".s";".d"]                                              *)
(*open ExtList                                                                               *)
(*open ExtChar                                                                               *)
(*                                                                                           *)
(*                                                       *)
(*                                                                                           *)

(*                                                                                           *)
(*let () =                                                                                   *)
(*  printf "%s\n" @$ show_stringlist @$ lex "sd $2,0($sp)"                                   *)
