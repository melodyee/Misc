type tau = TInt | TBool
type sigma = TComm | TVar of tau | TExp of tau
type theta = TBase of sigma | TApp of sigma * theta

(*
type tau = TInt $|$ TBool

type sigma = TCommand $|$ TVar of tau $|$ TExp of tau

type theta = TBase of sigma $|$ TApp of theta * theta
\end{block}
\end{frame}

\begin{frame}
\begin{block}{Constructs}
\begin{itemize}
\item type exp = ESeq of command * exp $|$ Int of int $|$ Bool of bool

$|$ Deref of var

\item type command = Skip $|$ Omega

$|$ Assign of var * exp

$|$ CSeq of command * command

$|$ While of exp * command $|$ If of exp * command * command

$|$ New of var * theta * exp

$|$ App of var * exp list
*)

module ERegLang = struct
  open List
  type tag = string
  type 'a t =
    | Bot
    | Eps
    | Letter of 'a * tag
    | Iter of 'a t
    | Seq of 'a t list
    | Union of 'a t list
    | Inter of 'a t list
    | Remove of 'a t * tag list
  let letter e t = Letter (e,t)
  let iter a = Iter a
  let concat a b = Seq [a;b]
  let union a b = Union [a;b]
  let inter a b = Inter [a;b]
  let remove a l = Remove (a,l)
  let isSeq = function Seq _ -> true    | _ -> false
  let isUnion = function Union _ -> true    | _ -> false
  let isInter = function Inter _ -> true    | _ -> false
  let isBotEps x = x==Bot || x==Eps
  let lift f =
    let rec work x =
    match x with
    | Bot
    | Eps
    | Letter _ -> f x
    | Iter e -> Iter (work e)
    | Seq l -> Seq (map work l)
    | Union l -> Union (map work l)
    | Inter l -> Inter (map work l)
    | Remove (t,l) -> Remove (work t,l) in
    work 
  let atom_simp on_change x = function
    | Iter Bot -> on_change ();Bot
	  | Iter Eps -> on_change ();Eps
    | Iter (Iter a) -> on_change ();Iter a
    | Seq [] -> on_change ();Bot
    | Seq l when exists (fun x -> isSeq x || isBotEps x) l ->
	      let aux x acc = match x with
          | Eps -> acc
	        | Seq y -> y@acc
	        | _ -> [x]@acc in
	      on_change ();
        if exists ((==)Bot) l then Bot else
        Seq (fold_right aux l [])
    | Union l when exists (fun x -> x==Bot || isUnion x) l ->
      let aux x acc = match x with
        | Bot -> acc
        | Union y -> y@acc
        | _ -> [x]@acc in
      on_change ();
      Union (fold_right aux l [])
    | Inter l when exists (fun x -> isInter x || isBotEps x) l ->
      let aux x acc = match x with
        | Inter y -> y@acc
        | _ -> [x]@acc in
      on_change ();
      if exists ((==)Bot) l then Bot else
        if exists ((==)Eps) l then Eps else
          Inter (fold_right aux l [])
    | _ -> x
end   