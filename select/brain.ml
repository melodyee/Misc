open Linda
open Printf

type code =
  | Loop of code
  | Seq of code list
  | Input
  | Output
  | Plus
  | Minus
  | Right
  | Left

let rec show = function
  | Loop c -> sprintf "[%s]" (show c)
  | Seq cs -> show_list ~brackets:("","") ~sep:"" show cs 
  | Input -> ","
  | Output -> "."
  | Plus -> "+"
  | Minus -> "-"
  | Right -> ">"
  | Left -> "<"

let rec dump = function
  | Loop c -> sprintf "Loop(%s)" (dump c)
  | Seq cs -> show_list ~sep:";" dump cs
  | c -> show c 

let rec optimize = function
  | Seq [Seq cs] -> optimize @$ Seq cs
  | Seq (Seq cs:: xs) -> optimize @$ Seq (cs@xs)
  | Seq cs -> Seq (optimize_list cs)
  | Loop c -> Loop (optimize c)
  | x -> x
and optimize_list l = match l with
  | Left:: Right:: xs
  | Right:: Left:: xs
  | Plus:: Minus:: xs
  | Minus:: Plus:: xs -> optimize_list xs
  | [] -> []
  | Seq cs :: xs -> optimize_list (cs@xs)
  | x:: xs -> x:: optimize_list xs 
      
let rec deepOptimize code =
  let code' = optimize code in
  if dump code' <> dump code then deepOptimize code' else code

let loop c = Loop c
let seq l = optimize @$ Seq l
                        
type state = int * int Parray.t
open Parray

let rec exec code ((i,pa) as state) = match code with
  | Loop c ->
    if get pa i=0 then state else
    LazyList.head @$
        LazyList.dropWhile (fun (i,pa) -> get pa i <>0) @$ LazyList.funPowers (exec c) state
  | Seq cs -> List.fold_left (flip exec) state cs
  | Input ->
    let v = int_of_char (input_line stdin).[0] in
    i,set pa i v
  | Output ->
    printf "%c" @$ char_of_int @$ (get pa i);
    state
  | Plus ->
    i,set pa i (succ @$ get pa i)
  | Minus ->
    i,set pa i (pred @$ get pa i)
  | Right -> rem (i+1) @$ length pa,pa
  | Left ->  rem (i-1) @$ length pa,pa

open Parray
type cell = {
    alloced : bool;
    value : int option
  }
let getAlloced p i = (get p i).alloced
let setAlloced p i = set p i {(get p i) with alloced = true}
let setFree p i = set p i {(get p i) with alloced = false}
let getValue p i = (get p i).value
let setValue p i v = set p i {(get p i) with value =v} 
let isFree p i = not (getAlloced p i)

module Construct = struct
  open ExtList
  
  open StateMonad
  let scrollTo i' =
    get >>= fun (i,heap) ->
      put (i',heap) >>= fun _ ->
        return @$ seq (if i=i' then [] else
          if i<i' then replicate (i'-i) Right else
            replicate (i-i') Left)
  let addInteger i' value =
    fmap seq @$ sequence @$
      if value=0 then [] else 
	      [scrollTo i';
        get >>= fun (i,heap) ->
		      return (seq (if value>0 then replicate value Plus else
			        replicate (-value) Minus)) >>= fun code ->
            put(i,setValue heap i (MaybeMonad.liftM2 (+) (Some value) @$ getValue heap i)) >>= fun _ ->
              return code        
            ]
	  
  let print i =
    fmap seq @$ sequence [scrollTo i;return Output]
  let inputTo i =
    get >>= fun (_,heap) ->
	    fmap seq @$ sequence [scrollTo i;
	        return Input] >>= fun code ->       
          put(i,setValue heap i None) >>= fun _ ->
            return code
  let clear i =
    get >>= fun (_,heap) ->
      match getValue heap i with
        | Some v -> addInteger i (-v)
        | None -> fmap seq @$ sequence @$ [
            scrollTo i;
            return (Loop(Seq [Minus]))]
  let set i value =
    get >>= fun (_,heap) ->
      match getValue heap i with
        | Some v -> addInteger i (value-v)
        | None -> fmap seq @$ sequence 
            [clear i;
            addInteger i value]
(*  let loopM m =                  *)
(*    get >>= fun (i,_) ->         *)
(*      m >>= fun code ->          *)
(*        get >>= fun (_,heap) ->  *)
(*        put (i,heap) >>= fun _ ->*)
(*          return (loop code)     *)
  let allocVar =
    get >>= fun (i',heap) ->
      let rec work i =
        if i>=Parray.length heap then return None else
          if isFree heap i then begin 
            put (i',setAlloced heap i) >>= fun _ ->
            return (Some i)
            end
          else work (succ i) in
      work 0 >>= function 
        | None -> failwith "allocVar"
        | Some i -> return i
  let freeVar i =
    get >>= fun (i,heap) ->
      clear i >>= fun _ ->
      put (i,setFree heap i)
    
  let print_char ch =
(*    fmap seq @$ sequence [*)
      allocVar >>= fun i ->
      fmap seq @$ sequence [set i (int_of_char ch);print i] >>= fun code ->
        freeVar i >>= fun _ ->
          return code
  let print_string s =
    fmap seq @$ sequence @$ map print_char @$ ExtString.to_list s
  let input =
    allocVar >>= fun i ->
      inputTo i
(*  let copyFromTo i i' =                                                                                *)
(*    if i=i' then sequence [] else                                                                      *)
(*      allocVar >>= fun t ->                                                                            *)
(*        get >>= fun (i,heap) ->                                                                        *)
(*        fmap (loop*@seq) @$ sequence [addInteger i (-1);addInteger i' 1;addInteger t 1] >>= fun code ->*)
(*          put (i,setValue heap                                                                         *)
  let rec addValueFromToWithPositveScale n i i' =
    assert (n>=0);
    if n=0 then return (seq []) else
	    if i=i' then
	      allocVar >>= fun t ->
	        fmap seq @$ sequence [addValueFromToWithPositveScale n i t;
	        addValueFromToWithPositveScale 1 t i']
	    else
		    get >>= fun (_,heap) ->
          let v =
            MaybeMonad.liftM2 (+) (getValue heap i') @$ MaybeMonad.liftM (( * ) n) (getValue heap i) in
          fmap seq @$ sequence [scrollTo i;
          fmap (loop*@seq) @$ sequence @$
		        [addInteger i (-1);
		          addInteger i' n;
              scrollTo i] >>= fun code ->
		      put (i,setValue (setValue heap i (Some 0)) i' v) >>= fun _ ->
	          return code]
  let addValueFromTo i i' = addValueFromToWithPositveScale 1 i i'
  let moveValueFromTo i i' = 
    fmap seq @$ sequence [clear i';addValueFromTo i i']
  let rec moveValueToList i l =
    if List.mem i l then
      allocVar >>= fun t ->
        fmap seq @$ sequence [moveValueFromTo i t;
          moveValueToList t l]
    else
      fmap seq @$ sequence [scrollTo i;
        fmap (loop*@seq) @$ sequence ([addInteger i (-1)] @ map (flip addInteger 1) l @ [scrollTo i])] 
  let copyFromTo i i' =
    moveValueToList i [i;i']
  let add i i' i'' =
    get >>= fun (_,heap) ->
      match MaybeMonad.liftM2 (+) (getValue heap i) (getValue heap i') with
        | Some v -> set i'' v
        | None -> 
          fmap seq @$ sequence [addValueFromTo i i'';addValueFromTo i' i'']
  let mul i i' i'' =
    get >>= fun (_,heap) ->
      match MaybeMonad.liftM2 ( * ) (getValue heap i) (getValue heap i') with
        | Some v -> set i'' v
        | None ->
          allocVar >>= fun bi' ->
            allocVar >>= fun t ->
          fmap seq @$ sequence [scrollTo i;
          fmap (loop*@seq) @$ sequence @$
                [addInteger i (-1);
                copyFromTo i' bi';
                addValueFromTo bi' t;
                scrollTo i] >>= fun code ->
              get >>= fun (_,heap) ->
              put (i,setValue heap i (Some 0)) >>= fun _ ->
              put (i,setValue heap i'' None) >>= fun _ ->
              return code]
  let isZero i i' =
      fmap seq @$ sequence [scrollTo i;
        fmap (loop*@seq) @$ sequence ([addInteger i (-1);set i' 1;scrollTo i])]     
  let printSum =
    allocVar >>= fun i ->
      allocVar >>= fun i' ->
        allocVar >>= fun i'' ->
	        fmap seq @$ sequence [inputTo i;
          inputTo i';
          addInteger i' (- int_of_char '0');
          add i i' i'';
          print i'';
          print_char '\n']
  let printProd =
    allocVar >>= fun i ->
      allocVar >>= fun i' ->
        allocVar >>= fun i'' ->
            fmap seq @$ sequence [inputTo i;
            addInteger i (- int_of_char '0');
            inputTo i';
            addInteger i' (- int_of_char '0');
            mul i i' i'';
            addInteger i'' (int_of_char '0');
            print i'';
            print_char '\n']
  let and_ i i' i'' =
    fmap seq @$ sequence [scrollTo i;
    fmap (loop*@seq) @$ sequence [addInteger i (-1);
    scrollTo i';
    fmap (loop*@seq) @$ sequence [addInteger i' (-1);
    set i'' 1;scrollTo i'];
    scrollTo i]]
  let or_ i i' i'' =
    add i i' i''
  let not_ i i' =
    fmap seq @$ sequence [set i' 1;
    scrollTo i;
    fmap (loop*@seq) @$ sequence [addInteger i (-1);
    clear i';
    scrollTo i]]
  let bool_eq i i' i'' =
    allocVar >>= fun t ->
      allocVar >>= fun bi ->
        allocVar >>= fun bi' ->
          fmap seq @$ sequence [copyFromTo i bi;
          copyFromTo i' bi';
          and_ i i' t;
          moveValueFromTo bi i;
          moveValueFromTo bi' i';
          or_ i i' bi;
          not_ bi bi';
          or_ bi' t i''] >>= fun code ->
            freeVar t >>= fun _ ->
              freeVar bi >>= fun _ ->
                freeVar bi' >>= fun _ ->
                  return code
  let rec bool_eq_list l l' i =
    if null l then return @$ seq [] else
    allocVar >>= fun t ->
      fmap seq @$ sequence [
      bool_eq (hd l) (hd l') t;
        bool_eq_list (tl l) (tl l') i;
        allocVar >>= fun t' ->
          fmap seq @$ sequence [
            and_ t i t';
            moveValueFromTo t' i]]
  let rec copyListTo l l' =
    if null l then return @$ seq [] else
      fmap seq @$ sequence [copyFromTo (hd l) (hd l');
      copyListTo (tl l) (tl l')]
(*  let isSubStringOf l l' =      *)
(*    replicate (length ) allocVar*)


  end

let () =
  let initState = 0,init 255 (fun i -> 0) in
  let heap = 0,init 255 (fun i -> {alloced = false;value = Some 0}) in
(*  let code = fst @$ StateMonad.runState (Construct.print_string "Hello World\n") heap in*)
  let code = fst @$ StateMonad.runState Construct.printProd heap in
  print_endline (dump code);
  print_endline (show code);
  printf "orig = %d\n" @$ String.length (show code);
(*  let code' = deepOptimize @$ code in               *)
(*  print_endline (dump code');                       *)
(*  print_endline (show code');                       *)
(*  printf "opt = %d\n" @$ String.length (show code');*)
  ignore @$ exec code initState
  
(*module Construct = struct*)
(*  let print_string s =   *)
(*                         *)
(*  end                    *)