open Linda
module type EQ = sig
  type t
  val (=) : t -> t -> bool
end
module Eq (E:EQ) = struct
  type t = E.t
  let (<>) a a' = not (a = a') 
end
module type ORD = sig
  type t
  val compare : t -> t -> int
end
module Ord (O:ORD) = struct
  type t = O.t
  let (>=) a a' = O.compare a a' >=0
  let (<=) a a' = O.compare a a' <=0
  let (>) a a' = O.compare a a' >0
  let (<) a a' = O.compare a a' <0
  let ge = (>=)
  let le = (<=)
  let gt = (>)
  let lt = (<)
  let max a a' = if a > a' then a else a'
  let min a a' = if a > a' then a' else a
end

module type MONAD = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
module type MONAD2 = sig
  type ('s,'a) t
  val bind : ('s,'a) t -> ('a -> ('s,'b) t ) -> ('s,'b) t
  val return : 'a -> ('s,'a) t
end
(*module type MONADTYP = sig*)
(*(*  type t*)              *)
(*  type t                  *)
(*  val bind: 'a -> 'b -> 'c*)
(*  val return : 'd -> 'e   *)
(*end                       *)
module Monad (M:MONAD) = struct
  open M
  include M  
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id 
  let fmap f xs = xs >>= (return *@ f)
  let fail = failwith
	let liftM f m = m >>= fun x -> return @$ f x
	let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
	let liftM3 f m m' m'' = 
	  m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
	let ap fm m =
	    fm >>= fun f ->
	        m >>= fun x ->
	            return @$ f x            
	let sequence ms =
	    let k m m' =
	        m >>= fun x ->
	            m' >>= fun xs ->
	                return (x::xs) in  
	    List.fold_right k ms (return [])    
end

module Monad2 (M:MONAD2) = struct
  open M
  include M
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap f xs = xs >>= (return *@ f)
  let fail = failwith
    let liftM f m = m >>= fun x -> return @$ f x
    let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
    let liftM3 f m m' m'' =
      m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
    let ap fm m =
        fm >>= fun f ->
            m >>= fun x ->
                return @$ f x
    let sequence ms =
        let k m m' =
            m >>= fun x ->
                m' >>= fun xs ->
                    return (x::xs) in
        List.fold_right k ms (return [])
end

module type MONADPLUS = sig
  include MONAD
  val mzero : 'a t
  val mplus : 'a t -> 'a t -> 'a t
end
module MonadPlus (M:MONADPLUS) = struct
  open M
  include M
  let guard flg = if flg then return () else mzero
  let msum l = List.fold_right mplus l mzero
end

module MaybeMonad = Monad (struct
  type 'a t = 'a option
  let bind m f = match m with
    | None -> None
    | Some x -> f x
  let return x = Some x 
(*  let mzero = None                *)
(*  let mplus a a' = match a,a' with*)
(*    | Some x, _ -> Some x         *)
(*    | _,x' -> x'                  *)
  end)
module ListMonad = Monad (struct
  open List
  type 'a t = 'a list
  let bind m f = concat @$ map f m
  let return x = [x] 
(*  let mzero = []  *)
(*  let mplus = (@) *)
  end)
  
module StateMonad = Monad2 (struct
    type ('s,'a) t = 's -> 'a *'s
    let bind (m: ('s,'a) t) (k: 'a -> ('s,'b) t) : ('s,'b) t =
      fun s ->
          let (a', s') = m s in
          let (a'', s'') = k a' s' in
          (a'', s'')
    let return (x:'a) : ('s,'a) t = fun s -> (x, s)
  end)

