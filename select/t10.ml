open Linda
open Printf
open ExtString

open T160
module MipsAsmParser = struct
  
  open ExtString
  let elimComment s = takeWhile ((<>) '#') s
  let stripComment = List.filter (not *@(startsWith ~needle:"#") *@lstrip)
  let stripLoc = List.filter (not *@(startsWith ~needle:".loc") *@lstrip)
  open ExtList
  open ListMonad
  
  module BB = struct
    type t = {
      labels : string list;
      instrs : Mips.instr list
    }
    let of_list l =
      let labels, l' = ExtList.partition (Logic.either (token ".") (token "$")) (stripLoc l) in
      let l''' = map (InstrLexer.tokenize) l' in
      (* let l''' = map (InstrLexer.tokenize) @$ List.filter (Logic.neither      *)
      (* (token ".") (token "$")) l in iter (printf "%s\n--------\n" *@          *)
      (* show_stringlist) l''';                                                  *)
      List.iter (fun i ->
              let s1 = ExtString.replace ((Mips.showInstr *@MipsInstrParse.parse) i) " " "" in
              let s2 = String.concat "" i
                |> ExtString.substitute ~old:"$gp" ~new_:"$28"
                |> ExtString.substitute ~old:"$sp" ~new_:"$29"
                |> ExtString.substitute ~old:"ldc1" ~new_:"l.d"
                |> ExtString.substitute ~old:"lwc1" ~new_:"l.s"
                |> ExtString.substitute ~old:"sdc1" ~new_:"s.d"
                |> ExtString.substitute ~old:"swc1" ~new_:"s.s"
              in
              if s1 <> s2 then printf "%s <> %s\n" s1 s2) l''';
      { labels = labels;
        instrs = map MipsInstrParse.parse l'''}
    let to_list t = t.labels @ map Mips.showInstr t.instrs
  end
  module Function = struct
    type t = {
      header : string list;
      bbs : BB.t list;
      tail : string list
    }
    let mapOverBBs f t =
      { t with bbs = map f t.bbs }
    let concatMapOverBBs f t =
      sequence (map f t.bbs) >>= fun bbs ->
          return { t with bbs = bbs }
    open ExtString
    open List
    let of_list l =
      let l', l2 = break (token "$") (List.map elimComment @$ stripComment l) in
      let l2', l2'' = span (token ".") (rev l2) in
      { header = l' ;
        bbs = map BB.of_list @$ packBy (fun s -> Logic.either (startsWith ~needle:"$") (endsWith ~needle:":") @$ strip s) @$ rev l2'';
        tail = rev l2'}
    let to_list t =
      t.header @ concatMap BB.to_list t.bbs @ t.tail
  end
  module Section = struct
    type t = {
      header : string list;
      funs : (string * Function.t) list
    }
    let mapOverFuns f t =
      { t with funs = map (fun (i, l) -> (i, f l)) t.funs }
    let concatMapOverFuns f t =
      let f' (i, l) =
        f l >>= fun l' ->
            return (i, l') in
      sequence (map f' t.funs) >>= fun funs ->
          return { t with funs = funs }
    let of_list l =
      let l' = packBy (token ".ent") @$ l in
      let l'' = map (fun l ->
                (strip *@elimComment) (nthField 1 (replace (hd l) "\t" " ")), Function.of_list @$ l) @$
        tl l' in
      { header = hd l';
        funs = l''}
    let to_list t =
      t.header @ concatMap (Function.to_list *@snd) t.funs
  end
  type file = {
    name : string;
    header : string list;
    sections : Section.t list
  }
  let of_file fn =
    let l = packBy (token ".section") @$ cleanLines (ExtUnix.readFile fn) in
    { name = fn;
      header = hd l;
      sections = map Section.of_list @$ tl l }
  let mapOverFuns f t =
    { t with sections = map (Section.mapOverFuns f) t.sections }
  let concatMapOverSections f t =
    sequence (map f t.sections) >>= fun sections ->
        return { t with sections = sections }
  let concatMapOverBBs f t =
    concatMapOverSections (Section.concatMapOverFuns (Function.concatMapOverBBs f)) t
  let mapOverBBs f t = mapOverFuns (Function.mapOverBBs f) t
  let show f = show_listlist ~brackets: ("","") ~sep:"\n" id (f.header:: map Section.to_list f.sections)
end

open MipsAsmParser
open ExtList
open ListMonad
module IntCounter = Algebra.Counter(Algebra.Int)
let getCount =
  let t = IntCounter.create () in
  fun () -> t.IntCounter.inc ();t.IntCounter.get ()
  
let () =
  print_endline @$ show @$ of_file (Sys.argv.(1))  
  
(*let () =                                                                                                                                                         *)
(*  Random.self_init ();                                                                                                                                           *)
(*(*  let trans bb = if List.mem "$LBB19_dotproduct:" bb.BB.labels then begin                                                                         *)           *)
(*(*      let a = ExtArray.take (length bb.BB.instrs - 2) @$ Array.of_list bb.BB.instrs in                                                            *)           *)
(*(*      concat @$ maybeToList (                                                                                                                     *)           *)
(*(*          MaybeMonad.bind (PartialOrder.TransitiveClosure.ofPartialOrder (PartialOrder.of_list @$ Mips.Analysis.orders bb.BB.instrs)) @$ fun tc ->*)           *)
(*(*              MaybeMonad.return @$ concatMap (fun _ -> maybeToList @$ PartialOrder.random                                                         *)           *)
(*(*                        tc                                                                                                                        *)           *)
(*(*                        (ExtList.range 0 (Array.length a))) (range 0 100)                                                                         *)           *)
(*(*        ) >>= fun p ->                                                                                                                            *)           *)
(*(*          return { bb with BB.instrs =                                                                                                            *)           *)
(*(*                (Array.to_list @$ Permutation.follow (Permutation.of_list p) a) @                                                                 *)           *)
(*(*                drop (length bb.BB.instrs - 2) @$ bb.BB.instrs }                                                                                  *)           *)
(*(*    end else [bb] in                                                                                                                              *)           *)
(*  let getTcA file =                                                                                                                                              *)
(*    let tc = ref None in                                                                                                                                         *)
(*    let a = ref None in                                                                                                                                          *)
(*    let trans bb =                                                                                                                                               *)
(*      if List.mem "$LBB19_dotproduct:" bb.BB.labels then begin                                                                                                   *)
(*      tc := PartialOrder.TransitiveClosure.ofPartialOrder (PartialOrder.of_list @$ Mips.Analysis.orders bb.BB.instrs);                                           *)
(*      a := Some bb.BB.instrs;                                                                                                                                    *)
(*      bb                                                                                                                                                         *)
(*      end else bb in                                                                                                                                             *)
(*    ignore @$ mapOverBBs trans file;                                                                                                                             *)
(*    (fromSome @$ !tc),fromSome !a                                                                                                                                *)
(*     in                                                                                                                                                          *)
(*  let permToFile ofile newFileName p =                                                                                                                           *)
(*    let trans bb = if List.mem "$LBB19_dotproduct:" bb.BB.labels then begin                                                                                      *)
(*      let a = ExtArray.take (length bb.BB.instrs - 2) @$ Array.of_list bb.BB.instrs in                                                                           *)
(*      { bb with BB.instrs =                                                                                                                                      *)
(*                (Array.to_list @$ Permutation.follow (Permutation.of_list p) a) @                                                                                *)
(*                drop (length bb.BB.instrs - 2) @$ bb.BB.instrs }                                                                                                 *)
(*      end else bb in                                                                                                                                             *)
(*    let file = mapOverBBs trans ofile in                                                                                                                         *)
(*    {file with name = newFileName} in                                                                                                                            *)
(*                                                                                                                                                                 *)
(*  let timeit f =                                                                                                                                                 *)
(*(*    let fn = sprintf "dotproduct%d.s" (Linda.counter ()) in*)                                                                                                  *)
(*    let fn = f.name in                                                                                                                                           *)
(*    ExtUnix.writeFile fn (show f);                                                                                                                               *)
(*    ignore @$ Sys.command @$ Loongson.Compile.mylgcc2 ^" ~/tmpd/testlib.c "^fn^" ~/tmpd/dotp_test.c >/dev/null 2>&1";                                            *)
(*    let rs = Array.init 3 (fun i -> T31.RE.timeitBin "a.out") in                                                                                                 *)
(*    let r = ExtArray.medianBy T31.Score.compare @$ rs in                                                                                                         *)
(*    printf "scores:%s\n" (show_array T31.Score.show rs);                                                                                                         *)
(*    printf "%s score:%s\n" fn (T31.Score.show r);                                                                                                                *)
(*    flush stdout;                                                                                                                                                *)
(*    r in                                                                                                                                                         *)
(*  let extractScore s =                                                                                                                                           *)
(*    ExtString.takeWhile ExtChar.isDigit @$ List.nth (ExtString.split ~needle:"ration:" (List.hd @$ List.filter (fun hay ->                                       *)
(*      isSome @$ ExtString.find "ration" hay 0 (ExtString.length hay)) @$ ExtString.lines s)) 1 in                                                                *)
(*  let timeit f =                                                                                                                                                 *)
(*    ExtUnix.writeFile "/home/zsc/lgccO0-mk/pathscale-compiler/libloongson/dotproduct.s" (show f);                                                                *)
(*    let fn = f.name in                                                                                                                                           *)
(*    ExtUnix.writeFile fn (show f);                                                                                                                               *)
(*    ignore @$ Sys.command @$ "make install -C /home/zsc/lgccO0-mk/pathscale-compiler >/dev/null 2>&1";                                                           *)
(*    let sc = T31.Score.Complete (Algebra.Float.neg @$ float_of_int @$ int_of_string @$ extractScore @$ ExtUnix.exec "cd /home/zsc/2im/spec2000/bin;./cmd art") in*)
(*    printf "%s score:%s\n" fn @$ T31.Score.show sc;                                                                                                              *)
(*    flush stdout;                                                                                                                                                *)
(*    sc in                                                                                                                                                        *)
(*                                                                                                                                                                 *)
(*  let file = of_file "/home/zsc/tmpd/dotproduct.s" in                                                                                                            *)
(*  let tc,a = getTcA file in                                                                                                                                      *)
(*  let permName =                                                                                                                                                 *)
(*    let permNames = Hashtbl.create 3 in                                                                                                                          *)
(*    fun p ->                                                                                                                                                     *)
(*      try Hashtbl.find permNames p                                                                                                                               *)
(*      with Not_found ->                                                                                                                                          *)
(*        let fn = sprintf "dotproduct%d.s" @$ getCount () in                                                                                                      *)
(*        Hashtbl.add permNames p fn;                                                                                                                              *)
(*        fn in                                                                                                                                                    *)
(*  let hist = Hashtbl.create 3 in                                                                                                                                 *)
(*  let timeitPerm p =                                                                                                                                             *)
(*    try Hashtbl.find hist p with Not_found ->                                                                                                                    *)
(*        let r = timeit (permToFile file (permName p) p) in                                                                                                       *)
(*        Hashtbl.add hist p r;                                                                                                                                    *)
(*        r in                                                                                                                                                     *)
(*  let module PO = Ga.Population(struct                                                                                                                           *)
(*	  type t = int list                                                                                                                                            *)
(*	  let random l =                                                                                                                                               *)
(*      fprintf stderr "random\n";                                                                                                                                 *)
(*      flush stderr;                                                                                                                                              *)
(*	    fromSome @$ PartialOrder.random tc l                                                                                                                       *)
(*	(*  let countGenes = Algebra.dumb*)                                                                                                                            *)
(*	  let crossover a =                                                                                                                                            *)
(*      fprintf stderr "crossover %s\n" @$ show_array permName a;                                                                                                  *)
(*      flush stderr;                                                                                                                                              *)
(*      PartialOrder.crossover a                                                                                                                                   *)
(*	  let show l =                                                                                                                                                 *)
(*      show_pair T31.Score.show id  (timeitPerm l,permName l)                                                                                                     *)
(*(*      ^ "\n" ^  show_intlist l *)                                                                                                                              *)
(*	  let mutate p l =                                                                                                                                             *)
(*      fprintf stderr "mutate %s\n" @$ permName l;                                                                                                                *)
(*      flush stderr;                                                                                                                                              *)
(*      fromSome @$ PartialOrder.mutate tc p l                                                                                                                     *)
(*  end) in                                                                                                                                                        *)
(*  let l = map (fun f -> timeit f, f) @$                                                                                                                          *)
(*    map (fun p -> permToFile file (permName p) p) @$ Array.to_list @$                                                                                            *)
(*        funPower 95 (                                                                                                                                            *)
(*          fun l ->                                                                                                                                               *)
(*            let r = PO.evolve 8 8 (Ga.Elitism 0.3) 0.3 (0.5,0.3) timeitPerm (flip T31.Score.compare) l in                                                        *)
(*(*            map pairs (to_list r);*)                                                                                                                           *)
(*            print_endline @$ show_listlist (fun (a,b) -> string_of_int @$ PartialOrder.distance a b) @$                                                          *)
(*                pairTable @$ Array.to_list r;                                                                                                                    *)
(*            r)                                                                                                                                                   *)
(*            (PO.create 8 (range 0 (length a-2))) in                                                                                                              *)
(*(*  let l = map (fun f -> timeit f, f) @$ concatMapOverBBs trans @$             *)                                                                               *)
(*(*    of_file "/home/zsc/tmpd/dotproduct.s" in                                  *)                                                                               *)
(*  print_endline @$ show_list (fun (v, f) -> T31.Score.show v) l;                                                                                                 *)
(*  let f = snd @$ maximumBy (fun (v, _) (v', _) -> T31.Score.compare v v') l in                                                                                   *)
(*  fprintf stderr "%s\n" @$ T31.Score.show @$ timeit f                                                                                                            *)
(*                                                                                                                                                                 *)
