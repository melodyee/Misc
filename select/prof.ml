open Linda
module Oprofile = struct
  open Printf
  open Command
  open ExtString
  let rmLog = "rm -f /var/lib/oprofile/samples/oprofiled.log"
  let reset = "opcontrol --reset"
  
  let measure cmd =
    "opcontrol --start -V" <+> cmd <+> "opcontrol --stop" <+> "opcontrol -d" <+>
    "opcontrol -h"
  
  let report ?(threshhold = 0.1) ?(detailed = false) =
    sprintf "opreport -t %f -s sample" threshhold <+>
    guard detailed "-l"
  
  let annot ?(threshhold = 0.05) fn =
    sprintf "opannotate -a -t %f >%s" threshhold fn
  
  let events =
    [(0, ("CPU_CLK_UNHALTED", "INSTRUCTION_COMMITTED"));
    (1, ("BRANCH_INSTRUCTIONS", "BRANCHES_MISPREDICTED"));
    (2, ("JUMP_INSTRUCTIONS", "JR_MISPREDICTED"));
    (3, ("JR31_INSTRUCTIONS", "JR31_MISPREDICTED"));
    (4, ("ICACHE_MISSES", "DCACHE_MISSES"));
    (5, ("ALU1_ISSUED", "ALU2_ISSUED")); (6, ("MEM_ISSUED", "FALU2_ISSUED"));
    (7, ("FALU1_ISSUED", "UNCACHED_ACCESS"));
    (8, ("BHT_BRANCH_INSTRUCTIONS", "BHT_MISPREDICTED"));
    (9, ("MEM_READ", "MEM_WRITE")); (10, ("FQUEUE_FULL", "FTQ_FULL"));
    (11, ("ROQ_FULL", "BRANCH_QUEUE_FULL"));
    (12, ("CP0_QUEUE_FULL", "ITLB_MISSES"));
    (13, ("TLB_REFILL", "TOTAL_EXCEPTIONS"));
    (14, ("EXCEPTION", "LOAD_SPECULATION_MISSES"));
    (15, ("INTERNAL_EXCEPTION", "CP0Q_FORWARD_VALID"))]
  
  let setup ~eventPairNumber =
    let a, b = List.assoc eventPairNumber events in
    let count s =
      if List.mem s ["CPU_CLK_UNHALTED";"INSTRUCTION_COMMITTED"]
      || isSubStringOf "_ISSUED" s
      then 5000000 else 500000 in
    sprintf "opcontrol --setup --event=%s:%d --event=%s:%d" a (count a) b (count b)
  
  let test ~eventPairNumber ?(threshhold = 0.1) ?(detailed = false) annotName cmd =
    rmLog <+> reset <+> setup eventPairNumber <+>
    measure cmd <+> report ~threshhold ~detailed <+>
    (match annotName with
      | None -> ""
      | Some fn -> annot ~threshhold fn)
  
end
open Command
open Oprofile
let () =
  print_endline @$
  List.fold_left (<+>) "" @$ List.map (fun i -> test i None "./a.out") (ExtList.range 0 16)
