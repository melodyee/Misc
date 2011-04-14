open Printf
open Linda
open ExtList
open Algebra

(*module Target                                 *)
(*:sig                                          *)
(*  type t                                      *)
(*  val ofString : string -> t                  *)
(*  val make : string -> string -> t            *)
(*  val show : t -> string                      *)
(*  end                                         *)
(* = struct                                     *)
(*  open ExtString                              *)
(*  type t = string                             *)
(*  let ofString = id                           *)
(*  let make account host = account ^ "@" ^ host*)
(*  let show t = t                              *)
(*  let account t = List.nth (splitChar '@' t) 0*)
(*  let host t = List.nth (splitChar '@' t) 1   *)
(*  end                                         *)

module ExtFilename = struct
  open MaybeMonad
  open Filename
(*  type filename = string                                                       *)
(*  type path = string                                                           *)
(*  type t =                                                                     *)
(*    | File of (path * filename)                                                *)
(*    | Dir of path                                                              *)
(*                                                                               *)
(*  let ofString s =                                                             *)
(*    if Sys.file_exists s then                                                  *)
(*        if Sys.is_directory s then return @$ Dir (fromSome (toAbsolute s)) else*)
(*            let file = fromSome (toAbsolute s) in                              *)
(*            return @$ File (dirname file, basename file)                       *)
(*    else mzero                                                                 *)
(*  let show = function                                                          *)
(*    | File (p,fn) -> Filename.concat p fn                                      *)
(*    | Dir p -> p                                                               *)

(*  let file path filename = File (path,filename)*)
(*  let dir path = Dir path*)
  let is_absolute s = not (is_relative s) 
  let separator = String.get (Filename.concat current_dir_name current_dir_name) 1
  open ExtList
  let simplify s =
    let rec work acc = function 
      | [] -> rev acc
      | x::xs ->
         if x = current_dir_name then work acc xs else
          if x = parent_dir_name then work (List.tl acc) xs else
            if x = "~" then work (List.rev (ExtString.splitChar separator @$ Sys.getenv "HOME") @ acc) xs else
            work (x::acc) xs in
     try Some (List.fold_left Filename.concat "/" @$ work [] (ExtString.splitChar separator s)) with
      | Failure _ -> None
  let toAbsolute s =
    simplify  (if is_absolute s then s else
      Filename.concat (Sys.getcwd ()) s)
  include Filename 
  end

module type COMMAND = sig
      type command
      type t
      val (<+>) : command -> command -> command
      val exec : command -> string
      val ofString : string -> command
      val pwd : command
      val cd : string -> command
      val home : command
      val ls : command
      val cp : string -> string -> command
      val kill : int -> command
      val killall : string -> command
      val copyTo : string -> string -> command
      val copyFrom : ?isDir:bool -> string -> string -> command
    end
module Spec2k = struct
  open ExtList
    let intSpec = ["gzip"; "vpr"; "gcc"; "mcf"; "crafty"; "parser"; "eon"; "perlbmk"; "gap"; "vortex"; "bzip2"; "twolf"]
    let floatSpec = ["wupwise"; "swim"; "mgrid"; "applu"; "mesa"; "galgel"; "art"; "equake"; "facerec"; "ammp"; "lucas"; "fma3d"; "sixtrack"; "apsi"]
    let isInt s = mem s intSpec
    let isFloat s = mem s floatSpec
    let remoteDir spec =
      let work spec = 
	      let dirs = "164.gzip 181.mcf 252.eon 255.vortex 175.vpr 186.crafty 253.perlbmk 256.bzip2 197.parser 254.gap 300.twolf164.gzip 181.mcf 252.eon 255.vortex 175.vpr 186.crafty 253.perlbmk 256.bzip2 197.parser 254.gap 300.twolf" in	      
	      assoc spec @$ map ((fun s -> (snd (ExtString.splitToPair '.' s),s))*@ExtString.strip) @$ ExtString.splitBlank dirs in
      sprintf "~/CPU2000/benchspec/%s/%s" (if isFloat spec then "CFP2000" else "CINT2000") (work spec)
(*    let remoteBase =     *)
  end   
module type SPEC = sig
    val isInt : string -> bool
    val isFloat : string -> bool  
(*    val remoteBase : string*)
  end   
  
module Exec (P: PARAM with type t = string option)
 = struct
  type command = string
  let execStr cmd =
    match P.p with
      | Some target -> sprintf "ssh %s %s" target (ExtString.quote cmd)
      | None -> cmd
  let exec cmd = ExtUnix.exec (execStr cmd)
  let pwd = "pwd"
  let cd dir = "cd " ^ dir
  let home = "echo $HOME"
  let ls = "ls"
  let cp src dest = sprintf "cp %s %s" src dest
  let kill n = sprintf "kill -9 %d" n
  let killall pattern = sprintf "killall -9 %s" pattern
  let exists pattern =
    ExtString.strip (exec (sprintf "ps x|grep %s|grep -v grep" pattern)) <> ""
  let clean pattern =
    sprintf "ps x|grep %s|grep -v grep|cut -d\" \" -f1|xargs kill -9" pattern
  let execClean ?(oc=stderr) pattern =
    while exists pattern do
      fprintf oc "%s\n" @$ exec (clean pattern)
    done
  
  let copyTo src remoteDest =
    let flag = if Sys.is_directory src then "-r" else "" in
    sprintf "scp %s %s %s:%s" flag src (fromSome P.p) remoteDest
  let copyFrom ?(isDir=false) remoteSrc dest =
    let flag = if isDir then "-r" else "" in
    sprintf "scp %s %s:%s %s" flag (fromSome P.p) remoteSrc dest
end

open Command
(*  let driveTestSpec ?(sendMail=false) path spec =                                    *)
(*    let phase i =                                                                    *)
(*      sprintf "-f loongcc_peak_tmp%d.cfg" i in                                       *)
(*    let target = Target.ofString (fromSome P.p)  in                                  *)
(*    let acc,host = Target.account target,Target.host target in                       *)
(*    cd path <+>                                                                      *)
(*    sprintf "./test_spec2k.sh -f loongcc_peak_tmp3.cfg  -l %s -a %s --run_host %s %s"*)
module ConfigFile = struct
  end
module Spec(P: PARAM with type t = string (* the base dir *) )(S: SPEC)(C: COMMAND) = struct
  open C
  open ExtString
  open ExtList
  let base spec =
    Filename.concat P.p @$
    (if S.isInt spec then "spec2000" else
      if S.isFloat spec then "spec2kfp" else
        failwith "base")
  let dir spec =
    Filename.concat (base spec) spec
(*  let remoteDir spec =*)
  let specFlag file spec =
    let s = ExtUnix.readFile @$ fromSome @$ ExtFilename.simplify file in
    let header, body = span (ExtChar.isUpper *@flip get 0) @$ cleanLines s in
    let assignmentsToAssocList l =
      map (fun s ->
              let a, b = tmap ExtString.of_list @$ break ((=) '=') @$ ExtString.to_list s in
              strip a, ExtString.drop 1 b) l in
    let assocListToAssignments l = map (fun (a, b) -> a ^ "=" ^ b) l in
    let base = assignmentsToAssocList header in
    let getSpec s = nthField 0 s in
    String.concat " " @$ filter (isSubStringOf ~needle:"FLAGS") @$
    assocListToAssignments @$ assoc spec @$
    map (fun l ->
            getSpec (hd l), AssocList.zipWith (^) base @$
            assignmentsToAssocList (tl l) ) @$ splitBy ((=) "}") body
  
  let clean spec = sprintf "make clean -C %s" (dir spec)
  let compile cfg spec =
    sprintf "make %s -C %s %s" spec (dir spec) (specFlag cfg spec)
  let runspec ?(cfg ="godson.cfg") ?(runtype ="test") n spec =
    cd "spec2000" <+>
    ofString @$ sprintf "source shrc && ulimit -s unlimited && runspec -I -c %s -n %d -i %s %s" cfg n runtype spec
end



(*let () =                                                                    *)
(*  print_endline @$ specFlag "~/2im/spec2000/bin/loongcc_peak_tmp3.cfg" "art"*)

(*                                    *)
(*module ExtSys(E:EXEC) = struct      *)
(*  open Sys                          *)
(*  open ExtList                      *)
(*  open ExtFilename                  *)
(*                                    *)
(*  include Sys                       *)
(*  end                               *)
(*                                    *)
(*class remote =                      *)
(*  fun target -> object              *)
(*  val target = target               *)
(*(*  method exec cmd =             *)*)
(*(*    ExtSys.remoteExec target cmd*)*)
(*  end                               *)
