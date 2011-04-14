(*open List*)
open Array
open Linda
(*open ExtList*)
open Printf
open ExtArray

module type ExecType = sig
    val exec : ?copy:bool -> string -> int
    val execBin : string -> int
    val time : ?copy:bool -> string -> float option (* micro-seconds *)
    val timen : ?copy:bool -> int -> string -> float list
end
module RemoteExec (P:PARAM with type t = string): ExecType = struct
    let target = P.p
    let (<+>) a b = if ExtString.isBlankString a then b else a ^ " && " ^ b 
    let fetch fn =
        sprintf "scp %s:%s . && ssh %s \"rm %s\"" target fn target fn
    let execStr ?(copy=true) cmd =
        (if copy then sprintf "scp a.out %s: " target else "") <+> 
        sprintf "ssh %s \"%s\"" target cmd
    let exec ?(copy=true) cmd = 
        Sys.command (execStr ~copy cmd)
    let execBin bin = 
        let fn = ExtList.last (ExtString.split bin "/") in
        Sys.command (sprintf "scp %s %s: ; ssh %s ./%s; ssh %s 'rm %s'" bin target target fn target fn) 
    let time ?(copy=true) cmd =
        let ret = Sys.command (execStr ~copy cmd <+> fetch "RUNTIME") in
        if ret = 0 then begin
            let ic = open_in "RUNTIME" in
            let t = float_of_string (input_line ic) in
            close_in ic;
            Some t
        end else begin
            printf "ret = %d\n" ret;
            None
        end
    let timen ?(copy=true) n cmd =
        let times = ref [] in
        let ret = Sys.command ( execStr ~copy (sprintf "python runaout.py %d >/dev/null 2>&1" n) <+> fetch "RUNTIME" <+> fetch "RUNTIMES") in
        if ret = 0 then begin
            let ic = open_in "RUNTIMES" in
            try
                while true do
                    let t = float_of_string (input_line ic) in
                    addRefList t times
                done
            with End_of_file -> close_in ic
            (* printf "timen:%f\n" t; *)
        end
        else failwith "timen: bad return value";
        !times
end
module LocalExec:ExecType = struct
    let exec ?(copy=true) = Sys.command
    let execBin s = Sys.command ("./" ^ s)
    let time ?(copy=true) cmd =
        let ret = exec cmd in
        if ret = 0 then
            let ic = open_in "RUNTIME" in
            let t = float_of_string (input_line ic) in
            close_in ic;
            Some t
        else
            None
(*      Linda.time (fun () -> Sys.command "./a.out")*)
    let timen ?(copy=true) n cmd =
        let rec work n times =
            if n = 0 then times else
                let ret = exec cmd in
                if ret = 0 then begin
                    let ic = open_in "RUNTIME" in
                    let t = float_of_string (input_line ic) in
(*                  printf "timen:%f\n" t;*)
                    close_in ic;
                    work (n-1) (t :: times)
                end
                else failwith "timen: bad return value"
        in
        work n []
end

module M = RemoteExec(struct
    type t = string
    let p = Sys.argv.(2)
  end)
exception End  
let () =
  try
	  foreach (filter (not*@Sys.is_directory) @$ of_list @$ Tree.flatten (ExtUnix.list_dir Sys.argv.(1))) (fun s ->
	        fprintf stderr "%s\n" s;
	        flush stderr;
	        let ret = M.execBin (sprintf "./%s" s) in
	        fprintf stderr "ret:%d\n" ret;
	        fprintf stderr "-----------\n";
	        flush stderr;
	        if ret <> 0 then raise End;
(*	        ExtUnix.sleepf 0.1*)
	    )
   with End -> ()