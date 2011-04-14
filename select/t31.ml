open Printf
open Linda
open Algebra

module RunScore (R: R) = struct
  open ExtArray
  module R = Ring (R)
  type time = R.t
  type score =
    | Timeout of time
    | Complete of time
    | Wrong
  type t = score
  module O = OrdLtEq(struct
      type t = score
      let eq s s' = match s, s' with
        | Timeout t, Timeout t' -> R.eq t t'
        | Complete t, Complete t' -> R.eq t t'
        | Wrong, Wrong -> true
        | _ -> false
      let lt s s' = match s, s' with
        | Wrong, Timeout _
        | Wrong, Complete _ -> true
        | Timeout t, Timeout t' -> R.gt t t'
        | Timeout _, Complete _ -> true
        | Complete t, Complete t' -> R.gt t t'
        | _ -> false
    end)
  include O
  let pk n timeit a a' =
      Int.sign @$ IntArray.sum @$ init n (fun _ -> compare (timeit a) (timeit a'))
  let show = function
    | Timeout t -> sprintf "timeout(%s)" (R.show t)
    | Complete t -> sprintf "complete(%s)" (R.show t)
    | Wrong -> "wrong"
end
module Score = RunScore(Float)

module type EXEC = sig
(* val execString : ?copy:bool -> ?isDeleteRemote:bool -> string -> string *)
  val timeitBin : ?recordFileName: string -> ?timeout: int -> string -> Score.t
(* val nTimeitBin : ?copy:bool -> ?isDeleteRemote:bool ->                  *)
(* ?recordFileName:string -> ?timeoutCmd:string -> ?timeout:int -> int ->  *)
(* string -> Score.t list val moveFromRemote : string -> int               *)
end

let timeoutExec recordFileName timeout cmd =
  let result = ExtUnix.exec cmd in
  if List.mem "Terminated" (ExtString.lines result) then
    Score.Timeout (float timeout)
  else begin
    try
      let ic = open_in recordFileName in
      let t = float_of_string (input_line ic) in
      close_in ic;
      Score.Complete t
    with _ -> Score.Wrong
  end

module RemoteExec (P: PARAM with type t = string) = struct
  open ExtList
  let target = P.p
  open Command
  let copyFromRemote fn =
    sprintf "scp %s:%s ." target fn
  let copyToRemote fn =
    sprintf "scp %s %s:" fn target
  let deleteRemote fn =
    sprintf "ssh %s \"rm -f %s\"" target fn
  let execRemoteCmd cmd =
    sprintf "ssh %s \"%s\"" target cmd
  let execRemoteBin bin =
    execRemoteCmd ("\"./\"" ^ bin)
  let moveFromRemote fn = copyFromRemote fn <+> deleteRemote fn
  let execString ?(copy = true) ?(isDeleteRemote = false) bin =
    let fn = Filename.basename bin in
    (if copy then copyToRemote bin else "") <+>
    execRemoteBin fn <+>
    (if isDeleteRemote then deleteRemote fn else "")
  let remoteTimeitBin ?(copy = true) ?(isDeleteRemote = false)
      ?(recordFileName ="RUNTIME") ?(timeout = 3600000) bin =
    let fn = Filename.basename bin in
    timeoutExec recordFileName timeout
      ((if copy then copyToRemote bin else "") <+>
        deleteRemote recordFileName <+>
        execRemoteCmd (sprintf "./%s" fn) <+>
        (if isDeleteRemote then deleteRemote fn else "") <+>
        copyFromRemote recordFileName)
  let timeitBin ?(recordFileName ="RUNTIME") ?(timeout = 3600000) bin =
    remoteTimeitBin ~recordFileName ~timeout bin
  let nTimeitBin ?(recordFileName ="RUNTIME") ?(timeout = 3600000) n bin =
    (if n > 0 then [remoteTimeitBin ~recordFileName ~timeout bin] else []) @
    map (fun _ -> remoteTimeitBin ~copy:false ~recordFileName ~timeout bin) (range 0 (n - 1))
end
module LocalExec = struct
  open ExtList
  let timeitBin ?(recordFileName ="RUNTIME") ?(timeout = 3600000) bin =
    timeoutExec recordFileName timeout (sprintf "timeout %d 100000 ./%s" timeout bin)
  let nTimeitBin ?(recordFileName ="RUNTIME") ?(timeout = 3600000) n bin =
    map (fun _ -> timeitBin ~recordFileName ~timeout bin) (range 0 n)
end

module LE = LocalExec
module RE = RemoteExec(struct type t = string let p = "root@10.3.0.182" end)
(*let () =                                                           *)
(*  printf "%s\n" @$ show_list Score.show @$ RE.nTimeitBin 30 "a.out"*)
(*    printf "%s\n" @$ ExtUnix.exec "ssh root@10.3.0.182 \"timeout 3600 :1 ./a.out\""*)

(*let () = List.length "abc"*)

(*let () = print_endline "Hello World"*)

class point x y = object
    val x : int = x
    val y : int = y
    method getX = x
    method getY = y
end