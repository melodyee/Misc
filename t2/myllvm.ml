open Printf
let x cmd =
  eprintf "%s\n" cmd;
  flush stderr;
  Sys.command cmd
let (>>=) f g =
  match f with
    | 0 -> g ()
    | x -> x  
let () =
  let base = Filename.chop_extension Sys.argv.(1) in
  eprintf "ret = %d\n" begin
  x (sprintf "llvmc --emit-llvm -S -O4 %s.c -o %s.ll" base base) >>= fun _ ->
  x (sprintf "llvm-as %s.ll -o %s.bc" base base) >>= fun _ ->
  x (sprintf "llc -O0 -mcpu=r6000 -march=mips -mattr=+o32 %s.ll -o %s.s" base base) >>= fun _ ->
  x (sprintf "mipsel-unknown-linux-gnu-gcc -mips3 %s.s -o %s" base base) >>= fun _ ->
  x (sprintf "cp %s a.out" base)
  end