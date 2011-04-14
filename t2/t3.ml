open Printf
let () = 
  List.iter (fun i -> ignore (Sys.command
	   (sprintf "memtime ./a.out %d >t" i))) [16;128;1024;8192;65536;524288]
