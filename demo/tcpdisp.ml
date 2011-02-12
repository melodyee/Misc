open Unix

let () =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  (* so we can restart our server quickly *)
  setsockopt server_sock SO_REUSEADDR true ;
  let address = (gethostbyname Sys.argv.(1)).h_addr_list.(0) in
  bind server_sock (ADDR_INET (address, int_of_string Sys.argv.(2))) ;
  (* Listen on the socket. Max of 10 incoming connections. *)
  listen server_sock 10 ;
  let n = 512 in
  let n2 = n*n in
  let len = 8192 in
  let str = String.make len ' ' in
  Graphics.open_graph (Printf.sprintf " %dx%d" n n);
  Graphics.set_window_title Sys.argv.(3);
  let (client_sock, client_addr) = accept server_sock in
  Printf.printf "connected\n"; 
  flush Pervasives.stdout;
	let buf = ref [] in
	let cnt = ref 0 in
	let rec disp l = match l with
	  | r::g::b::xs ->
	      Graphics.set_color (Graphics.rgb
	            (int_of_char r)
	            (int_of_char g)
	            (int_of_char b)
	        );
	      Graphics.plot (!cnt mod n) (!cnt/n);
	      incr cnt;
	      if (!cnt >= n2) then begin
	        cnt := 0;
	      end;
        disp xs
    | x -> x  in
  while true do
    let x = read client_sock str 0 len in
(*        Printf.printf "received %d\n" x;*)
(*        flush_all ();                   *)
    if !cnt =0 then Graphics.clear_graph ();
    for i = 0 to x-1 do
      buf := str.[i] :: !buf
    done;
    buf := List.rev (disp (List.rev !buf));
  done;
  shutdown client_sock SHUTDOWN_ALL
