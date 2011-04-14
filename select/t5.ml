open Linda
open ExtUnix
let uptime targ =
	let r = ref "" in
	timeout 3.0 (fun () ->
		r := exec (Printf.sprintf "ssh %s \"uptime\"" targ));
	try Some (List.hd @$ List.tl @$ ExtString.split "average:" !r) with _ -> None
(*let ssh targ =                                                *)
(*	let ic,oc,ic' = Unix.open_process_full ("ssh "^targ) [||] in*)
			
let () =
	let targ = try Sys.argv.(1) with _ -> "root@10.3.0.182" in
	let logfn = try Sys.argv.(2) with _ -> "myloads" in
	let oc = open_out_gen [Open_creat;Open_append;Open_binary] 0o644 logfn in
	try
		while true do
			let f = Unix.time () in
			let cmd = Printf.sprintf "ssh %s \"uptime\"" targ in
			let r = exec cmd in
			Printf.fprintf oc "%s %s uptime:%s\n" (ppTime f) targ r;
			flush oc;
(*			Printf.printf "%s %s uptime:%s\n" (ppTime f) targ r;*)
(*			flush stdout;*)
			Unix.sleep 10
		done
	with _ -> close_out oc