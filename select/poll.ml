open Linda
let mtime f = try Some ((Unix.stat f).Unix.st_mtime) with _ -> None
let lastMtime = ref None
let () =
  let cmd t =
    lastMtime := Some t;
    Unix.sleep 3;
    ignore @$ Sys.command "cd /home/zsc/workspace/linda2/;make;make" in
  while true do
    (match !lastMtime,mtime "/home/zsc/workspace/linda2/linda.mli" with
      | None,Some t ->
        cmd t
      | Some t',Some t when t>t'->
        cmd t
      | _ -> ());
    Unix.sleep 3 
    done