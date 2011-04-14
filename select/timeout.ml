open Linda
let () =
  ExtUnix.withTimeout (float_of_string Sys.argv.(1)) (fun i -> Printf.printf "Terminated %d\n" i) @$
  (fun () -> ignore @$
        Sys.command (String.concat " " @$
            Array.to_list @$ ExtArray.drop 2 Sys.argv))