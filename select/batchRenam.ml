open Linda
(*open List*)
open ExtArray
let () =
(*    Array.iter print_endline @$*)
    if length Sys.argv<3 then
      Printf.printf "%s oldSuffix newSuffix\n" Sys.argv.(0)
    else begin
      let old = Sys.argv.(1) and new_ = Sys.argv.(2) in
        iter (fun s ->
          if Filename.check_suffix s old then
          ignore @$ Sys.command @$ Printf.sprintf "mv %s %s" s (Filename.chop_extension s ^ new_)) @$ 
        filter (not*@Sys.is_directory) @$ of_list @$ Tree.flatten (ExtUnix.list_dir Filename.current_dir_name)
    end