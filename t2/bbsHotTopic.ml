open Printf

  let readLines ic =
    let l = ref [] in
    try
      while true do
        l := input_line ic :: !l
      done;
      failwith "readLines"
    with End_of_file -> List.rev !l

    
let () =
  Array.iteri (fun i e -> if i mod 9=0 then print_endline e) (Array.of_list (readLines stdin))
  