open List
open Linda
open ExtList
open Printf

let points = [ 10.,10.
	; 100.,100.
	; 30.,40.
	; 50.,60.
	; 21.,12.
	; 234.,12.
	; 5.,98.
	; 4.,32.
	; 23.,32.
	]
	 
let naive l =
	let rec work acc = function
		| [] -> acc
		| [x] -> acc
		| x::x'::xs -> work (Point.distance x x' +. acc) (x'::xs) in
	work 0. @$ l@[hd l]

(*module HashMap		*)

let rand n l =
	naive @$ ExtList.minimumByMap compare naive @$ 
		map (fun _ -> Array.to_list @$ Permutaion.randomize (Array.of_list l)) (range 0 n)
	
let () =
	Random.self_init ();
	printf "%f\n" (naive points);
	printf "%f\n" (rand 10 points);
	printf "%f\n" (rand 100 points);
	print_intlist @$ Permutaion.to_list @$ Permutaion.crossover2 (Permutaion.of_list [3;2;1;0]) (Permutaion.of_list [1;3;2;0])
(*	[1;2;3;4;5;6;7;8] [8;7;6;5;4;3;2;1]*)
(*	printf "%s\n" Permutaion.show *)
(*	print_intlist @$ randPermutaion [1;2;3;4]*)