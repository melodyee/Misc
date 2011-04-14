open Linda
open Printf
(*open Array                         *)
(*(*type bitSet = int * int list 	*)*)
(*                                   *)
(*type ary = Ary of n                *)
(*let of_array a =                   *)
(*	map (fun v -> fst v ) a          *)

(*let () =                        *)
(*	let bs = new digitSet 28 10 in*)
(*	print_intarray @$ bs#to_array;*)
(*	bs#set 2 max_int;             *)
(*	printf "%d\n" (bs#get 2);     *)
(*	print_endline @$ bs#show;     *)
(*	print_intarray @$ bs#to_array;*)

(*module LinearProg = struct                                                        *)
(*	open Array                                                                      *)
(*	(* [am] is the coefficient matrix. [bv] is the bounds. [cv] is the cost vector*)*)
(*	let solve am bv cv =                                                            *)
(*		let m,n = dims am in                                                          *)
(*		assert (length bv = m && length cv =n);                                       *)
(*		                                                                              *)
(*end                                                                               *)

(*open Glpk                                                                                                *)
(*let () =                                                                                                 *)
(*  let lp = make_problem Maximize                                                                         *)
(*       [|10.; 6.; 4.|]                                                                                   *)
(*       [|                                                                                                *)
(*         [|1.; 1.; 1.|];                                                                                 *)
(*         [|10.; 4.; 5.|];                                                                                *)
(*         [|2.; 2.; 6.|]                                                                                  *)
(*       |]                                                                                                *)
(*       [| -.infinity, 100.; -.infinity, 600.; -.infinity, 300. |]                                        *)
(*       [| 0., infinity; 0., infinity; 0., infinity|] in                                                  *)
(*(*		set_class lp Mixed_integer_prog;*)                                                                 *)
(*(*		set_col_kind lp 1 Integer_var;  *)                                                                 *)
(*    scale_problem lp;                                                                                    *)
(*(*    use_presolver lp true;*)                                                                           *)
(*		simplex lp;                                                                                          *)
(*(*		interior lp;*)                                                                                     *)
(*(*    branch_and_bound lp;*)                                                                             *)
(*    let prim = get_col_primals lp in                                                                     *)
(*      Printf.printf "Z: %g    x0: %g    x1: %g    x2: %g\n%!" (get_obj_val lp) prim.(0) prim.(1) prim.(2)*)

(*let () =                                        *)
(*	let ma = [|                                   *)
(*	[|1.;2.|];                                    *)
(*	[|4.;5.|]                                     *)
(*	|] in                                         *)
(*	printf "%s\n" (FM.show ma);                   *)
(*	printf "%s\n" (FM.show @$ FM.gaussianElim ma);*)
(*	printf "%s\n" (FM.show @$ FM.inv ma);         *)
	
(*module DS = DigitSet	             *)
(*                                   *)
(*let () =                           *)
(*	let bs = DS.create 28 10 in      *)
(*	print_intarray @$ DS.to_array bs;*)
(*	DS.set bs 2 max_int;             *)
(*	printf "%d\n" (DS.get bs 2);     *)
(*	print_endline @$ DS.show bs;     *)
(*	print_intarray @$ DS.to_array bs;*)

(*LET () =                                                                                                                      *)
(*	LET BL = (EXTLIST.RANGE 0 1000000) IN                                                                                       *)
(*(*	LET F () =                                                                                               *)               *)
(*(*		LET L = REF [] IN                                                                                      *)               *)
(*(*		PRINT_INTLIST @$ EXTLIST.TAKE 10 @$ LIST.REV @$ !(LIST.FOLD_LEFT (FUN L A -> ADDREFLIST A L;L) L BL) IN*)               *)
(*(*	PRINTF "%F\N" @$ TIME F;                                                                                 *)               *)
(*	LET F2 () =                                                                                                                 *)
(*		PRINT_INTARRAY @$ EXTARRAY.TAKE 10 @$ FIFO2.TO_ARRAY (LIST.FOLD_LEFT (FUN Q A -> FIFO2.PUSH A Q;Q) (FIFO2.EMPTY ()) BL) IN*)
(*	PRINTF "%F\N" @$ TIME F2;;                                                                                                  *)
 
(*let () =                            *)
(*	let q = FIFO2.empty () in         *)
(*	FIFO2.push 5 q;                   *)
(*	FIFO2.push 3 q;                   *)
(*	FIFO2.push 2 q;                   *)
(*	FIFO2.push 4 q;                   *)
(*(*	ignore @$ FIFO2.to_array q*)    *)
(*	print_intarray @$ FIFO2.to_array q*)
	
(*                                                                               *)
(*module FIFO = struct                                                           *)
(*	type 'a t = {                                                                *)
(*		mutable cap : int;                                                         *)
(*		mutable i : int;                                                           *)
(*		mutable i' : int;                                                          *)
(*		mutable d : 'a array array                                                 *)
(*	}                                                                            *)
(*(*	let init n f = {    *)                                                     *)
(*(*		cap = n;          *)                                                     *)
(*(*		idx = n;          *)                                                     *)
(*(*		d = Array.init n f*)                                                     *)
(*(*	}                   *)                                                     *)
(*	let empty = {                                                                *)
(*			cap = 0;                                                                 *)
(*			idx = 0;                                                                 *)
(*			d = [||]                                                                 *)
(*		}                                                                          *)
(*(*	let mapi f da = {                   *)                                     *)
(*(*		da with d = Array.mapi f da.d     *)                                     *)
(*(*	}                                   *)                                     *)
(*(*	let iteri f da = ignore @$ mapi f da*)                                     *)
(*(*	let map f da = mapi (fun i -> f) da *)                                     *)
(*(*	let iter f da = ignore @$ map f da  *)                                     *)
(*	let enlargeToAtLeast da n =                                                  *)
(*		while n>= da.cap do                                                        *)
(*			let size = 1 lsl (da.i+1) in                                             *)
(*			da.cap <- da.cap + size;                                                 *)
(*			da.d <-                                                                  *)
(*		done                                                                       *)
(*		                                                                           *)
(*		let f = ref (float da.cap) in                                              *)
(*		while int_of_float (!f) < n do                                             *)
(*			f := !f *. 1.5 +. 1.                                                     *)
(*		done;                                                                      *)
(*		let n' = int_of_float !f in                                                *)
(*		let ocap = da.cap in                                                       *)
(*		da.cap <- n';                                                              *)
(*		da.d <- Array.init n' (fun i -> if i < ocap then da.d.(i) else Obj.magic 0)*)
(*	let push d da =                                                              *)
(*		enlargeToAtLeast da (da.idx+1);                                            *)
(*		da.d.(da.idx) <- d;                                                        *)
(*		da.idx <- da.idx + 1                                                       *)
(*end                                                                            *)
(*let () =                                                                       *)
(*(*	let da = DynArray.init 3 (const "a") in*)                                  *)
(*(*	printf "%s\n" @$ *)                                                        *)
open Algebra
module FM = FloatMatrix
open ExtArray
let simplex ?(debug=false) p b c= 
	let cnt = ref 0 in
	let max_iter = ref 10 in
	let rec work p mb b c cb =
		if debug then printf "-----%d-----\n" !cnt;
		if !cnt >= !max_iter then
			failwith "max_iter exceeded" 
		else begin 
			incr cnt;
			if debug then begin
				printf "%s\n\n" @$ FM.show p;
				printf "%s\n\n" @$ FM.show mb;
				printf "%s\n" @$ show_floatarray b;
				printf "%s\n" @$ show_floatarray c;
				printf "%s\n" @$ show_floatarray cb;
				printf "------\n";
			end;
			let mbi = (FM.inverse mb) in
			let y = FM.transpose @$ FM.mul mbi (FM.col_vector cb) in
			if debug then begin
				printf "%s\n\n" @$ FM.show mbi;
				printf "%s\n\n" @$ FM.show y;
			end;
			let t = map (fun i -> 
				(FloatArray.dotProduct y.(0) p.(i) -. c.(i),i)) (range 0 (length p)) in
			let tj,j = minimum t in
			if debug then printf "%f,%d\n" tj j;
			let xb = FM.mul (FM.row_vector b) mbi in
			if tj < 0. then begin
				if debug then begin
					printf "%s\n\n" @$ FM.show (FM.col_vector b);
			(*		let xb = FM.mul mbi (FM.col_vector b) in*)
					printf "%s\n\n" @$ FM.show xb;
				end; 
		(*		let aj = FM.mul mbi (FM.col_vector p.(j)) in*)
				let aj = FM.mul (FM.row_vector p.(j)) mbi in
		(*		let t = map (fun i -> xb.(i).(0) /. aj.(i).(0),i) (range 0 (length mb)) in*)
				let t = ExtArray.filter (fun (a,b) -> a>0.) @$ 
					map (fun i -> xb.(0).(i) /. aj.(0).(i),i) (range 0 (length mb)) in
				assert (t<>[||]);
				let _,r = minimum t in
				if debug then begin
					printf "%s\n" @$ show_floatarray (map fst t);
					printf "%d\n" r;
				end;
				let t = mb.(r) in
				mb.(r) <- p.(j);
				p.(j) <- t;
				let t = cb.(r) in
				cb.(r) <- c.(j);
				c.(j) <- t; 
		(*	printf "%s\n\n" @$ FM.show p;      *)
		(*	printf "%s\n\n" @$ FM.show mb;     *)
		(*	printf "%s\n" @$ show_floatarray b;*)
		(*	printf "%s\n" @$ show_floatarray c;*)
		(*	printf "------\n";                 *)
				work p mb b c cb
			end else begin
				if debug then begin 
					printf "cb = %s\n" @$ show_floatarray cb;
					printf "xb = %s\n" @$ FM.show xb;
				end;
				FloatArray.dotProduct cb (xb.(0))
			end
		end in
	let mb = FM.eye (length p.(0)) in
	let cb = make (length p.(0)) 0. in
	work p mb b c cb
let () =
	printf "%f\n" @$
	simplex ~debug:false [|[|1.;0.;3.|];
		[|0.;2.;2.|];
		|] [|4.;12.;18.|] [|3.;5.;0.|];
(*	printf "%f\n" @$                                         *)
(*	simplex ~debug:true [|[|1.;-1.;1.|];                     *)
(*		[|1.;-1.;-2.|];                                        *)
(*		[|-1.;1.;2.|];                                         *)
(*		|] (FM.eye 3) [|7.;-7.;4.|] [|2.;-3.;3.|] [|0.;0.;0.|];*)
	printf "%f\n" @$
	simplex ~debug:false [|[|1.;2.;4.|];
		[|1.;2.;1.|];
		[|3.;5.;2.|];
		|] [|30.;24.;36.|] [|3.;1.;2.|];
(*	printf "%f\n" @$                             *)
(*	simplex ~debug:true [|[|2.;-5.;-3.|];        *)
(*		[|-8.;-2.;5.|];                            *)
(*		[|0.;0.;-10.|];                            *)
(*		[|-10.;0.;2.|];                            *)
(*		|] [|-50.;-100.;-25.|] [|-1.;-1.;-1.;-1.|];*)


open ExtList 
(*let () =                                                  *)
(*	iter print_intlist @$ packBy even [1;2;3;2;1;5;3;4;5;6] *)

(*let () =                                                           *)
(*	iter print_intlist @$ groupBy (==) [1;2;3;2;2;2;1;1;3;4;5;66;66] *)
		
(*let rabinkarp needle hay =*)
			