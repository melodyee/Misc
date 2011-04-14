(*open List*)
open Array
open Linda
(*open ExtList*)
open Printf
open ExtArray

module type ExecType = sig
	val exec : ?copy:bool -> string -> int
	val time : ?copy:bool -> string -> float option (* micro-seconds *)
	val timen : ?copy:bool -> int -> string -> float list
end
module RemoteExec: ExecType = struct
	let target = "root@10.3.0.182"
	let (<+>) a b = if ExtString.isBlankString a then b else a ^ " && " ^ b 
	let fetch fn =
		sprintf "scp %s:%s . && ssh %s \"rm %s\"" target fn target fn
	let execStr ?(copy=true) cmd =
		(if copy then sprintf "scp a.out %s: " target else "") <+> 
		sprintf "ssh %s \"%s\"" target cmd
	let exec ?(copy=true) cmd = 
		Sys.command (execStr ~copy cmd)
	let time ?(copy=true) cmd =
		let ret = Sys.command (execStr ~copy cmd <+> fetch "RUNTIME") in
		if ret = 0 then begin
			let ic = open_in "RUNTIME" in
			let t = float_of_string (input_line ic) in
			close_in ic;
			Some t
		end else begin
			printf "ret = %d\n" ret;
			None
		end
	let timen ?(copy=true) n cmd =
		let times = ref [] in
		let ret = Sys.command ( execStr ~copy (sprintf "python runaout.py %d >/dev/null 2>&1" n) <+> fetch "RUNTIME" <+> fetch "RUNTIMES") in
		if ret = 0 then begin
			let ic = open_in "RUNTIMES" in
			try
				while true do
					let t = float_of_string (input_line ic) in
					addRefList t times
				done
			with End_of_file -> close_in ic
			(* printf "timen:%f\n" t; *)
		end
		else failwith "timen: bad return value";
		!times
end
module LocalExec:ExecType = struct
	let exec ?(copy=true) = Sys.command
	let time ?(copy=true) cmd =
		let ret = exec cmd in
		if ret = 0 then
			let ic = open_in "RUNTIME" in
			let t = float_of_string (input_line ic) in
			close_in ic;
			Some t
		else
			None
(*		Linda.time (fun () -> Sys.command "./a.out")*)
	let timen ?(copy=true) n cmd =
		let rec work n times =
			if n = 0 then times else
				let ret = exec cmd in
				if ret = 0 then begin
					let ic = open_in "RUNTIME" in
					let t = float_of_string (input_line ic) in
(*					printf "timen:%f\n" t;*)
					close_in ic;
					work (n-1) (t :: times)
				end
				else failwith "timen: bad return value"
		in
		work n []
end

let standardDeviation l = sqrt (Algebra.FloatList.variance l)
let median l = List.nth (List.sort compare l) (List.length l /2)

module MeasureTime (Exec:ExecType) = struct
	let decideRuns ?(budget=1.0) cmd minRuns verifyRuns ratio =
		let stability runs =
			let l = List.map (fun _ -> median (Exec.timen ~copy:false runs cmd)) (ExtList.range 0 verifyRuns) in
			printf "%s" @$ show_floatlist l;
			standardDeviation l /. median l in
		let ret = ExtUnix.withTimeout (budget /. float minRuns)
				(fun i -> printf "Fail with signal %d" i;-1) (fun () -> Exec.exec cmd) in
		if ret <> 0 then (printf "Ret = %d;Check return value and budget.\n" ret;None) else begin
			let approxTime = fromSome @$ Exec.time ~copy:false cmd in
	(*		printf "approxTime %f\n" approxTime;*)
			let maxRuns = budget *. 1e6 /. approxTime in
	(*		printf "maxRuns %f\n" maxRuns;*)
			let rec work runs =
				let ratio' = stability (int_of_float runs) in
				if runs < maxRuns && ratio' > ratio then work (1.2*.runs) else begin
					printf "stability ratio = %f\n" ratio';
					int_of_float runs 
				end in
			Some (work (float minRuns))
		end
end
(*module M = MeasureTime(RemoteExec)                                           *)
(*let () =                                                                     *)
(*	print_floatlist @$ RemoteExec.timen 3 "./a.out >/dev/null 2>&1" ;          *)
(*(*(*	printf "%f\n" @$ fromSome @$ RemoteExec.time "./a.out" ;*)           *)*)
(*	match M.decideRuns ~budget: 5.0 "./a.out >/dev/null 2>&1" 3 10 0.05 with   *)
(*	| Some i -> printf "runs = %d\n" i                                         *)
(*	| None -> printf "error\n"                                                 *)

let dotp = "#include<stdio.h>
#include\"testlib.h\"
#define N 2000000
#define TYP double
TYP dotproduct(TYP* a,TYP* b,int n){
  int i=0; 
  TYP sum=0;
  TYP sum1=0;
  TYP sum2=0;
  TYP sum3=0;
	TYP * pa = a;
	TYP * pb = b;
  for (i = 0; i < n/4; ++i) {
    sum+=a[4*i]*b[4*i];
    sum1+=a[4*i+1]*b[4*i+1];
    sum2+=a[4*i+2]*b[4*i+2];
    sum3+=a[4*i+3]*b[4*i+3];
#if 0		
		__asm__ (
				\"lbu $0,K(%0) \\n\\t\"
				:
				: \"r\" (pa)
		);
		__asm__ (
				\"lbu $0,K(%0) \\n\\t\"
				:
				: \"r\" (pb)
		);
		pa +=4;
		pb +=4;
#endif		
  } 
  i=4*i;
  for (; i < n; ++i) {
    sum+=a[i]*b[i];
  }
  sum+=sum1+sum2+sum3;
  return sum;
}
TYP a[N];
TYP b[N];
int main() {
	int i;
	init(a,N);
	init(b,N);
	resetTimer();
	TYP s = dotproduct(a,b,N);
	printElapsedTime(\"dotproduct\", 0);
	printf(\"s = %f\\n\", s);
	return 0;
}
"

let stride = "/*
 * stride.c
 *
 *  Created on: Jun 24, 2009
 *      Author: zsc
 */
#include<stdio.h>
#include\"testlib.h\"
#define N 2000000
#ifndef M
#define M 1
#endif
int* a[N];
int main() {
	int i;
	for (i = 0; i < N; ++i) {
		a[i] = &a[(i + M) % N];
	}
	resetTimer();
	int* p = a[0];
	for (i = 0; i < N / 4; i++) {
		p = p[0];
		p = p[0];
		p = p[0];
		p = p[0];
		__asm__ (
				\"lbu $0,K(%0) \\n\\t\"
				:
				: \"r\" (p)
		);

	}
	printElapsedTime(\"stride\", 0);
	printf(\"p = 0x%x\\n\", p);
	return 0;
}
"

let genC s fn k =
	let oc = open_out fn in
	output_string oc (ExtString.replace s "K" (string_of_int k));
	close_out oc

let mylgcc2 = "loongcc -O3 -Wb,-WOPT:use_ld_st_offset=1 -static -loongson2f -Wb,-CG:float_use_madd -CG:use_loongson2e_multdivmod "
let mylgcc = mylgcc2 ^ " -ipa "
let stgcc = "mips64el-st-linux-gnu-gcc -O3 -march=loongson2f -mtune=loongson2f -static"
let () = 
	let local = false in
	let l = ExtList.exprangef 4 5 1.2 in
(*	let l = ExtList.range2 42 88 2 in*)
	ExtList.foreach l @$ fun i ->
(*		genC stride "stride.c" i; *)
		genC dotp "stride.c" i;
		let cmd = sprintf "%s -DM=1 stride.c testlib.c >/dev/null 2>&1" 
			(if local then "gcc" else mylgcc)  in
		let ret = Sys.command cmd in
		if ret =0 then begin
			let t = median @$ 
				(if local then LocalExec.timen else RemoteExec.timen) 5 "./a.out >/dev/null 2>&1" in
			if false then printf "stride:%d, time:%.3f\n" i t else
				printf "%d\t%.3f\n" i t;
			flush stdout
		end else
			failwith (sprintf "%d" ret) 