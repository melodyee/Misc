(*open List*)
open Array
open Linda
(*open ExtList*)
open Printf
open ExtArray

let () =
	let n = 10000 in
	let oc = open_out "icache.c" in
	output_string oc @$ String.concat "\n" @$ [
		"#include<stdio.h>"
		;"#include\"testlib.h\""
		;sprintf "#define N %d" n
		;sprintf "int a[%d] = { %s };" n @$ String.concat "," @$ to_list @$ map string_of_int @$ range 0 n
		;sprintf "int b[%d] = { %s };" n @$ String.concat "," @$ to_list @$ map string_of_int @$ range 0 n
		;"int main(){"
		;"int i,s=0;"
		;"resetTimer();"
		;"for(i=0;i<N;i++){"
		;"s+=a[i]*b[i];"
		;"}"
		;"printElapsedTime(\"icache\",0);"
		;"printf(\"sum = %d\\n\",s);"
		;"return 0;"
		;"}"
		;""];
	close_out oc
