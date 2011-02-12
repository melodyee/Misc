ip = 10.3.0.29
comp:cswim cray
cswim:
	gcc -O2 test.c -lglut -lm -o swim
cray:
	ocamlopt -g unix.cmxa graphics.cmxa tcpdisp.ml -o ray
goswim:
	-./swim 1027 "Loongcc peak" &
	-./swim 1029 "GCC peak"&
goray:
	-./ray ${ip} 1037 "GCC peak:ray tracing" &	
	-./ray ${ip} 1039 "Loongcc peak:ray tracing" &
go:goswim goray
	
