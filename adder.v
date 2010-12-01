module system;
	reg [15:0] x;
	reg [15:0] y;
	reg [63:0] x2;
	reg [63:0] y2;
	reg clock;
	wire [15:0] z;
	wire [15:0] z2;
	wire [63:0] z3;
	wire cout;
	adder16 ad(.x(x),.y(y),.z(z),.c0(1'b0),.cout(cout));
	booth8 bo(.x(x[7:0]),.y(y[7:0]),.z(z2),.cout());  
	adder64 ad64(.x(x2),.y(y2),.z(z3),.c0(1'b0),.cout());
	initial #1000 $finish ;
	initial clock <= 1'b0;
	always #1 clock <= ~clock;
	initial begin
		//x <= 16'b1111111111111111;
		//x <= 16'b1110;
		//y <= 16'b1;
		x <= 16'b1011;
		y <= 16'b1011;
		x2 <= 64'b1111111111111111111111111111111111111111111111111111111111111111;
		y2 <= 64'b1;
	end
	always @(posedge clock) begin
		y <= y + 3;
		y2 <= y2 + 3;
	end
    initial begin
    	$dumpfile ("testbed.vcd");
       $dumpvars (0);
    end
endmodule

module carry4(p,g,c0,c,c4,P,G);
	input [3:0] p;
	input [3:0] g;
	input c0;
	output [3:0] c;
	output c4;
	output P;
	output G;
	
	assign c[0] = c0;
	assign c[1] = g[0] | p[0]&c0;
	assign c[2] = g[1] | p[1]&g[0] | p[1]&p[0]&c0;
	assign c[3] = g[2] | p[2]&g[1] | p[2]&p[1]&g[0]
		| p[2]&p[1]&p[0]&c0;
	assign c4 = g[3] | p[3]&g[2] | p[3]&p[2]&g[1] 
		| p[3]&p[2]&p[1]&g[0] | p[3]&p[2]&p[1]&p[0]&c0;
	assign P = p[0]*p[1]*p[2]*p[3];
	assign G = g[3] | p[3]&g[2] | p[3]&p[2]&g[1] | p[3]&p[2]&p[1]&g[0]; 
endmodule

module carry16(p,g,c0,c,c16,P,G);
	input [15:0] p;
	input [15:0] g;
	input c0;
	output [15:0] c;
	output c16;
	wire [3:0] bc;
	wire [3:0] P2;
	wire [3:0] G2;
	output P;
	output G;

	carry4 ca1(.p(p[3:0]),.g(g[3:0]),.c0(c0),.c(c[3:0]),.P(P2[0]),.G(G2[0]),.c4());
	carry4 ca2(.p(p[7:4]),.g(g[7:4]),.c0(bc[1]),.c(c[7:4]),.P(P2[1]),.G(G2[1]),.c4());
	carry4 ca3(.p(p[11:8]),.g(g[11:8]),.c0(bc[2]),.c(c[11:8]),.P(P2[2]),.G(G2[2]),.c4());
	carry4 ca4(.p(p[15:12]),.g(g[15:12]),.c0(bc[3]),.c(c[15:12]),.P(P2[3]),.G(G2[3]),.c4(c16));
	
	carry4 ca5(.p(P2),.g(G2),.c0(c0),.c(bc),.P(P),.G(G),.c4());
endmodule

module adder64(x,y,z,c0,cout);
	input [63:0] x;
	input [63:0] y;
	input c0;
	output [63:0] z;
	output cout;
	wire [63:0] p;
	wire [63:0] g;
	wire [63:0] c;
	wire [3:0] bc;
	wire [3:0] P;
	wire [3:0] G;
	assign p = x|y;
	assign g = x&y;
	assign z = x^y^c;
	carry16 ca1(.p(p[15:0]),.g(g[15:0]),.c0(c0),.c(c[15:0]),.P(P[0]),.G(G[0]),.c16());
	carry16 ca2(.p(p[31:16]),.g(g[31:16]),.c0(bc[1]),.c(c[31:16]),.P(P[1]),.G(G[1]),.c16());
	carry16 ca3(.p(p[47:32]),.g(g[47:32]),.c0(bc[2]),.c(c[47:32]),.P(P[2]),.G(G[2]),.c16());
	carry16 ca4(.p(p[63:48]),.g(g[63:48]),.c0(bc[3]),.c(c[63:48]),.P(P[3]),.G(G[3]),.c16(cout));
	
	carry4 ca5(.p(P),.g(G),.c0(c0),.c(bc),.P(),.G(),.c4());
endmodule

module adder16(x,y,z,c0,cout);
	input [15:0] x;
	input [15:0] y;
	input c0;
	output [15:0] z;
	output cout;
	wire [15:0] p;
	wire [15:0] g;
	wire [15:0] c;
	wire [3:0] bc;
	wire [3:0] P;
	wire [3:0] G;
	assign p = x|y;
	assign g = x&y;
	assign z = x^y^c;
	carry4 ca1(.p(p[3:0]),.g(g[3:0]),.c0(c0),.c(c[3:0]),.P(P[0]),.G(G[0]),.c4());
	carry4 ca2(.p(p[7:4]),.g(g[7:4]),.c0(bc[1]),.c(c[7:4]),.P(P[1]),.G(G[1]),.c4());
	carry4 ca3(.p(p[11:8]),.g(g[11:8]),.c0(bc[2]),.c(c[11:8]),.P(P[2]),.G(G[2]),.c4());
	carry4 ca4(.p(p[15:12]),.g(g[15:12]),.c0(bc[3]),.c(c[15:12]),.P(P[3]),.G(G[3]),.c4(cout));
	
	carry4 ca5(.p(P),.g(G),.c0(c0),.c(bc),.P(),.G(),.c4());
endmodule

module full_adder1 (a,b,c,s,cout);
	input a,b,c;
	output s,cout;
	assign s = a^b^c;
	assign cout = a&b | b&c | a&c;
endmodule

module half_adder1 (a,b,s,cout);
	input a,b;
	output s,cout;
	assign s = a^b;
	assign cout = a&b;
endmodule

module wallace5 (in,cin,cout,c,s);
	input [4:0] in;
	input [1:0] cin;
	output [1:0] cout;
	output c,s;
	wire s1, s2;
	
	full_adder1 fa1(.a(in[4]),.b(in[3]),.c(in[2]),.s(s1),.cout(cout[1]));
	full_adder1 fa2(.a(s1),.b(in[1]),.c(in[0]),.s(s2),.cout(cout[0]));
	full_adder1 fa3(.a(s2),.b(cin[1]),.c(cin[0]),.s(s),.cout(c));
endmodule

module transx(x,y,xout,cout);
	input [7:0] x;
	input [2:0] y;
	output [15:0] xout;
	output cout;
	
	assign xout = (y==3'b000|y==3'b111)?16'b0:
		(y==3'b001|y==3'b010)?{{9{x[7]}},x[6:0]}:
		(y==3'b011)?({{8{x[7]}},x[6:0],1'b0}):
		(y==3'b100)?(~{{8{x[7]}},x[6:0],1'b0}):
		~{{9{x[7]}},x[6:0]};
	assign cout = (y==3'b101|y==3'b110|y==3'b100); 
endmodule

module booth8 (x,y,z,cout);
	input [7:0] x;
	input [7:0] y;
	output [15:0] z;
	output cout;
	wire [15:0] xs[3:0];
	wire [3:0] c;
	wire [1:0] cs[14:0];
	wire [15:0] a;
	wire [15:0] b;
	transx tx1 (.x(x),.y({y[1:0],1'b0}),.xout(xs[0]),.cout(c[0]));
	transx tx2 (.x(x),.y(y[3:1]),.xout(xs[1]),.cout(c[1]));
	transx tx3 (.x(x),.y(y[5:3]),.xout(xs[2]),.cout(c[2]));
	transx tx4 (.x(x),.y(y[7:5]),.xout(xs[3]),.cout(c[3]));
	
	assign a[0] = c[0];
	wallace5 wa1 (.in({xs[0][0],1'b0,1'b0,1'b0,1'b0}),
	.cin(1'b0),.cout(cs[0]),.c(a[1]),.s(b[0]));
	wallace5 wa2 (.in({xs[0][1],1'b0,1'b0,1'b0,1'b0}),
	.cin(cs[0]),.cout(cs[1]),.c(a[2]),.s(b[1]));
	wallace5 wa3 (.in({xs[0][2],xs[1][0],c[1],1'b0,1'b0}), //
	.cin(cs[1]),.cout(cs[2]),.c(a[3]),.s(b[2]));
	wallace5 wa4 (.in({xs[0][3],xs[1][1],1'b0,1'b0,1'b0}),
	.cin(cs[2]),.cout(cs[3]),.c(a[4]),.s(b[3]));
	wallace5 wa5 (.in({xs[0][4],xs[1][2],xs[2][0],c[2],1'b0}),//
	.cin(cs[3]),.cout(cs[4]),.c(a[5]),.s(b[4]));
	wallace5 wa6 (.in({xs[0][5],xs[1][3],xs[2][1],1'b0,1'b0}),
	.cin(cs[4]),.cout(cs[5]),.c(a[6]),.s(b[5]));
	wallace5 wa7 (.in({xs[0][6],xs[1][4],xs[2][2],xs[3][0],c[3]}),//
	.cin(cs[5]),.cout(cs[6]),.c(a[7]),.s(b[6]));
	wallace5 wa8 (.in({xs[0][7],xs[1][5],xs[2][3],xs[3][1],1'b0}),
	.cin(cs[6]),.cout(cs[7]),.c(a[8]),.s(b[7]));
	wallace5 wa9 (.in({xs[0][8],xs[1][6],xs[2][4],xs[3][2],1'b0}),
	.cin(cs[7]),.cout(cs[8]),.c(a[9]),.s(b[8]));
	wallace5 wa10 (.in({xs[0][9],xs[1][7],xs[2][5],xs[3][3],1'b0}),
	.cin(cs[8]),.cout(cs[9]),.c(a[10]),.s(b[9]));
	wallace5 wa11 (.in({xs[0][10],xs[1][8],xs[2][6],xs[3][4],1'b0}),
	.cin(cs[9]),.cout(cs[10]),.c(a[11]),.s(b[10]));
	wallace5 wa12 (.in({xs[0][11],xs[1][9],xs[2][7],xs[3][5],1'b0}),
	.cin(cs[10]),.cout(cs[11]),.c(a[12]),.s(b[11]));
	wallace5 wa13 (.in({xs[0][12],xs[1][10],xs[2][8],xs[3][6],1'b0}),
	.cin(cs[11]),.cout(cs[12]),.c(a[13]),.s(b[12]));
	wallace5 wa14 (.in({xs[0][13],xs[1][11],xs[2][9],xs[3][7],1'b0}),
	.cin(cs[12]),.cout(cs[13]),.c(a[14]),.s(b[13]));
	wallace5 wa15 (.in({xs[0][14],xs[1][12],xs[2][10],xs[3][8],1'b0}),
	.cin(cs[13]),.cout(cs[14]),.c(a[15]),.s(b[14]));
	wallace5 wa16 (.in({xs[0][15],xs[1][13],xs[2][11],xs[3][9],1'b0}),
	.cin(cs[14]),.cout(),.c(),.s(b[15]));

	adder16 add(.x(a),.y(b),.c0(1'b0),.z(z),.cout(cout));
endmodule