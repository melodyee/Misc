`timescale 1ns / 10ps
module system;
	reg clock, reset;
	cpu cpu00(.clock(clock), .reset(reset));
	initial clock <= 1'b0;
	initial #100000 $finish ;
	always #20 clock <= ~clock;
	initial begin
		#0 reset <= 1'b1;
		#70 reset <= 1'b0;
	end
    initial begin
    	$dumpfile ("testbed.vcd");
       $dumpvars (0);
    end
endmodule


module cpu(clock, reset);
	input clock;
	input reset;
	
	wire[17:0] brbus;
	wire[15:0] inst;
	wire[2:0] ex_dest, mem_dest, wb_dest;
	wire[19:0] wbbus;
	wire[55:0] idbus;
	wire[39:0] exbus;
	wire[39:0] membus;
	
	fetch_module fetch(.clock(clock), .reset(reset), .brbus(brbus), .inst(inst));
	
	decode_module decode(.clock(clock), .reset(reset), .inst(inst),
		.ex_dest(ex_dest), .mem_dest(mem_dest), .wb_dest(wb_dest),
		.wbbus(wbbus), .brbus(brbus), .idbus(idbus));
	
	alu_module alu(.clock(clock), .reset(reset),
		.idbus(idbus), .exbus(exbus), .ex_dest(ex_dest));
	
	mem_module mem(.clock(clock), .reset(reset),
		.exbus(exbus), .membus(membus), .mem_dest(mem_dest));
	
	wb_module wb(.clock(clock), .reset(reset),
		.membus(membus), .wbbus(wbbus), .wb_dest(wb_dest));

endmodule

`timescale 1ns / 10ps
module fetch_module(clock, reset, brbus, inst);
	input clock;
	input reset;
	input[17:0]brbus;
	output[15:0]inst;
	
	reg[15:0] pc;
	
	wire brbus_valid;
	wire brbus_taken;
	wire[15:0] brbus_offset;
	
	assign brbus_valid = brbus[17];
	assign brbus_taken = brbus[16];
	assign brbus_offset = brbus[15:0];
	
	// -- Enter your statements here -- //
	wire [11:0] addr;
	wire [15:0] next_pc;
	assign addr = pc[11:0];
	assign next_pc = pc + (brbus_taken?brbus_offset:16'b10);
	rom rom1(.raddr(addr),.rout(inst));
	always @(posedge clock) begin
		if (reset) begin
			pc <= 16'b0;
		end else begin 
			if (brbus_valid) begin
				pc <= next_pc;
			end
		end
	end
endmodule

`timescale 1ns / 10ps
module decode_module(clock, reset, inst, ex_dest, mem_dest, wb_dest, wbbus, brbus, idbus);
	input clock;
	input reset;
	input[15:0]inst;
	input[2:0]ex_dest, mem_dest, wb_dest;
	input[19:0]wbbus;
	output[55:0]idbus;
	output[17:0]brbus;
	reg[15:0] ir;
	
	wire wbbus_valid;
	wire[2:0] wbbus_dest;
	wire[15:0] wbbus_value;
	
	wire idbus_valid;
	wire[3:0] idbus_op;
	wire[2:0] idbus_dest;
	wire[15:0] idbus_value1;
	wire[15:0] idbus_value2;
	wire[15:0] idbus_stvalue;
	
	wire brbus_valid;
	wire brbus_taken;
	wire[15:0] brbus_offset;
	
	assign wbbus_valid = wbbus[19];
	assign wbbus_dest = wbbus[18:16];
	assign wbbus_value = wbbus[15:0];
	
	// -- Enter your statements here -- //
	wire rout1_ez;
	wire rout1_lz;
	wire rout1_lez;
	wire raw;
	wire use_imm;
	wire is_branch;
	wire is_store;
	wire [3:0] op;
	wire [2:0] cond;
	wire invalid_inst;
	wire invalid_branch;
	wire [2:0] rs1;
	wire [2:0] rs2;
	wire [15:0] rout1;
	wire [15:0] rout2;
	
	assign raw = rs1!=3'b0 & (wb_dest==rs1 | ex_dest==rs1 | mem_dest==rs1) |
		rs2!=3'b0 & (wb_dest==rs2 | ex_dest==rs2 | mem_dest==rs2) ;
	assign use_imm = ir[15] && (ir[14] || ir[13] || ir[12]) ;
	assign op = raw?4'b0:ir[15:12];
	assign invalid_branch = is_branch & (cond[2] | cond[1] & cond[0]); 
	assign invalid_inst = ir[15:12]==4'b0
		|| (op[3] & op[2] & (op[1:0]!=2'b0))
		|| invalid_branch
		|| (~op[3] & (ir[2:0]!=3'b0))
		|| ((op==4'b1000) & (ir[2:0]!=3'b0));
	assign idbus_valid = (~op[3]&(op[2]|op[1]|op[0]))
		|(op[3]&~op[2]&~op[1])
		|(~op[2]&op[1]&~op[0]);
	assign is_branch = op==4'b1100;
	assign is_store = op==4'b1011;
	assign rs1 = ir[8:6];
	assign rs2 = is_store?ir[11:9]:use_imm?0:ir[5:3];
	regfile regs(.clock(clock), .raddr1(rs1), .rout1(rout1), 
		.raddr2(rs2), .rout2(rout2), 
		.wen(wbbus_valid), .waddr(wbbus_dest), .win(wbbus_value));
	assign idbus_op = op;
	assign idbus_dest = (raw|is_store|is_branch)?3'b0:ir[11:9];
	assign idbus_value1 = rout1;
	assign idbus_value2 = use_imm?{{10{ir[5]}},ir[5:0]}:rout2;
	assign idbus_stvalue = rout2;
	assign rout1_ez = rout1==16'b0;
	assign rout1_lz = rout1[15];
	assign rout1_lez = rout1_ez | rout1_lz;
	assign cond = ir[11:9];
	assign brbus_valid = ~raw & ~invalid_inst;
	assign brbus_taken = is_branch & (cond==3'b0 & rout1_ez |
		cond==3'b1 & ~rout1_lez |
		cond==3'b10 & rout1_lez);
	assign brbus_offset = {{9{ir[5]}},ir[5:0],1'b0};
	always @(posedge clock) begin
		if (reset) begin
			ir <= 16'b0;
		end else begin
			if (!raw) begin
				ir <= inst;
			end
		end
	end
	
	assign brbus[17] = brbus_valid;
	assign brbus[16] = brbus_taken;
	assign brbus[15:0] = brbus_offset;
	
	assign idbus[55] = idbus_valid;
	assign idbus[54:51] = idbus_op;
	assign idbus[50:48] = idbus_dest;
	assign idbus[47:32] = idbus_value1;
	assign idbus[31:16] = idbus_value2;
	assign idbus[15:0] = idbus_stvalue;

endmodule

`timescale 1ns / 10ps
module alu_module(clock, reset, idbus, exbus, ex_dest);
	input clock;
	input reset;
	input[55:0]idbus;
	output[39:0]exbus;
	output[2:0]ex_dest;
	
	wire idbus_valid;
	wire[3:0] idbus_op;
	wire[2:0] idbus_dest;
	wire[15:0] idbus_value1;
	wire[15:0] idbus_value2;
	wire[15:0] idbus_stvalue;
	
	wire exbus_valid;
	wire[3:0] exbus_op;
	wire[2:0] exbus_dest;
	wire[15:0] exbus_exresult;
	wire[15:0] exbus_stvalue;
	
	assign idbus_valid = idbus[55];
	assign idbus_op = idbus[54:51];
	assign idbus_dest = idbus[50:48];
	assign idbus_value1 = idbus[47:32];
	assign idbus_value2 = idbus[31:16];
	assign idbus_stvalue = idbus[15:0];
	
	// -- Enter your statements here -- //
	reg valid;
	reg [3:0] op;
	reg [2:0] dest;
	reg [15:0] value1;
	reg [15:0] value2;
	reg [15:0] stvalue;
	wire [15:0] add_result;
	wire [15:0] sub_result;
	wire [15:0] and_result;
	wire [15:0] or_result;
	wire [15:0] not_result;
	wire [15:0] sl_result;
	wire [15:0] sr_result;
	wire [15:0] sru_result;
	
	assign exbus_valid = valid;
	assign exbus_op = op;
	assign exbus_dest = dest;
	assign ex_dest = dest;
	assign exbus_exresult = (op==4'b1|op==4'b1001|op[3:1]==3'b101)?add_result:
				(op==4'b10)?sub_result:
				(op==4'b11)?and_result: 
				(op==4'b100)?or_result:
				(op==4'b101)?not_result:
				(op==4'b110)?sl_result:
				(op==4'b0111)?sr_result:
				sru_result;//				op==4'b1000?;
	assign exbus_stvalue = stvalue;
	
	assign add_result = value1+value2;
	assign sub_result = value1+~value2+1;
	assign and_result = value1&value2;
	assign or_result = value1|value2;
	assign not_result = ~value1;
	assign sl_result = value1<<value2;
	assign sr_result = value1>>>value2;
	assign sru_result = value1>>value2;
	
	always @(posedge clock) begin
		if (reset) begin
			op <= 4'b0;
			dest <= 3'b0;
		end else begin
			valid <= idbus_valid;
			op <= idbus_op;
			dest <= idbus_dest;
			value1 <= idbus_value1;
			value2 <= idbus_value2;				
			stvalue <= idbus_stvalue;
		end
	end
	
	assign exbus[39] = exbus_valid;
	assign exbus[38:35] = exbus_op;
	assign exbus[34:32] = exbus_dest;
	assign exbus[31:16] = exbus_exresult;
	assign exbus[15:0] = exbus_stvalue;

endmodule

`timescale 1ns / 10ps
module mem_module(clock, reset, exbus, membus, mem_dest);
	input clock;
	input reset;
	input[39:0]exbus;
	output[39:0]membus;
	output[2:0]mem_dest;
	
	wire exbus_valid;
	wire[3:0] exbus_op;
	wire[2:0] exbus_dest;
	wire[15:0] exbus_exresult;
	wire[15:0] exbus_stvalue;
	
	wire membus_valid;
	wire[3:0] membus_op;
	wire[2:0] membus_dest;
	wire[15:0] membus_exresult;
	wire[15:0] membus_memresult;
	
	assign exbus_valid = exbus[39];
	assign exbus_op = exbus[38:35];
	assign exbus_dest = exbus[34:32];
	assign exbus_exresult = exbus[31:16];
	assign exbus_stvalue = exbus[15:0];
	
	// -- Enter your statements here -- //
	reg valid;
	reg [3:0] op;
	reg [2:0] dest;
	reg [15:0] exresult;
	reg [15:0] stvalue;
	wire [11:0] addr;
	ram ram1(.clock(clock), .raddr(addr), .rout(membus_memresult), 
		.wen(op==4'b1011), .waddr(addr), .win(stvalue));
	
	assign addr = exresult[11:0];
	assign membus_valid = valid;
	assign membus_op = op;
	assign membus_dest = dest;
	assign mem_dest = dest;
	assign membus_exresult = exresult;
	always @(posedge clock) begin
		if (reset) begin
			valid <= 1'b0;
			dest <= 3'b0;
		end else begin
			valid <= exbus_valid;
			op <= exbus_op;
			dest <= exbus_dest;
			exresult <= exbus_exresult;
			stvalue <= exbus_stvalue;
		end
	end
	
	assign membus[39] = membus_valid;
	assign membus[38:35] = membus_op;
	assign membus[34:32] = membus_dest;
	assign membus[31:16] = membus_exresult;
	assign membus[15:0] = membus_memresult;

endmodule

`timescale 1ns / 10ps
module wb_module(clock, reset, membus, wbbus, wb_dest);
	input clock;
	input reset;
	input[39:0]membus;
	output[19:0]wbbus;
	output[2:0]wb_dest;
	
	wire membus_valid;
	wire[3:0] membus_op;
	wire[2:0] membus_dest;
	wire[15:0] membus_exresult;
	wire[15:0] membus_memresult;
	
	wire wbbus_valid;
	wire[2:0] wbbus_dest;
	wire[15:0] wbbus_value;
	
	assign membus_valid = membus[39];
	assign membus_op = membus[38:35];
	assign membus_dest = membus[34:32];
	assign membus_exresult = membus[31:16];
	assign membus_memresult = membus[15:0];
	
	// -- Enter your statements here -- //
	reg valid;
	reg [2:0] dest;
	reg [3:0] op;
	reg [15:0] exresult;
	reg [15:0] memresult;
	wire is_loadstore;
	assign is_loadstore = op[3] & op[1];
	assign wbbus_valid = valid;
	assign wbbus_dest = dest;
	assign wb_dest = dest;
	assign wbbus_value = is_loadstore?memresult:exresult; // dest of st is 0
	always @(posedge clock) begin
		if (reset) begin
			dest <= 3'b0;
		end else begin
			valid <= membus_valid;
			dest <= membus_dest;
			op <= membus_op;
			exresult <= membus_exresult;
			memresult <= membus_memresult;
		end
	end
	
	assign wbbus[19] = wbbus_valid;
	assign wbbus[18:16] = wbbus_dest;
	assign wbbus[15:0] = wbbus_value;

endmodule

`timescale 1ns / 10ps
module rom(raddr, rout);
	input[11:0]raddr;
	output[15:0]rout;
	
	reg[15:0] rom[4095:0];
	integer i;
	initial
	begin
		$readmemb("rom.vlog", rom);
		$display("\nLoad rom successfully !!\n\n");
	end
	assign rout = rom[raddr];
endmodule

`timescale 1ns / 10ps
module ram(clock, raddr, rout, wen, waddr, win);
	input clock;
	input wen;
	input[15:0]win;
	input[11:0]raddr;
	input[11:0]waddr;
	output[15:0]rout;
	
	reg[15:0] ram[4095:0];
	assign rout = ram[raddr];
	
	always @(posedge clock) begin
		if (wen) begin
			ram[waddr] = win;
		end
	end
endmodule

`timescale 1ns / 10ps
module regfile(clock, raddr1, rout1, raddr2, rout2, wen, waddr, win);
	input clock;
	input wen;
	input[15:0]win;
	input[2:0]raddr1, raddr2;
	input[2:0]waddr;
	output[15:0]rout1, rout2;
	
	reg[15:0] regs[7:0];
	assign rout1 = regs[raddr1];
	assign rout2 = regs[raddr2];
	
	always @(posedge clock)
	begin
		regs[0] <= 16'b0;
		if (wen)
		begin
			if (waddr != 0)regs[waddr] <= win;
		end
	end

endmodule
