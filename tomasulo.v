`timescale 1ns / 10ps
module rs(reset,clock,issuebus,resbus,out);
	input reset;
	input clock;
	input [99:0] issuebus;
	input [40:0] resbus;
	output [80:0] out;
	
	reg [1:0] rs_valid;
	reg [7:0] rs_op[1:0];
	reg [7:0] rs_qj[1:0];
	reg [7:0] rs_qk[1:0];
	reg [1:0] rs_rj; // assume 1 indicates ready
	reg [1:0] rs_rk;
	reg [31:0] rs_vj[1:0];
	reg [31:0] rs_vk[1:0];
	reg [7:0] rs_dest[1:0];
	
	wire issue_valid;
	wire issue_rj,issue_rk;
	wire [7:0]issue_op;
	wire [7:0]issue_qj;
	wire [7:0]issue_qk;
	wire [31:0] issue_vj;
	wire [31:0] issue_vk;
	
	wire res_valid;
	wire [7:0]res_dest;
	wire [31:0] res_value;
	
	wire out_valid;
	wire [7:0] out_dest;
	wire [7:0] out_op;
	wire [31:0] out_vj;
	wire [31:0] out_vk;
	
	assign issue_valid= issuebus[98];
	assign issue_op   = issuebus[97:90];
	assign issue_dest = issuebus[89:82];
	assign issue_rj   = issuebus[81];
	assign issue_rk   = issuebus[80];
	assign issue_qj   = issuebus[79:72];
	assign issue_qk   = issuebus[71:64];
	assign issue_vj   = issuebus[63:32];
	assign issue_vk   = issuebus[31:0];
	
	assign res_valid  = resbus[40];
	assign res_dest   = resbus[39:32];
	assign res_value  = resbus[31:0];
	
	assign out[80]    = out_valid;
	assign out[79:72] = out_op;
	assign out[71:64] = out_dest;
	assign out[63:32] = out_vj;
	assign out[31:0]  = out_vk;
	 
	/*Insert your code here*/
	wire zeroOwnsOut, oneOwnsOut;
	assign zeroOwnsOut = rs_valid[0] & rs_rj[0] & rs_rk[0];
	assign oneOwnsOut = rs_valid[1] & rs_rj[1] & rs_rk[1];
	assign out_valid = zeroOwnsOut | oneOwnsOut;
	assign out_op = zeroOwnsOut?rs_op[0]:rs_op[1];
	assign out_dest = zeroOwnsOut?rs_dest[0]:rs_dest[1];
	assign out_vj = zeroOwnsOut?rs_vj[0]:rs_vj[1];
	assign out_vk = zeroOwnsOut?rs_vk[0]:rs_vk[1];
	
	always @(posedge clock) begin
		if (reset) begin
			rs_valid <= 2'b0;
		end else begin
			if (issue_valid) begin
				if (~rs_valid[0]) begin
					rs_valid[0] <= 1'b1;
					
					rs_op[0] <= issue_op;
					rs_dest[0] <= issue_dest;
					rs_rj[0] <= issue_rj;
					rs_rk[0] <= issue_rk;
					rs_qj[0] <= issue_qj;
					rs_qk[0] <= issue_qk;
					rs_vj[0] <= issue_vj;
					rs_vk[0] <= issue_vk;
				end else if (~rs_valid[1]) begin
					rs_valid[1] <= 1'b1;
					
					rs_op[1] <= issue_op;
					rs_dest[1] <= issue_dest;
					rs_rj[1] <= issue_rj;
					rs_rk[1] <= issue_rk;
					rs_qj[1] <= issue_qj;
					rs_qk[1] <= issue_qk;
					rs_vj[1] <= issue_vj;
					rs_vk[1] <= issue_vk;  
				end // ~rs_valid[0]
			end // issue_valid
			if (res_valid) begin
				if (rs_valid[0]) begin
					if (~rs_rj[0] & res_dest==rs_qj[0]) begin
						rs_vj[0] <= res_value;
						rs_rj[0] <= 1'b1;
					end
					if (~rs_rk[0] & res_dest==rs_qk[0]) begin
						rs_vk[0] <= res_value;
						rs_rk[0] <= 1'b1;
					end
				end
				if (rs_valid[1]) begin
					if (~rs_rj[1] & res_dest==rs_qj[1]) begin
						rs_vj[1] <= res_value;
						rs_rj[1] <= 1'b1;
					end
					if (~rs_rk[1] & res_dest==rs_qk[1]) begin
						rs_vk[1] <= res_value;
						rs_rk[1] <= 1'b1;
					end
				end
			end // res_valid
		end // reset
	end

endmodule