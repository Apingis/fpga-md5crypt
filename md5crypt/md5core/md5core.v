`timescale 1ns / 1ps
/*
 * This software is Copyright (c) 2018-2019 Denis Burykin
 * [denis_burykin yahoo com], [denis-burykin2014 yandex ru]
 * and it is hereby released to the general public under the following terms:
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted.
 *
 */
`include "../md5.vh"

//
// Do define CORE_INCLUDE_SRC when NGC file for "blackbox" module 
// is being built.
//
//`define CORE_INCLUDE_SRC


`ifndef CORE_INCLUDE_SRC
	`ifdef SIMULATION
		`define CORE_INCLUDE_SRC
	`endif
`endif

module md5core_type0(
	input CLK,
	input start, ctx_num, seq_num,
	output [3:0] ready, // input buffer is ready for data
	input wr_en,
	input [31:0] din,
	input [3:0] wr_addr,
	input [`BLK_OP_MSB:0] input_blk_op,
	input input_ctx, input_seq, set_input_ready,
	output [31:0] dout,
	output dout_seq_num, dout_ctx_num, dout_en
	);

`ifdef CORE_INCLUDE_SRC
	md5core #( .CORE_TYPE(0) ) core(
		.CLK(CLK),
		.start(start), .ctx_num(ctx_num),
		.seq_num(seq_num), .ready(ready),
		.wr_en(wr_en), .din(din), .wr_addr(wr_addr),
		.input_blk_op(input_blk_op), .input_seq(input_seq),
		.input_ctx(input_ctx), .set_input_ready(set_input_ready),
		
		.dout(dout), .dout_en(dout_en),
		.dout_seq_num(dout_seq_num), .dout_ctx_num(dout_ctx_num)
	);
`endif

endmodule


module md5core_type1(
	input CLK,
	input start, ctx_num, seq_num,
	output [3:0] ready, // input buffer is ready for data
	input wr_en,
	input [31:0] din,
	input [3:0] wr_addr,
	input [`BLK_OP_MSB:0] input_blk_op,
	input input_ctx, input_seq, set_input_ready,
	output [31:0] dout,
	output dout_seq_num, dout_ctx_num, dout_en
	);

`ifdef CORE_INCLUDE_SRC
	md5core #( .CORE_TYPE(1) ) core(
		.CLK(CLK),
		.start(start), .ctx_num(ctx_num),
		.seq_num(seq_num), .ready(ready),
		.wr_en(wr_en), .din(din), .wr_addr(wr_addr),
		.input_blk_op(input_blk_op), .input_seq(input_seq),
		.input_ctx(input_ctx), .set_input_ready(set_input_ready),
		
		.dout(dout), .dout_en(dout_en),
		.dout_seq_num(dout_seq_num), .dout_ctx_num(dout_ctx_num)
	);
`endif

endmodule


module md5core_type2(
	input CLK,
	input start, ctx_num, seq_num,
	output [3:0] ready, // input buffer is ready for data
	input wr_en,
	input [31:0] din,
	input [3:0] wr_addr,
	input [`BLK_OP_MSB:0] input_blk_op,
	input input_ctx, input_seq, set_input_ready,
	output [31:0] dout,
	output dout_seq_num, dout_ctx_num, dout_en
	);

`ifdef CORE_INCLUDE_SRC
	md5core #( .CORE_TYPE(2) ) core(
		.CLK(CLK),
		.start(start), .ctx_num(ctx_num),
		.seq_num(seq_num), .ready(ready),
		.wr_en(wr_en), .din(din), .wr_addr(wr_addr),
		.input_blk_op(input_blk_op), .input_seq(input_seq),
		.input_ctx(input_ctx), .set_input_ready(set_input_ready),
		
		.dout(dout), .dout_en(dout_en),
		.dout_seq_num(dout_seq_num), .dout_ctx_num(dout_ctx_num)
	);
`endif

endmodule


module md5core #(
	parameter CORE_TYPE = 0
	)(
	input CLK,
	input start, // asserts on cycle 0 of each sequence
	input ctx_num,
	input seq_num,
	output reg [3:0] ready = 4'b1111, // input buffer is ready for data

	input wr_en,
	input [31:0] din,
	input [3:0] wr_addr,
	input [`BLK_OP_MSB:0] input_blk_op, // registered on set_input_ready
	input input_ctx, input_seq,
	input set_input_ready, // valid only when wr_en is asserted

	output [31:0] dout,
	output dout_seq_num, dout_ctx_num,
	output reg dout_en = 0
	);


	reg seq_num_r = 0, start_r = 0;
	always @(posedge CLK) begin
		seq_num_r <= seq_num;
		start_r <= start;
	end


	// ***************************************************************
	//
	//   INPUT CONTROLS
	//
	// Input buffer (for 4 contexts) consists of:
	// - memory (rows 0-63)
	// - input_blk_op_r[0:3]
	//
	// ***************************************************************
	always @(posedge CLK) begin
		if (wr_en)
			ready [{input_ctx, input_seq}] <= 0;
		if (cnte == 67)
			ready [{ctxe, seq_num_curr}] <= 1;
	end

	reg [3:0] input_ready = 4'b0000; // input buffer has required data
	always @(posedge CLK) begin
		if (wr_en & set_input_ready)
			input_ready [{input_seq, input_ctx}] <= 1;
		if (start_eqn)
			input_ready [{seq_num_r, ctxe}] <= 0;
	end

	(* RAM_STYLE="DISTRIBUTED" *)
	reg [`BLK_OP_MSB:0] input_blk_op_r[0:3], blk_op[0:1];
	always @(posedge CLK)
		if (wr_en & set_input_ready)
			input_blk_op_r [{input_seq, input_ctx}] <= input_blk_op;


	// ***************************************************************
	//
	//   STATE, ROUND COUNTERS
	//
	// - Every 2 computation are (RND_CNT_MAX+1)*2 rounds.
	// - Rounds are "interleaved": one cycle it goes context #0
	//   and on the next round it goes context #1.
	// - Cycles are labeled "even" (read mem, write B,C,D) and "odd".
	//
	// ***************************************************************
	localparam RND_CNT_MAX = 71;
	localparam AFTER_CNT_MAX = 6;

	reg ctxe = 0; // context # in "even" cycle
	// round count 0..RND_CNT_MAX (RND_CNT_MAX+1 means idle)
	reg [6:0] cnt0 = RND_CNT_MAX+1, cnt1 = RND_CNT_MAX+1;
	// round count for even,odd cycles
	reg [6:0] cnte = RND_CNT_MAX+1, cnto = RND_CNT_MAX+1;
	reg cnte_idle = 1;
	reg [1:0] ctx_idle = 2'b11;
	reg seq_num_curr = 0, seq_num0 = 0, seq_num1 = 0;

	always @(posedge CLK) begin
		cnte <= ctxe ? cnt0 : cnt1;
		cnte_idle <= (ctxe ? cnt0 : cnt1) == RND_CNT_MAX+1;

		cnto <= cnte;
		ctxe <= ctx_num;
		seq_num_curr <= ctxe ? seq_num0 : seq_num1;
	end

	// Enable operation of the core
	reg glbl_en = 0;
	always @(posedge CLK)
		glbl_en <= ~(&ctx_idle) | after_cnt != AFTER_CNT_MAX+1;

	reg input_ready_r = 0;
	always @(posedge CLK)
		input_ready_r <= input_ready [{seq_num, ~ctxe}];

	wire start_eqn = start_r & input_ready_r;

	always @(posedge CLK) begin
		if (start_eqn) begin
			if (~ctxe) begin
				cnt0 <= 0;
				seq_num0 <= seq_num_r;
			end
			else begin
				cnt1 <= 0;
				seq_num1 <= seq_num_r;
			end
			ctx_idle [ctxe] <= 0;
			blk_op [ctxe] <= input_blk_op_r [{seq_num_r, ctxe}];
		end
		else begin
			if (cnte_idle)
				ctx_idle [ctxe] <= 1;
			if (~cnte_idle & ~ctxe)
				cnt0 <= cnte + 1'b1;
			if (~cnte_idle & ctxe)
				cnt1 <= cnte + 1'b1;
		end
	end
	

	// it requires to finish operation after "main" counter is done
	reg [2:0] after_cnt = AFTER_CNT_MAX+1;
	// Only 1 context at a time is performing finishing operation
	// after "main" counter
	reg after_ctx;
	reg after_seq_num;
	reg [`BLK_OP_MSB:0] after_blk_op;
	
	always @(posedge CLK) begin
		if (cnte == 67) begin
			after_cnt <= 0;
			after_ctx <= ctxe;
			after_seq_num <= seq_num_curr;
			after_blk_op <= blk_op [ctxe];
		end
		else if (after_cnt != AFTER_CNT_MAX+1 & after_ctx == ctxe)
			after_cnt <= after_cnt + 1'b1;
	end

	
	// ***************************************************************
	//
	//   BLOCK & CONTEXT OPERATION.
	//
	// ***************************************************************
	// Write memory1 (from input)
	wire [5:0] mem_wr_addr = { input_seq, input_ctx, wr_addr };

	// Write memory2 (save context)
	reg mem2_wr_en = 0;
	always @(posedge CLK) begin
		mem2_wr_en <= after_cnt >= 3 & after_cnt <= 6
			& after_ctx != ctxe & ~`BLK_OP_END_COMP_OUTPUT(after_blk_op);
		// 'dout_en' asserts 1 cycle before actual data
		dout_en <= after_cnt >= 2 & after_cnt <= 5
			& after_ctx == ctxe & `BLK_OP_END_COMP_OUTPUT(after_blk_op);
	end
	
	wire [1:0] mem2_wr_addr_local = after_cnt - 2'd3;
	wire [3:0] mem2_wr_addr =
		{ after_seq_num, after_ctx, mem2_wr_addr_local };


	//
	// Memory, K[t] - Read
	//
	wire [3:0] mem_rd_addr_data;
	cnte2addr cnte2addr( .cnte(cnte), .rd_addr(mem_rd_addr_data) );

	// Memory1 layout:
	// 0..63 input data (x4 contexts)
	wire [5:0] mem_rd_addr =
		{ seq_num_curr, ctxe, mem_rd_addr_data };


	wire mem2_rd_en_add = after_cnt <= 3 & after_ctx == ctxe;
	wire mem2_rd_en = mem2_rd_en_add | cnte >= 0 & cnte <= 3;
	// Memory2 layout:
	// 0..15 saved IVs (x4)
	// 16..19 IVs for a new computation
	wire [4:0] mem2_rd_addr =
		~mem2_rd_en_add ? ( // Initial load of IVs
			`BLK_OP_IF_NEW_CTX(blk_op[ctxe]) ? { 3'b100, cnte[1:0] }
				: { 1'b0, seq_num_curr, ctxe, cnte[1:0] }
		) : ( // Load IVs for additions at the end of block
			`BLK_OP_IF_NEW_CTX(after_blk_op) ? { 3'b100, after_cnt[1:0] }
				: { 1'b0, after_seq_num, after_ctx, after_cnt[1:0] }
		);

	wire K_rst = cnto >= 0 & cnto <= 3;
	wire [6:0] K_rnd_num = cnte;


	//
	// Various Controls
	//
	reg ctx_in_rotate = 0;
	always @(posedge CLK)
		ctx_in_rotate <= after_cnt >= 1 & after_cnt <= 4
			& after_ctx != ctxe;
	
	wire A_rst = cnte >= 1 & cnte <= 4;
	wire D_rst = cnte >= 0 & cnte <= 3;

	reg addB2en = 0;
	always @(posedge CLK)
		addB2en <= cnto >= 5 & cnto <= 68;

	reg F_en = 0;
	reg [1:0] F_rnd_num = 0;
	always @(posedge CLK) begin
		F_en <= cnte >= 5 & cnte <= 68;
		F_rnd_num <=
			cnte >= 5 & cnte <= 20 ? 2'b00 :
			cnte >= 21 & cnte <= 36 ? 2'b01 :
			cnte >= 37 & cnte <= 52 ? 2'b10 :
			2'b11;
	end

	wire [3:0] rotate_opt;
	cnto2rotate_opt cnto2rotate_opt(
		.CLK(CLK), .cnto(cnto), .rotate_opt(rotate_opt) );


	md5ctx #( .CORE_TYPE(CORE_TYPE)
	) md5ctx(
		.CLK(CLK),
		.din(din), .mem_wr_en(wr_en), .mem_wr_addr(mem_wr_addr),
		.mem2_wr_en(mem2_wr_en), .mem2_wr_addr(mem2_wr_addr),
		.mem2_rd_en(mem2_rd_en), .mem2_rd_addr(mem2_rd_addr),
		
		.mem_rd_en(~mem2_rd_en & glbl_en), .mem_rd_addr(mem_rd_addr),
		.en(glbl_en), .F_en(F_en), .addB2en(addB2en), .A_rst(A_rst),
		.D_rst(1'b0),//D_rst),
		.F_rnd_num(F_rnd_num),
		
		.rotate_opt(rotate_opt[3:0]),
		.ctx_in_rotate(ctx_in_rotate),
		.K_rnd_num(K_rnd_num), .K_rst(K_rst),
		.out({dout[15:0], dout[31:16]})
	);


	assign dout_seq_num = after_seq_num;
	assign dout_ctx_num = after_ctx;

endmodule


module md5ctx #(
	parameter CORE_TYPE = 0
	)(
	input CLK,
	input [31:0] din,
	input mem_wr_en, mem_rd_en,
	input [5:0] mem_wr_addr, mem_rd_addr,
	input mem2_wr_en, mem2_rd_en,
	input [3:0] mem2_wr_addr,
	input [4:0] mem2_rd_addr,
	
	input en, F_en, addB2en, K_rst, D_rst,
	input A_rst,
	input ctx_in_rotate,
	input [1:0] F_rnd_num,
	input [3:0] rotate_opt,
	input [6:0] K_rnd_num,
	output [31:0] out
	);

	//
	// Memory with input data blocks
	//
	wire [31:0] mem_out;
	if (CORE_TYPE != 2) begin

		mem_type01 mem(.CLK(CLK), .din(din),
			.wr_en(mem_wr_en), .wr_addr(mem_wr_addr),
			.rd_en(mem_rd_en), .rd_addr(mem_rd_addr),
			.dout(mem_out)
		);
	end else begin // CORE_TYPE == 2

		mem_type2 mem(.CLK(CLK), .din(din),
			.wr_en(mem_wr_en), .wr_addr(mem_wr_addr),
			.rd_en(mem_rd_en), .rd_addr(mem_rd_addr),
			.dout(mem_out)
		);
	end

	//
	// Memory with IVs and results
	//
	wire [31:0] mem2_out;
	if (CORE_TYPE == 0) begin

		mem2_type0 mem2(.CLK(CLK), .din(out),
			.wr_en(mem2_wr_en), .wr_addr(mem2_wr_addr),
			.rd_en(mem2_rd_en), .rd_addr(mem2_rd_addr),
			.dout(mem2_out)
		);
	end else begin // CORE_TYPE == 1

		mem2_type12 mem2(.CLK(CLK), .din(out),
			.wr_en(mem2_wr_en), .wr_addr(mem2_wr_addr),
			.rd_en(mem2_rd_en), .rd_addr(mem2_rd_addr),
			.dout(mem2_out)
		);
	end

	reg mem2_rd_en_r = 0, mem2_rd_en_r2 = 0;
	always @(posedge CLK) begin
		mem2_rd_en_r <= mem2_rd_en;
		mem2_rd_en_r2 <= mem2_rd_en_r;
	end

	reg [31:0] mem_mux_r;
	always @(posedge CLK)
		if (en)
			mem_mux_r <= mem2_rd_en_r ? mem2_out : mem_out;


	wire [31:0] Kt;
	if (CORE_TYPE != 2) begin

		md5_Kt_bram Kt_inst(
			.CLK(CLK), .t(K_rnd_num), .en(en), .rst(K_rst),
			.Kt(Kt)
		);
	end else begin // CORE_TYPE == 2

		md5_Kt_dram Kt_inst(
			.CLK(CLK), .t(K_rnd_num), .en(en), .rst(K_rst),
			.Kt(Kt)
		);
	end

	reg [31:0] ctx_in;
	always @(posedge CLK)
		if (en) // 1 LUT/bit
			ctx_in <= (mem2_rd_en_r2 ? 0 : Kt)
				+ (ctx_in_rotate ? {mem_mux_r[15:0], mem_mux_r[31:16]} :
					mem_mux_r);


	reg [31:0] B = 0, B2 = 0, C = 0;
	
	//wire [31:0] D;
	//shreg #(.DEPTH(2)) D_inst( .CLK(CLK), .en(en), .i(C), .o(D) );
	//shreg_ff #(.DEPTH(2)) D_inst( .CLK(CLK), .en(en), .rst(D_rst),
	//	.i(C), .o(D) );
	(* SHREG_EXTRACT="no" *) reg [31:0] C2 = 0, D = 0;

	//wire [31:0] A;
	//shreg_ff #(.DEPTH(2)) A_inst( .CLK(CLK), .en(en), .rst(A_rst),
	//	.i(D), .o(A) );
	//shreg #(.DEPTH(2)) A_inst( .CLK(CLK), .en(en), .i(D), .o(A) );
	(* SHREG_EXTRACT="no" *) reg [31:0] D2 = 0, A = 0;

	wire [31:0] F = ~F_en ? 0 :
		F_rnd_num == 0 ? ((B & C) | (~B & D)) :
		F_rnd_num == 1 ? ((B & D) | (C & ~D)) :
		F_rnd_num == 2 ? (B ^ C ^ D) :
		(C ^ (B | ~D));

	wire [31:0] add1result;// = F + A + in;
	add3 add1_inst( .CLK(CLK), .en(en), .rst(1'b0),
		.a(A), .b(F), .c(ctx_in), .o(add1result) );

	//wire [31:0] rotate1result, rotate2result;
	//rotator0 rotator_inst(
	//	.rotate_opt1(rotate_opt[1:0]), .rotate_opt2(rotate_opt[3:2]),
	//	.din(add1result), .dout1(rotate1result), .dout2(rotate2result)
	//);

	//wire [31:0] add2result = (addB2en ? B2 : 0) + rotate2result;

	wire [31:0] rotate_result;
	rotator1 rotator_inst(
		.rotate_opt({rotate_opt[1:0], rotate_opt[3]}),
		.din(add1result), .dout(rotate_result)
	);

	wire [31:0] add2result = (addB2en ? B2 : 0)
		+ (~rotate_opt[2]
			? rotate_result
			: {rotate_result[30:0], rotate_result[31]}
		); // 1 LUT/bit

	always @(posedge CLK) begin
		if (en) begin
			B <= add2result;
			B2 <= B;
			C <= B2;
			C2 <= C;
			D2 <= D;
		end
		if (A_rst)
			A <= 0;
		else if (en)
			A <= D2;
		if (D_rst)
			D <= 0;
		else if (en)
			D <= C2;
	end

	assign out = B2;

endmodule


// Counter for "even" cycles -> data addr
module cnte2addr(
	input [6:0] cnte,
	output [3:0] rd_addr
	);

	localparam [72*4-1 :0] RD_ADDR = {
		4'd0, 4'd0, 4'd0, 4'd0,
		4'd9, 4'd2, 4'd11, 4'd4, 4'd13, 4'd6, 4'd15, 4'd8,
		4'd1, 4'd10, 4'd3, 4'd12, 4'd5, 4'd14, 4'd7, 4'd0,
		4'd2, 4'd15, 4'd12, 4'd9, 4'd6, 4'd3, 4'd0, 4'd13,
		4'd10, 4'd7, 4'd4, 4'd1, 4'd14, 4'd11, 4'd8, 4'd5,
		4'd12, 4'd7, 4'd2, 4'd13, 4'd8, 4'd3, 4'd14, 4'd9,
		4'd4, 4'd15, 4'd10, 4'd5, 4'd0, 4'd11, 4'd6, 4'd1,
		4'd15, 4'd14, 4'd13, 4'd12, 4'd11, 4'd10, 4'd9, 4'd8,
		4'd7, 4'd6, 4'd5, 4'd4, 4'd3, 4'd2, 4'd1, 4'd0,
		4'd0, 4'd0, 4'd0, 4'd0
	};

	integer k;

	(* RAM_STYLE="DISTRIBUTED" *)
	reg [3:0] rd_addr_mem [0:63];
	(* RAM_STYLE="DISTRIBUTED" *)
	reg [3:0] rd_addr_mem2 [0:3];
	initial
		for (k=0; k < 68; k=k+1)
			if (k < 64)
				rd_addr_mem[k] = RD_ADDR [k*4 +:4];
			else
				rd_addr_mem2[k-64] = RD_ADDR [k*4 +:4];

	assign rd_addr = ~cnte[6] ? rd_addr_mem [cnte[5:0]]
		: rd_addr_mem2 [cnte[1:0]];

	//assign rd_addr = RD_ADDR [cnte*4 +:4]; // this consumes muxf7's

endmodule


module cnto2rotate_opt(
	input CLK,
	input [6:0] cnto,
	output reg [3:0] rotate_opt = 4'b1010 // s=16
	);

	localparam [73*8-1 :0] S = {
		8'd16, 8'd16, 8'd16, 8'd16, 8'd16,
		8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
		8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
		8'd5, 8'd9, 8'd14, 8'd20, 8'd5, 8'd9, 8'd14, 8'd20,
		8'd5, 8'd9, 8'd14, 8'd20, 8'd5, 8'd9, 8'd14, 8'd20,
		8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
		8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
		8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21,
		8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21,
		8'd16, 8'd16, 8'd16, 8'd16
	};
	
	integer k, s, opt1, opt2;

	(* RAM_STYLE="DISTRIBUTED" *)
	reg [3:0] rotate_opt_mem [0:63];
	(* RAM_STYLE="DISTRIBUTED" *)
	reg [3:0] rotate_opt_mem2 [0:8];
	initial
		for (k=0; k < 73; k=k+1) begin
			s = S [(7'd73 - k)*8-4 -:5];
			opt1 =
				s >= 4 & s <= 7 ? 2'b00 :
				s >= 9 & s <= 12 ? 2'b01 :
				s >= 14 & s <= 17 ? 2'b10 :
				2'b11;
			opt2 =
				s == 4 | s == 9 | s == 14 | s == 20 ? 2'b00 :
				s == 5 | s == 10 | s == 15 | s == 21 ? 2'b01 :
				s == 6 | s == 11 | s == 16 | s == 22 ? 2'b10 :
				2'b11;
			if (k < 64)
				rotate_opt_mem[k] = {opt2[1:0], opt1[1:0]};
			else
				rotate_opt_mem2[k-64] = {opt2[1:0], opt1[1:0]};
		end

	always @(posedge CLK)
		rotate_opt <= ~cnto[6] ? rotate_opt_mem [cnto[5:0]]
			: rotate_opt_mem2 [cnto[3:0]];

endmodule


// ************************************************************
//
// Several shifters were considered.
//
// ************************************************************
module rotator0(
	input [1:0] rotate_opt1, rotate_opt2,
	input [31:0] din,
	output [31:0] dout1, dout2
	);

	assign dout1 =
		rotate_opt1 == 2'b00 ? {din[27:0], din[31:28]} :
		rotate_opt1 == 2'b01 ? {din[22:0], din[31:23]} :
		rotate_opt1 == 2'b10 ? {din[17:0], din[31:18]} :
		{din[11:0], din[31:12]};

	assign dout2 =
		rotate_opt2 == 2'b00 ? dout1 :
		rotate_opt2 == 2'b01 ? {dout1[30:0], dout1[31]} :
		rotate_opt2 == 2'b10 ? {dout1[29:0], dout1[31:30]} :
		{dout1[28:0], dout1[31:29]};

endmodule


module rotator1(
	input [2:0] rotate_opt,
	input [31:0] din,
	output [31:0] dout
	);

	wire [31:0] r0 = {din[27:0], din[31:28]};
	wire [31:0] r1 = {din[25:0], din[31:26]};
	wire [31:0] r2 = {din[22:0], din[31:23]};
	wire [31:0] r3 = {din[20:0], din[31:21]};
	wire [31:0] r4 = {din[17:0], din[31:18]};
	wire [31:0] r5 = {din[15:0], din[31:16]};
	wire [31:0] r6 = {din[11:0], din[31:12]};
	wire [31:0] r7 = {din[9:0], din[31:10]};

	genvar i;
	generate
	for (i=0; i < 32; i=i+1) begin:mux

		mux8to1 mux8to1_inst(
			.s(rotate_opt), .din({r7[i], r6[i], r5[i], r4[i],
				r3[i], r2[i], r1[i], r0[i]}),
			.dout(dout[i])
		);

	end
	endgenerate

endmodule


module mux8to1(
	input [2:0] s,
	input [7:0] din,
	output dout
	);

	localparam I0 = 64'haaaaaaaaaaaaaaaa;
	localparam I1 = 64'hcccccccccccccccc;
	localparam I2 = 64'hf0f0f0f0f0f0f0f0;
	localparam I3 = 64'hff00ff00ff00ff00;
	localparam I4 = 64'hffff0000ffff0000;
	localparam I5 = 64'hffffffff00000000;

	LUT6_L #(
		.INIT(~I1&~I0&I2 | ~I1&I0&I3 | I1&~I0&I4 | I1&I0&I5)
	) LUT6_L_inst0 (
		.LO(lut_out0), // LUT local output
		.I0(s[0]), // LUT input
		.I1(s[1]), // LUT input
		.I2(din[0]), // LUT input
		.I3(din[1]), // LUT input
		.I4(din[2]), // LUT input
		.I5(din[3])  // LUT input
	);

	LUT6_L #(
		.INIT(~I1&~I0&I2 | ~I1&I0&I3 | I1&~I0&I4 | I1&I0&I5)
	) LUT6_L_inst1 (
		.LO(lut_out1), // LUT local output
		.I0(s[0]), // LUT input
		.I1(s[1]), // LUT input
		.I2(din[4]), // LUT input
		.I3(din[5]), // LUT input
		.I4(din[6]), // LUT input
		.I5(din[7])  // LUT input
	);

	MUXF7 MUXF7_inst (
		.O(dout),    // Output of MUX to general routing
		.I0(lut_out0),  // Input (tie to LUT6 O6 pin)
		.I1(lut_out1),  // Input (tie to LUT6 O6 pin)
		.S(s[2])     // Input select to MUX
	);

endmodule


// *******************************************************
//
// Cores of type0, type1, type2 differ in memory units.
//
// *******************************************************
module mem_type01(
	input CLK,
	input [31:0] din,
	input [5:0] wr_addr, rd_addr,
	input wr_en, rd_en,
	output reg [31:0] dout
	);

	(* RAM_STYLE="block" *)
	reg [31:0] mem [0:63];

	always @(posedge CLK)
		if (wr_en)
			mem [wr_addr] <= din;

	always @(posedge CLK)
		if (rd_en)
			dout <= mem [rd_addr];

endmodule


module mem_type2(
	input CLK,
	input [31:0] din,
	input [5:0] wr_addr, rd_addr,
	input wr_en, rd_en,
	output reg [31:0] dout
	);

	(* RAM_STYLE="distributed" *)
	reg [31:0] mem [0:63];

	always @(posedge CLK)
		if (wr_en)
			mem [wr_addr] <= din;

	always @(posedge CLK)
		if (rd_en)
			dout <= mem [rd_addr];

endmodule


module mem2_type0(
	input CLK,
	input [31:0] din,
	input [3:0] wr_addr,
	input [4:0] rd_addr,
	input wr_en, rd_en,
	output reg [31:0] dout
	);

	(* RAM_STYLE="block" *)
	reg [31:0] mem2 [0:31];

	initial begin
		// Context is saved in order: A D C B
		mem2[16] = 32'h23016745; // A
		mem2[17] = 32'h54761032; // D
		mem2[18] = 32'hdcfe98ba; // C
		mem2[19] = 32'hab89efcd; // B
	end

	always @(posedge CLK)
		if (wr_en)
			mem2 [{1'b0, wr_addr}] <= din;

	always @(posedge CLK)
		if (rd_en)
			dout <= mem2 [rd_addr];

endmodule


module mem2_type12(
	input CLK,
	input [31:0] din,
	input [3:0] wr_addr,
	input [4:0] rd_addr,
	input wr_en, rd_en,
	output reg [31:0] dout
	);

	(* RAM_STYLE="distributed" *)
	reg [31:0] mem2 [0:31];

	initial begin
		// Context is saved in order: A D C B
		mem2[16] = 32'h23016745; // A
		mem2[17] = 32'h54761032; // D
		mem2[18] = 32'hdcfe98ba; // C
		mem2[19] = 32'hab89efcd; // B
	end

	always @(posedge CLK)
		if (wr_en)
			mem2 [{1'b0, wr_addr}] <= din;

	always @(posedge CLK)
		if (rd_en)
			dout <= mem2 [rd_addr];

endmodule


// ***************************************************
//
// "Dummy" core
//
// ***************************************************
module md5core_dummy(
	input CLK,
	input start, // asserts on cycle 0 of each sequence
	input ctx_num,
	input seq_num,
	output reg [3:0] ready = 4'b1111, // input buffer is ready for data

	input wr_en,
	input [31:0] din,
	input [3:0] wr_addr,
	input [`BLK_OP_MSB:0] input_blk_op, // registered on set_input_ready
	input input_ctx, input_seq,
	input set_input_ready, // valid only when wr_en is asserted

	output [31:0] dout,
	output dout_seq_num, dout_ctx_num,
	output reg dout_en = 0
	);

	reg x = 0;
	always @(posedge CLK)
		x <= ~x;
		
	assign dout = {32{x}};
	assign dout_seq_num = x;
	assign dout_ctx_num = x;

	// WARNING:MapLib:935 - Output port "ready<2>" on Partition
   // "/ztex_inouttraffic/pkt_comm/units[11].unit/cores[2].core" is driven by
	// constant signal "pkt_comm/units[11].unit/core_ready<10>".
	//
	// An amount of such messages after the start of Map takes time
	// (up to 1 min). Removed constant signals in "dummy" instances.
	always @(posedge CLK)
		ready <= ~ready;

endmodule
