module bitcoin_hash_serial (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;
logic [31:0] hout0[num_nonces];

 

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


enum logic [2:0] {IDLE, READ_MESS, READ_PHASE_1, READ_PHASE_2, COMPUTE, PHASE3, FETCH, WRITE} state;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [31:0] cur_write_data;
logic [4:0] wr_ctr;
logic cur_we;
logic [15:0] t;
logic [15:0] rd_ptr;
logic [2:0] phase_ctr;
logic [4:0] nonce = 4'b0000;
logic [31:0] w[16];
logic [15:0] cur_addr;
logic [15:0] offset = 0;
logic done_ph;
logic [31:0] message[19];
logic [31:0] ph1_h0, ph1_h1, ph1_h2, ph1_h3, ph1_h4, ph1_h5, ph1_h6, ph1_h7;


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;
assign mem_addr = cur_addr + offset;

//SHA256 HASH ROUND
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                   			  input logic [7:0] t);
	logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
	S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
	ch = (e & f) ^ ((~e) & g);
	t1 = h + S1 + ch + k[t] + w;
	S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
	maj = (a & b) ^ (a & c) ^ (b & c);
	t2 = S0 + maj;

	sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

//WORD EXPAND FUNCTION
function logic [31:0] word_expand; // function with no inputs
	logic [31:0] s0, s1;
		s0 = rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
		s1 = rightrotate(w[14],17)^rightrotate(w[14],19)^(w[14]>>10);
		word_expand = w[0] + s0 + w[9] + s1;
endfunction

// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                    			  input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction


always @ (posedge clk or negedge reset_n) begin
	if(!reset_n) begin
				state <= IDLE;
  				h0 <= 0;
     			h1 <= 0;
     			h2 <= 0;
     			h3 <= 0;
     			h4 <= 0;
     			h5 <= 0;
     			h6 <= 0;
     			h7 <= 0;

     			a <= 0;
     			b <= 0;
     			c <= 0;
     			d <= 0;
     			e <= 0;
     			f <= 0;
     			g <= 0;
     			h <= 0;
				
				t <= 0;
     			cur_we <= 1'b0;
     			cur_write_data <= 0;
     			wr_ctr <= 0;
				offset <= 0;
				cur_addr <= 0; 
				phase_ctr <= 0;
				done_ph <= 0;
	end
	else begin
		case(state)
		
			//initialize ho to h7 and a to h to original initial hash constants
			//initialize cur_addr to message_addr to start reading from that location in the next state
			IDLE: begin
				if(start) begin
					h0 <= 32'h6a09e667;
					h1 <= 32'hbb67ae85;
					h2 <= 32'h3c6ef372;
					h3 <= 32'ha54ff53a;
					h4 <= 32'h510e527f;
					h5 <= 32'h9b05688c;
					h6 <= 32'h1f83d9ab;
					h7 <= 32'h5be0cd19;

					a <= 32'h6a09e667;
					b <= 32'hbb67ae85;
					c <= 32'h3c6ef372;
					d <= 32'ha54ff53a;
					e <= 32'h510e527f;
					f <= 32'h9b05688c;
					g <= 32'h1f83d9ab;
					h <= 32'h5be0cd19;
					
					cur_we <= 1'b0;
					state <= READ_MESS;
					cur_write_data <= 0;
					cur_addr <= message_addr;
					offset <= offset + 1;
					wr_ctr <= 0;
					t <= 0;
					done <= 0;
				end
			end
			
			//Reading 19 words from memory
			READ_MESS: begin
				if(offset<20) begin
					offset <= offset + 1;
					message[offset-1] <= mem_read_data;
					state <= READ_MESS;
				end
				else begin
					offset <= 0;
					state <= READ_PHASE_1;
				end
			end
			
			// Reading 1st message block i.e. message[0] to message[15] to w array
			READ_PHASE_1: begin
				phase_ctr <= 1;
				cur_we <= 1'b0;
				if(t<16) begin
					w[t] <= message[t];
					t<= t+1;
					state <= READ_PHASE_1;
				end
				else begin
					state <= COMPUTE;
					t <= 0;
				end
			end
			
			//a to h and h0 to h7 values are output hash values of phase 1 i.e. ph1_h0 t0 ph1_h7
			//read remaining 3 message words into word array and 4th word is the nonce
			// remaining words padding bits and size
			READ_PHASE_2: begin
				phase_ctr <= 2;
				cur_we <= 1'b0;
				h0 <= ph1_h0;
				h1 <= ph1_h1;
				h2 <= ph1_h2;
				h3 <= ph1_h3;
				h4 <= ph1_h4;
				h5 <= ph1_h5;
				h6 <= ph1_h6;
				h7 <= ph1_h7;
				
				w[0] <= message[16];
				w[1] <= message[17];
				w[2] <= message[18];
				w[3] <= nonce;
				w[4] <= 32'h8000_0000;
				{w[5],w[6],w[7],w[8],w[9],w[10],w[11],w[12],w[13],w[14]} <= {10{32'h0000_0000}}; 
				w[15] <= 32'd640;
				
				a <= ph1_h0;
				b <= ph1_h1;
				c <= ph1_h2;
				d <= ph1_h3;
				e <= ph1_h4;
				f <= ph1_h5;
				g <= ph1_h6;
				h <= ph1_h7;
				state <=COMPUTE;
			
			
			end
			
			
			//generates output hash values by generating a to h by calling sha256_op function 64 times
			//state transition is decided based on phase_ctr
			COMPUTE: begin
				 if(t<15)begin
					{a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
					t <= t+1;
					state <= COMPUTE;
				end else if (t<64)begin
					for (int n = 0; n < 15; n++) w[n] <= w[n+1];
					w[15] <= word_expand();
					{a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[15], t);
					t <= t+1;
					
				end
				
				else begin
					h0 <= h0 + a;
					h1 <= h1 + b;
					h2 <= h2 + c;
					h3 <= h3 + d;
					h4 <= h4 + e;
					h5 <= h5 + f;
					h6 <= h6 + g;
					h7 <= h7 + h;
					t <= 0;
					done_ph <= 1;
					
					//should go to phase2
					if (phase_ctr == 1 && done_ph==1) begin
					
						ph1_h0 <= h0;
						ph1_h1 <= h1;
						ph1_h2 <= h2;
						ph1_h3 <= h3;
						ph1_h4 <= h4;
						ph1_h5 <= h5;
						ph1_h6 <= h6;
						ph1_h7 <= h7;
						state <= READ_PHASE_2;
					end
					
					//should go to phase 3
					else if (phase_ctr == 2) begin
						state <= PHASE3;
					end
					
					//should go to fetch
					else if (phase_ctr==3) begin
						state <= FETCH;
						
					end
					
					else state<= COMPUTE;
				end
			end
			
			//input words for phase3 are the output hash values of phase 2
			//ho to h7 and a to h <= original hash constants
			PHASE3: begin
				phase_ctr <= 3;
				{w[0],w[1],w[2],w[3],w[4],w[5],w[6],w[7]} <= {h0,h1,h2,h3,h4,h5,h6,h7};
				w[8] <= 32'h8000_0000;
				{w[9],w[10],w[11],w[12],w[13],w[14]} <= {6{32'h0000_0000}}; 
				w[15] <= 32'd256;
				
				h0 <= 32'h6a09e667;
				h1 <= 32'hbb67ae85;
				h2 <= 32'h3c6ef372;
				h3 <= 32'ha54ff53a;
				h4 <= 32'h510e527f;
				h5 <= 32'h9b05688c;
				h6 <= 32'h1f83d9ab;
				h7 <= 32'h5be0cd19;
				
				a <= 32'h6a09e667;
				b <= 32'hbb67ae85;
				c <= 32'h3c6ef372;
				d <= 32'ha54ff53a;
				e <= 32'h510e527f;
				f <= 32'h9b05688c;
				g <= 32'h1f83d9ab;
				h <= 32'h5be0cd19;
				
				state <= COMPUTE;
				
			end
			
			//nonce is incremented and goes to read_phase_2 until nonce = 16
			//stores h0 value for different nonce into hout0
			//when nonce is greater than 16 goes to WRITE state
			FETCH: begin
				hout0[nonce] <= h0;
			
				if(nonce > 15)begin
					state <= WRITE;
					cur_addr <= output_addr;
				end
				
				else begin
					phase_ctr <= 2;
					nonce<= nonce + 1'b1;
					state <= READ_PHASE_2;
				end
			
			end
			
			//Write value of hout0[nonce] for nonce value 0 to 15 into memory 
			WRITE:begin
				cur_we <= 1;
     			//state <= (wr_ctr == 15) ? IDLE:WRITE;
				done <= (wr_ctr == 16) ? 1'b1:1'b0;
     		   if(wr_ctr<16) begin
     			   cur_addr <= cur_we ? cur_addr  + 1 : cur_addr;
     			   wr_ctr <= wr_ctr + 1;
     			   cur_write_data <= hout0[wr_ctr];
					state <= WRITE;
     		   end
				else begin
					state <= IDLE;
				end
				
			end
		endcase
	end
end

endmodule