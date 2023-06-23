module bitcoin_hash (input logic    	clk, reset_n, start,
                 	input logic [15:0] message_addr, output_addr,
                	output logic    	done, mem_clk, mem_we,
                	output logic [15:0] mem_addr,
                	output logic [31:0] mem_write_data,
                 	input logic [31:0] mem_read_data);

parameter num_nonces = 16;
//logic [ 4:0] state;
logic [31:0] hout0[num_nonces];
/*
logic [31:0] hout1[num_nonces];
logic [31:0] hout2[num_nonces];
logic [31:0] hout3[num_nonces];
logic [31:0] hout4[num_nonces];
logic [31:0] hout5[num_nonces];
logic [31:0] hout6[num_nonces];
logic [31:0] hout7[num_nonces];
*/
 

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



// Student to add rest of the code here

enum logic [2:0] {IDLE, READ_MESS, READ_PHASE_2, COMPUTE, PHASE3, FETCH, WRITE} state;
//logic [31:0] h0[num_nonces], h1[num_nonces], h2[num_nonces], h3[num_nonces], h4[num_nonces], h5[num_nonces], h6[num_nonces], h7[num_nonces];
logic [31:0]H[num_nonces][8];
logic [31:0] a[num_nonces], b[num_nonces], c[num_nonces], d[num_nonces], e[num_nonces], f[num_nonces], g[num_nonces], h[num_nonces];
logic [31:0] cur_write_data;
logic cur_we;
logic [15:0] i,j;
logic [15:0] rd_ptr;
logic [2:0] phase_ctr;
logic [4:0] nonce;
logic [31:0] w[num_nonces][16];
//logic[31:0] word[3];
logic [15:0] cur_addr;
logic [15:0] offset;
logic [15:0] wr_ptr;
logic [31:0] message[3];
logic [31:0] ph1_h0;
logic [31:0] ph1_h1;
logic [31:0] ph1_h2;
logic [31:0] ph1_h3;
logic [31:0] ph1_h4;
logic [31:0] ph1_h5;
logic [31:0] ph1_h6;
logic [31:0] ph1_h7;


assign mem_clk = clk;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;
assign mem_addr = cur_addr + offset;


function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ (~e & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};

end
endfunction


function logic [31:0] rightrotate(input logic [31:0] x,
               				   input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction

function logic [31:0] word_expand; // function with no inputs
    logic [31:0] s0, s1;
   	 s0 = rightrotate(w[j][14],7)^rightrotate(w[j][14],18)^(w[j][14]>>3);
   	 s1 = rightrotate(w[j][1],17)^rightrotate(w[j][1],19)^(w[j][1]>>10);
   	 word_expand = w[j][15] + s0 + w[j][6] + s1;
endfunction

always @ (posedge clk or negedge reset_n) begin
    if(!reset_n) begin
   			 
   		 state <= IDLE;
			 for(i=0;i<num_nonces;i++) begin
				for(j=0;j<8;j++) H[i][j] <= 0;
				a[i]	<= 0;
				b[i]	<= 0;
				c[i]	<= 0;
				d[i]	<= 0;
				e[i]	<= 0;
				f[i]	<= 0;
				g[i]	<= 0;
				h[i]	<= 0;
				 end
   			 
				 cur_we <= 1'b0;
				 cur_write_data <= 0;
   			 offset <= 0;
			 wr_ptr <= 0;
   			 cur_addr <= 0;
   			 phase_ctr <= 0;
   		 nonce    <= 0;
   		 ph1_h0    <= 0;
   		 ph1_h1    <= 0;
   		 ph1_h2    <= 0;
   		 ph1_h3    <= 0;
   		 ph1_h4    <= 0;
   		 ph1_h5    <= 0;
   		 ph1_h6    <= 0;
   		 ph1_h7    <= 0;
    end
    else begin
   	 case(state)
   		 IDLE: begin	//Used to Initiaze the H constants 
   			 if(start) begin
					for(i=0;i<num_nonces;i++)begin
				 H[i][0] <= 32'h6a09e667;
   				 H[i][1] <= 32'hbb67ae85;
   				 H[i][2] <= 32'h3c6ef372;
   				 H[i][3] <= 32'ha54ff53a;
   				 H[i][4] <= 32'h510e527f;
   				 H[i][5] <= 32'h9b05688c;
   				 H[i][6] <= 32'h1f83d9ab;
   				 H[i][7] <= 32'h5be0cd19;
					end
   				 cur_we <= 1'b0;
   				 state <= READ_MESS;
   				 cur_write_data <= 0;
   				 cur_addr <= message_addr;
				offset	<= offset  + 1;
   			 end
			for(j=0;j<num_nonces;j++) begin
				a[j] <= 32'h6a09e667;
				b[j] <= 32'hbb67ae85;
				c[j] <= 32'h3c6ef372;
				d[j] <= 32'ha54ff53a;
				e[j] <= 32'h510e527f;
				f[j] <= 32'h9b05688c;
				g[j] <= 32'h1f83d9ab;
				h[j] <= 32'h5be0cd19;
	end
   		 end
   		 
   		 READ_MESS: begin	//Read the message from memory into W array and pipeline computing as soon as 1st read is done
			
		  			  cur_we <= 0;
		     		 offset <= offset + 1;
					phase_ctr	<= 1;
				if(offset<18) begin
				for(j=0;j<num_nonces;j++) begin
					for(i=1;i<16;i++) begin
					if(offset < 17) begin
							w[j][0]   <= mem_read_data;
							w[j][i]	  <= w[j][i-1];
					end
					if(offset>1) begin
       		 {a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j]} <= sha256_op(a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j], w[j][0], offset-2);
	end
		end
		end
	end
					else begin
					state <= COMPUTE;
		 for(j=0;j<num_nonces;j++) begin
			 for(i=0;i<15;i++) w[j][i+1] <= w[j][i];
   			  w[j][0]	<= word_expand();
		end
									
	end
					 	

     end
   		 
   		 
   		 READ_PHASE_2: begin	//Read phase 2 values from memory and pad the remaining to make 512 bits
			if(offset==16) begin
			for(j=0;j<num_nonces;j++) begin
				a[j] <= H[j][0];
				b[j] <= H[j][1];
				c[j] <= H[j][2];
				d[j] <= H[j][3];
				e[j] <= H[j][4];
				f[j] <= H[j][5];
				g[j] <= H[j][6];
				h[j] <= H[j][7];
		end
end
		  			  cur_we <= 0;
				phase_ctr <= 2;
		     		 offset <= offset + 1;
				if(offset<34) begin
				for(j=0;j<num_nonces;j++) begin
					for(i=1;i<16;i++) begin
							if(offset<20) begin
							w[j][0]   <= mem_read_data;
						end
							else if(offset == 20) begin
							w[j][0]   <= j;
						end
							else if(offset == 21) begin
							w[j][0]   <= 32'h8000_0000;
						end
							else if(offset<32) begin
							w[j][0]   <= 0;
						end
							else if(offset==32) begin
							w[j][0]   <= 640;
						end
					if(offset<33)		
					w[j][i]	  <= w[j][i-1];
							
					if(offset>17) 
       		 {a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j]} <= sha256_op(a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j], w[j][0], offset-18);
			end
		end
	end
					else begin
					state <= COMPUTE;
		 for(j=0;j<num_nonces;j++) begin
			 for(i=0;i<15;i++) w[j][i+1] <= w[j][i];
   			  w[j][0]	<= word_expand();
			 offset	<= offset - 15;
		end
	end
end
   		 PHASE3: begin //Read the h values from phase 2 and assign to W and pad the remaning 
			if(offset==1) begin
			for(j=0;j<num_nonces;j++) begin
				a[j] <= 32'h6a09e667;
				b[j] <= 32'hbb67ae85;
				c[j] <= 32'h3c6ef372;
				d[j] <= 32'ha54ff53a;
				e[j] <= 32'h510e527f;
				f[j] <= 32'h9b05688c;
				g[j] <= 32'h1f83d9ab;
				h[j] <= 32'h5be0cd19;
		end
end
   			 phase_ctr <= 3;
		  			  cur_we <= 0;
		     		 offset <= offset + 1;
				if(offset<18) begin
				for(j=0;j<num_nonces;j++) begin
					for(i=1;i<16;i++) begin
					if(offset<9) begin
							w[j][0]   <= H[j][offset-1];
		end
					else if(offset == 9) begin
							w[j][0]   <= 32'h8000_0000;
					
	end
					else if(offset < 16) begin
							w[j][0]   <= 0;
			
	end
					else if(offset == 16) begin
							w[j][0]   <= 256;
				 H[j][0] <= 32'h6a09e667;
   				 H[j][1] <= 32'hbb67ae85;
   				 H[j][2] <= 32'h3c6ef372;
   				 H[j][3] <= 32'ha54ff53a;
   				 H[j][4] <= 32'h510e527f;
   				 H[j][5] <= 32'h9b05688c;
   				 H[j][6] <= 32'h1f83d9ab;
   				 H[j][7] <= 32'h5be0cd19;
							
		end
					if(offset<17)		
					w[j][i]	  <= w[j][i-1];
					if(offset>1) 
       		 {a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j]} <= sha256_op(a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j], w[j][0], offset-2);
	
		 	end
		end
	end
					else begin
					state <= COMPUTE;
		 for(j=0;j<num_nonces;j++) begin
			 for(i=0;i<15;i++) w[j][i+1] <= w[j][i];
   			  w[j][0]	<= word_expand();
		end
									
	end
   			 
 end
    
   		 COMPUTE: begin //Compute for final has values for each phase
     		   offset <= offset + 1;
   	  if(offset<67) begin
		 for(j=0;j<num_nonces;j++) begin
			 for(i=0;i<15;i++) w[j][i+1] <= w[j][i];
   			  w[j][0]	<= word_expand();
       		 {a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j]} <= sha256_op(a[j], b[j], c[j], d[j], e[j], f[j], g[j], h[j], w[j][0], offset-3);
end
end
   	 else if(offset == 67) begin
		  for(j=0;j<num_nonces;j++) begin
				 H[j][0] <= H[j][0] + a[j];
				 H[j][1] <= H[j][1] + b[j];
				 H[j][2] <= H[j][2] + c[j];
				 H[j][3] <= H[j][3] + d[j];
				 H[j][4] <= H[j][4] + e[j];
				 H[j][5] <= H[j][5] + f[j];
				 H[j][6] <= H[j][6] + g[j];
				 H[j][7] <= H[j][7] + h[j];
				 end
end
	else if(offset == 68) begin
   				 ph1_h0    <= H[0][0];
   				 ph1_h1    <= H[1][0];
   				 ph1_h2    <= H[2][0];
   				 ph1_h3    <= H[3][0];
   				 ph1_h4    <= H[4][0];
   				 ph1_h5    <= H[5][0];
   				 ph1_h6    <= H[6][0];
   				 ph1_h7    <= H[7][0];
end
    else if(offset == 69) begin
   		 if (phase_ctr == 1) begin
   				 state <= READ_PHASE_2;
				 offset	   <= 16;
   			 H[j][0]    <= ph1_h0;
   			 H[j][1]    <= ph1_h1;
   			 H[j][2]    <= ph1_h2;
   			 H[j][3]    <= ph1_h3;
   			 H[j][4]    <= ph1_h4;
   			 H[j][5]    <= ph1_h5;
   			 H[j][6]    <= ph1_h6;
   			 H[j][7]    <= ph1_h7;
   			 end
   					 
   		 else if (phase_ctr == 2) begin
   				 state <= PHASE3;
				 offset <= 1;
   			 end
   		 else if (phase_ctr==3) begin
   				 state <= FETCH;   				 
   			 end
   				 
   		 else state<= COMPUTE;
   			 end
end		 
   		 FETCH: begin	//used to fetch final has values 
			  for(j=0;j<num_nonces;j++) begin
   			 hout0[j] <= H[j][0];
   		end
				    state <= WRITE;
   				 cur_addr <= output_addr;
   				 offset <= 0;
   		 
   		 end
   		 
   		 WRITE:begin	//write the final hash values into the memory
   			 cur_we <= 1;
				if(wr_ptr<16) begin
					cur_addr <= cur_we ? cur_addr  + 1 : cur_addr;
					wr_ptr <= wr_ptr + 1;
					cur_write_data <= hout0[wr_ptr];
   				 state <= WRITE;
				end
   			 else begin
   				 state <= IDLE;
   			 end
   			 
   		 end
   	 endcase
    end
end

assign done = (state == IDLE);
endmodule
