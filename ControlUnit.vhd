LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_unsigned.all; -- needed for CONV_INTEGER()
ENTITY ctrl IS PORT (
	clk_ctrl: IN std_logic;
	rst_ctrl: IN std_logic;
	muxsel_ctrl: OUT std_logic_vector(1 DOWNTO 0);
	imm_ctrl: OUT std_logic_vector(7 DOWNTO 0);
	accwr_ctrl: OUT std_logic;
	rfaddr_ctrl: OUT std_logic_vector(2 DOWNTO 0);
	rfwr_ctrl: OUT std_logic;
	alusel_ctrl: OUT std_logic_vector(2 DOWNTO 0);
	shiftsel_ctrl: OUT std_logic_vector(1 DOWNTO 0);
	outen_ctrl: OUT std_logic;
	zero_ctrl: IN std_logic;
	positive_ctrl: IN std_logic);
END ctrl;
ARCHITECTURE fsm OF ctrl IS
	TYPE state_type IS
		(S1,S2,S8,S9,S10,S11,S12,S13,S14,S210,S220,S230,S240,S30,S31,S32,S33,S41,S42,S43,S44,S45,S46,S51,S52,S99);
	SIGNAL state: state_type;
-- Instructions
-- load instructions
	CONSTANT LDA : std_logic_vector(3 DOWNTO 0) := "0001";
-- S10 -- OK
	CONSTANT STA : std_logic_vector(3 DOWNTO 0) := "0010";
-- S11 -- OK
	CONSTANT LDM : std_logic_vector(3 DOWNTO 0) := "0011";
-- S12
	CONSTANT STM : std_logic_vector(3 DOWNTO 0) := "0100";
-- S13
	CONSTANT LDI : std_logic_vector(3 DOWNTO 0) := "0101";
-- S14 -- OK
-- jump instructions
	CONSTANT JMP : std_logic_vector(3 DOWNTO 0) := "0110";
-- S210 -- OK
--CONSTANT JMPR : std_logic_vector(3 DOWNTO 0) := "0110";
--the relativejumps are determined by the 4 LSBs
	CONSTANT JZ : std_logic_vector(3 DOWNTO 0) := "0111";
-- S220
--CONSTANT JZR : std_logic_vector(3 DOWNTO 0) := "0111";
	CONSTANT JNZ : std_logic_vector(3 DOWNTO 0) := "1000";
-- S230
--CONSTANT JNZR : std_logic_vector(3 DOWNTO 0) := "1000";
	CONSTANT JP : std_logic_vector(3 DOWNTO 0) := "1001";
-- S240 changed
--CONSTANT JPR : std_logic_vector(3 DOWNTO 0) := "1001";
	-- arithmetic and logical instructions
	CONSTANT ANDA : std_logic_vector(3 DOWNTO 0) := "1010";
-- S30 -- OK
	CONSTANT ORA : std_logic_vector(3 DOWNTO 0) := "1011";
-- S31 -- OK
	CONSTANT ADD : std_logic_vector(3 DOWNTO 0) := "1100";
-- S32 -- OK
	CONSTANT SUB : std_logic_vector(3 DOWNTO 0) := "1101";
-- S33 -- OK
	-- single operand instructions
	CONSTANT SOI : std_logic_vector(3 DOWNTO 0) := "1110";
		CONSTANT NOTA : std_logic_vector(2 DOWNTO 0) := "000";
-- S41 -- OK
		CONSTANT INC : std_logic_vector(2 DOWNTO 0) := "001";
-- S42 -- OK
		CONSTANT DEC : std_logic_vector(2 DOWNTO 0) := "010";
-- S43 -- OK
		CONSTANT SHFL : std_logic_vector(2 DOWNTO 0) := "011";
-- S44 -- OK
		CONSTANT SHFR : std_logic_vector(2 DOWNTO 0) := "100";
-- S45 -- OK
		CONSTANT ROTR : std_logic_vector(2 DOWNTO 0) := "101";
-- S46
	-- Input / Output and Miscellaneous instructions
	CONSTANT MISC : std_logic_vector(3 DOWNTO 0) := "1111";
--
		CONSTANT INA : std_logic_vector(1 DOWNTO 0) := "00";
-- S51
		CONSTANT OUTA : std_logic_vector(1 DOWNTO 0) := "01";
-- S52
		CONSTANT HALT : std_logic_vector(1 DOWNTO 0) := "10";
-- S99
	CONSTANT NOP : std_logic_vector(3 DOWNTO 0) := "0000";
-- S1
	TYPE PM_BLOCK IS ARRAY(0 TO 31) OF std_logic_vector(7 DOWNTO 0);
BEGIN
PROCESS (rst_ctrl,clk_ctrl)
	VARIABLE PM : PM_BLOCK;
	VARIABLE IR : std_logic_vector(7 DOWNTO 0);
	VARIABLE OPCODE : std_logic_vector( 3 DOWNTO 0);
	VARIABLE PC : integer RANGE 0 TO 31;
	VARIABLE zero_flag, positive_flag: std_logic;
BEGIN 
--Termina parte de abel
	 IF (rst_ctrl='1') THEN
	PC := 0;
	muxsel_ctrl <= "00";
	imm_ctrl <= (OTHERS => '0');
	accwr_ctrl <= '0';
	rfaddr_ctrl <= "000";
	rfwr_ctrl <= '0';
	alusel_ctrl <= "000";
	shiftsel_ctrl <= "00";
	outen_ctrl <= '0';
	state <= S1;

	-- load program memory with statements
	-- Multiplication programAxB
	PM(0) := "01010000"; -- LDI A,0
	PM(1) := "00000000"; -- constant 0
	PM(2) := "00100000"; -- STA R[0],A
	PM(3) := "01010000"; -- LDI A,13
	PM(4) := "00001101"; -- constant 13
	PM(5) := "00100001"; -- STA R[1],A
	PM(6) := "11110000"; -- IN A
	PM(7) := "00100010"; -- STA R[2],A
	PM(8) := "01110000"; -- JZ out
	PM(9) := "00010001";
	PM(10) := "00010000"; -- repeat: LDA A,R[0]
	PM(11) := "11000001"; -- ADD A,R[1]
	PM(12) := "00100000"; -- STA R[0],A
	PM(13) := "00010010"; -- LDA A,R[2]
	PM(14) := "11100010"; -- DEC A
	PM(15) := "00100010"; -- STA R[2],A
	PM(16) := "10001110"; -- JNZR repeat
	PM(17) := "00010000"; -- LDA A,R[0] output answer
	PM(18) := "11110001"; -- OUT A
	PM(19) := "11110010"; -- HALT

  ELSIF (clk_ctrl'event and clk_ctrl = '1') THEN
	CASE state IS
			WHEN S1 => -- fetch instruction
	    IR := PM(PC);
				OPCODE := IR(7 DOWNTO 4);
				PC := PC + 1;
		muxsel_ctrl <= "00";
		imm_ctrl <= (OTHERS => '0');
		accwr_ctrl <= '0';
		rfaddr_ctrl <= "000";
		rfwr_ctrl <= '0';
		alusel_ctrl <= "000";
		shiftsel_ctrl <= "00";
		outen_ctrl <= '0';
				state <= S2;
			  WHEN S2 => -- decode instruction
				CASE OPCODE IS
			WHEN NOP => state <= S1;
				WHEN LDA => state <= S10;
		WHEN STA => state <= S11;
		WHEN LDM => state <= S12;
		WHEN STM => state <= S13;
		WHEN LDI => state <= S14;
		WHEN JMP => state <= S210;
		WHEN JZ => state <= S220;
		WHEN JNZ => state <= S230;
		WHEN JP => state <= S240;
		WHEN ANDA => state <= S30;
		WHEN ORA => state <= S31;
		WHEN ADD => state <= S32;
		WHEN SUB => state <= S33;
		WHEN SOI => -- single operand instructions
	    CASE IR(2 DOWNTO 0) IS
		  WHEN NOTA => state <= S41;
		  WHEN INC => state <= S42;
		  WHEN DEC => state <= S43;
		  WHEN SHFL => state <= S44;
		  WHEN SHFR => state <= S45;
		  WHEN ROTR => state <= S46;
		  WHEN OTHERS => state <= S99;
	    END CASE;
	WHEN MISC => -- I/O and miscellaneous instructions
	 CASE IR(1 DOWNTO 0) IS
		WHEN INA => state <= S51;
		WHEN OUTA => state <= S52;
		 WHEN HALT => state <= S99;
		WHEN OTHERS => state <= S99;
	  END CASE;
		   WHEN OTHERS => state <= S99;
		 END CASE;
	muxsel_ctrl <= "00";
	imm_ctrl <= (OTHERS => '0');
	accwr_ctrl <= '0';
	rfaddr_ctrl <= "000";
	rfwr_ctrl <= '0';
	alusel_ctrl <= "000";
	shiftsel_ctrl <= "00";
	outen_ctrl <= '0';

  WHEN S8 => -- set zero and positive flags and then goto next instruction
	
	muxsel_ctrl <= "00";
	imm_ctrl <= (OTHERS => '0');
	accwr_ctrl <= '0';
	rfaddr_ctrl <= "000";
	rfwr_ctrl <= '0';
	alusel_ctrl <= "000";
	shiftsel_ctrl <= "00";
	outen_ctrl <= '0';
	state <= S1;

	zero_flag := zero_ctrl;
	positive_flag := positive_ctrl;

  WHEN S9 => -- next instruction
	muxsel_ctrl <= "00";
	imm_ctrl <= (OTHERS => '0');
	accwr_ctrl <= '0';
	rfaddr_ctrl <= "000";
--Termina parte de Alexis

rfwr_ctrl <= '0';
alusel_ctrl <= "000";
shiftsel_ctrl <= "00";
outen_ctrl <= '0';
state <= S1;

			WHEN S10 =>--LDA --OK
		muxsel_ctrl <= "01";
		imm_ctrl <= (OTHERS => '0');
		accwr_ctrl <= '1';
		rfaddr_ctrl <= IR (2 DOWNTO 0);
		rfwr_ctrl <= '0';
		alusel_ctrl <= "000";
		shiftsel_ctrl <= "00";
		outen_ctrl <= '0';
		state <= S8;
		
	WHEN S11 => --STA --OK
	 muxsel_ctrl <= "00";
	 imm_ctrl <= (OTHERS => '0');
	 accwr_ctrl <= '0';
	 rfaddr_ctrl <= IR (2 DOWNTO 0);
	 rfwr_ctrl <= '1';
	 alusel_ctrl <= "000";
	 shiftsel_ctrl <= "00";
	 outen_ctrl <= '0';
	 state <= S1;
	 
	 WHEN S12 => --LDM
     muxsel_ctrl <= "10";
	  imm_ctrl <= (OTHERS => '0');
	  accwr_ctrl <= '1';
	  rfaddr_ctrl <= "000";
	  rfwr_ctrl <= '1';
	  alusel_ctrl <= "000";
	  shiftsel_ctrl <= "00";
	  outen_ctrl <= '0';
	  state <= S9;
	  
	  WHEN S13 => --STM
      muxsel_ctrl <= "00";
	   imm_ctrl <= (OTHERS => '0');
	   accwr_ctrl <= '0';
	   rfaddr_ctrl <= "000";
	   rfwr_ctrl <= '0';
	   alusel_ctrl <= "000";
	   shiftsel_ctrl <= "00";
	   outen_ctrl <= '0';
	   state <= S9;
		
	  WHEN S14 => --LDI --OK
      muxsel_ctrl <= "11";
	   imm_ctrl <= PM(PC);
		PC := PC + 1;
	   accwr_ctrl <= '1';
	   rfaddr_ctrl <= "000";
	   rfwr_ctrl <= '0';
	   alusel_ctrl <= "000";
	   shiftsel_ctrl <= "00";
	   outen_ctrl <= '0';
	   state <= S8;
		
	   WHEN S210 => --JMP OK
		  IF (IR(3 DOWNTO 0) = "0000") THEN
			--absolute
				IR := PM(PC); --get next byte for absolute address
				PC := CONV_INTEGER (IR(4 DOWNTO 0));
			ELSIF (IR(3) = '0') THEN --relative positive
				-- minus 1 because PC has already incremented
				PC := PC + CONV_INTEGER("00" & IR(2 DOWNTO 0)) -1;
			 ELSE-- relative negative
				PC := PC - CONV_INTEGER("00" & IR(2 DOWNTO 0)) -1;
			 END IF;
			 
			muxsel_ctrl <= "00";
			imm_ctrl <= (OTHERS => '0');
			accwr_ctrl <= '0';
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <= "000";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			state <= S1;
			
		 WHEN S220 => --JMP OK
		  IF (zero_flag= '1') THEN --may need TO USE zero_flag instead
			--absolute
			IF (IR(3 DOWNTO 0) = "0000") THEN
				IR := PM(PC); --get next byte for absolute address
				PC := CONV_INTEGER (IR(4 DOWNTO 0));
			ELSIF (IR(3) = '0') THEN --relative positive
				-- minus 1 because PC has already incremented
				PC := PC + CONV_INTEGER("00" & IR(2 DOWNTO 0)) -1;
			 ELSE-- relative negative
				PC := PC - CONV_INTEGER("00" & IR(2 DOWNTO 0)) -1;
			 END IF;
			END IF;
			
			muxsel_ctrl <= "00";
			imm_ctrl <= (OTHERS => '0');
			accwr_ctrl <= '0';
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <= "000";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			state <= S1;
			
--Termina parte de Charly
			
		WHEN S230 => --JMP OK
			IF (zero_flag= '0') THEN --may need TO USE zero_flag instead
				IF (IR(3 DOWNTO 0) = "0000") THEN
			--absolute
				IR := PM(PC); --get next byte for absolute address
				PC := CONV_INTEGER (IR(4 DOWNTO 0));
			ELSIF (IR(3) = '0') THEN  --relative positive
			 --minus 1 because PC has already incremented
				PC := PC + CONV_INTEGER ("00" & IR(2 DOWNTO 0)) - 1;
			ELSE --relative negative
				PC := PC - CONV_INTEGER ("00" & IR(2 DOWNTO 0)) - 1;
			END IF;
		END IF;
		
			muxsel_ctrl <= "00";
			imm_ctrl <= (OTHERS => '0');
			accwr_ctrl <= '0';
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <= "000";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			state <= S1;
		WHEN S240 => -- JP
			IF (positive_flag='1') THEN -- may need TO USE positive_flag instead
				IF (IR(3 DOWNTO 0) = "0000") THEN
					-- absolute
					IR := PM(PC); -- get next byte for absolute address
					PC := CONV_INTEGER(IR(4 DOWNTO 0));
				ELSIF (IR(3) = '0') THEN -- relative positive
					 -- minus 1 because PC has already incremented
					PC := PC + CONV_INTEGER("00" & IR(2 DOWNTO 0)) - 1;
				ELSE -- relative negative
					PC := PC - CONV_INTEGER("00" & IR(2 DOWNTO 0)) - 1;
				END IF;
			END IF;

			muxsel_ctrl <= "00";
			imm_ctrl <= (OTHERS => '0');
			accwr_ctrl <= '0';
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <= "000";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			state <= S1;

		WHEN S30 => -- ANDA -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');
			rfaddr_ctrl <= IR(2 DOWNTO 0);
			rfwr_ctrl <= '0';
			alusel_ctrl <="001";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result ??

		WHEN S31 => -- ORA -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');  
			rfaddr_ctrl <= IR(2 DOWNTO 0);
			rfwr_ctrl <= '0';
			alusel_ctrl <="010";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result
		WHEN S32 => -- ADD -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');
			rfaddr_ctrl <= IR(2 DOWNTO 0);
			rfwr_ctrl <= '0';
			alusel_ctrl <="100";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result
		WHEN S33 => -- SUB -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');
			rfaddr_ctrl <= IR(2 DOWNTO 0);
			rfwr_ctrl <= '0';
			alusel_ctrl <="101";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result
		WHEN S41 => -- NOTA -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <="011";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result
		WHEN S42 => -- INC -- OK
			muxsel_ctrl <="00";
			imm_ctrl <= (OTHERS => '0');
			rfaddr_ctrl <= "000";
			rfwr_ctrl <= '0';
			alusel_ctrl <="110";
			shiftsel_ctrl <= "00";
			outen_ctrl <= '0';
			accwr_ctrl <='1'; -- write occurs IN the next cycle
			state <= S8;
--			state <= S9; -- need one extra cycle TO write back result
	
				
--Termina parte de Wilber

--Inicia parte de Christiam
 When S43 => -- DEC    -- OK
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "111";
   shiftsel_ctrl <= "00";
   outen_ctrl <= '0';
   accwr_ctrl <= '1';  -- write occurs IN the next cycle
   state <= S8;
   state <= S9;  -- need one extra cycle TO write back result

  When S44 => -- SHFL
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000"; -- pass
   shiftsel_ctrl <= "01";
   outen_ctrl <= '0';
   accwr_ctrl <= '1';  -- write occurs IN the next cycle
   state <= S8;
   state <= S9;  -- need one extra cycle TO write back result

  When S45 => -- SHFR    -- OK
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000"; -- pass
   shiftsel_ctrl <= "10";
   outen_ctrl <= '0';
   accwr_ctrl <= '1';  -- write occurs IN the next cycle
   state <= S8;
   state <= S9;  -- need one extra cycle TO write back result

  When S46 => -- ROTR    -- ??
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000"; -- pass
   shiftsel_ctrl <= "11";
   outen_ctrl <= '0';
   accwr_ctrl <= '1';  -- write occurs IN the next cycle
   state <= S8;
   state <= S9;  -- need one extra cycle TO write back result

  When S51 => -- INA
   muxsel_ctrl <= "10";
   imm_ctrl <= (OTHERS => '0');
   accwr_ctrl <= '1';
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000";
   shiftsel_ctrl <= "00";
   outen_ctrl <= '0';
   state <= S8;
   state <= S9;

  When S52 => -- OUTA
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   accwr_ctrl <= '0';
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000";
   shiftsel_ctrl <= "00";
   outen_ctrl <= '1';
   state <= S1;
   state <= S9;

  When S99 => -- HALT
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   accwr_ctrl <= '0';
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000";
   shiftsel_ctrl <= "00";
   outen_ctrl <= '0';
         state <= S9;

  When OTHERS =>
   muxsel_ctrl <= "00";
   imm_ctrl <= (OTHERS => '0');
   accwr_ctrl <= '0';
   rfaddr_ctrl <= "000";
   rfwr_ctrl <= '0';
   alusel_ctrl <= "000";
   shiftsel_ctrl <= "00";
   outen_ctrl <= '0';
   state <= S99;
    END CASE;
 END IF;
END PROCESS;
END fsm;