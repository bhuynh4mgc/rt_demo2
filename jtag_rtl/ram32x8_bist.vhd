--
--  Tue Jul  8 12:04:09 2008
--  MBISTArchitect v8.2008_1.10  Tue Feb 26 21:08:59 GMT 2008
--  VHDL model:  ram32x8_bist
--  Ram Pattern Generator State Machine
--      With Output Comparator
--  Test Types: march2
--  DataWidth = 8
--

library ieee;
use ieee.std_logic_1164.all;

ENTITY ram32x8_bist IS
	PORT(
		Test_A_0: out STD_ULOGIC_VECTOR(4 downto 0);
		Test_DI_0: out STD_ULOGIC_VECTOR(7 downto 0);
		Test_WEN_0: out STD_ULOGIC;
		tst_done: out STD_ULOGIC;
		fail_h: out STD_ULOGIC;
		Test_DO_0: in STD_ULOGIC_VECTOR(7 downto 0);
		test_h: in STD_ULOGIC;
		bist_clk: in STD_ULOGIC;
		rst_l: in STD_ULOGIC);
END ram32x8_bist;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;


ARCHITECTURE behavior OF ram32x8_bist is      -- <<< JTAG_132

	constant start_state: STD_ULOGIC_VECTOR(2 downto 0) := "000";
	constant s_march2_march2_wBackgroundUp_0: STD_ULOGIC_VECTOR(2 downto 0) := "001";
	constant s_march2_march2_rwrInvBackgroundUp_0: STD_ULOGIC_VECTOR(2 downto 0) := "011";
	constant s_march2_march2_rwrBackgroundUp_0: STD_ULOGIC_VECTOR(2 downto 0) := "010";
	constant s_march2_march2_rwrInvBackgroundDown_0: STD_ULOGIC_VECTOR(2 downto 0) := "110";
	constant s_march2_march2_rwrBackgroundDown_0: STD_ULOGIC_VECTOR(2 downto 0) := "111";
	constant s_march2_march2_rBackgroundDown_0: STD_ULOGIC_VECTOR(2 downto 0) := "101";
	constant complete_state: STD_ULOGIC_VECTOR(2 downto 0) := "100";
	constant md_none: STD_ULOGIC_VECTOR(1 downto 0) := "00";
	constant md_r: STD_ULOGIC_VECTOR(1 downto 0) := "01";
	constant md_rwr: STD_ULOGIC_VECTOR(1 downto 0) := "10";
	constant md_w: STD_ULOGIC_VECTOR(1 downto 0) := "11";
	constant p_background1: STD_ULOGIC_VECTOR(7 downto 0) := "00000000";
	constant AddrNoOp: STD_ULOGIC_VECTOR(1 downto 0) := "00";
	constant AddrOp_Unary_0_31_up_5_31: STD_ULOGIC_VECTOR(1 downto 0) := "01";
	constant AddrOp_Unary_31_0_down_5_31: STD_ULOGIC_VECTOR(1 downto 0) := "10";

--  Internal Registers and Signals

	signal tst_done_var: STD_ULOGIC;
	signal tst_done_reg: STD_ULOGIC;
	signal mode_var: STD_ULOGIC_VECTOR( 1 downto 0);
	signal rw_state: STD_ULOGIC_VECTOR( 2 downto 0);
	signal tstate: STD_ULOGIC_VECTOR( 2 downto 0);
	signal new_tstate: STD_ULOGIC_VECTOR( 2 downto 0);
	signal mbist_active_background: STD_ULOGIC_VECTOR( 7 downto 0);
	signal tmp_A_0: STD_ULOGIC_VECTOR( 4 downto 0);
	signal tmp_DI_0: STD_ULOGIC_VECTOR( 7 downto 0);
	signal Test_tmp_WEN_0: STD_ULOGIC;
	signal addr_reg: STD_ULOGIC_VECTOR( 4 downto 0);
	signal addr_reg_start_var: STD_ULOGIC_VECTOR( 4 downto 0);
	signal expect_DO_0: STD_ULOGIC_VECTOR( 7 downto 0);
	signal compare_DO_0: STD_ULOGIC;
	signal wen_state: STD_ULOGIC;
	signal addr_op_var: STD_ULOGIC_VECTOR( 1 downto 0);
	signal pat_var: STD_ULOGIC;
	signal rslt_reg: STD_ULOGIC;
	signal new_rslt_reg: STD_ULOGIC;
	signal DO_0: STD_ULOGIC_VECTOR( 7 downto 0);
	signal background1: STD_ULOGIC_VECTOR( 7 downto 0);

function "+" (vecA :  IN STD_ULOGIC_VECTOR ; vecB :  IN STD_ULOGIC_VECTOR)   return STD_ULOGIC_VECTOR is
   variable sum: STD_ULOGIC_VECTOR(vecA'length - 1 downto 0);
   begin
      sum := to_stdulogicvector(to_stdlogicvector(vecA) + to_stdlogicvector(vecB));
   return sum;
end "+" ;


begin


background1 <= p_background1;



--   Merge Inputs

  DO_0 <= Test_DO_0;




--  Assign from Active Background
ActiveBackground : PROCESS (mbist_active_background)
BEGIN
   tmp_DI_0 <= mbist_active_background;
END PROCESS ActiveBackground;


--  AssignOutputs

AssignAddressOutputs : PROCESS (addr_reg)
BEGIN
   tmp_A_0 <= addr_reg;
END PROCESS AssignAddressOutputs;

AssignDataOutputs : PROCESS (background1, pat_var)
BEGIN
   CASE pat_var
   IS
      WHEN '0' => 
         mbist_active_background <= background1;
      WHEN '1' => 
         mbist_active_background <= NOT background1;
      WHEN OTHERS =>
         mbist_active_background <= "00000000";
   END CASE;
END PROCESS AssignDataOutputs;

AssignControlOutputs : PROCESS (wen_state)
BEGIN
   Test_tmp_WEN_0 <= NOT wen_state;
END PROCESS AssignControlOutputs;

AssignFailHTestDoneOutput : PROCESS (rslt_reg, tst_done_reg)
BEGIN
   tst_done <= tst_done_reg;
   fail_h <= rslt_reg;
END PROCESS AssignFailHTestDoneOutput;


	Test_A_0 <= tmp_A_0;
	Test_DI_0 <= tmp_DI_0;
	Test_WEN_0 <= Test_tmp_WEN_0;


--  Comparator
mbist_expect_process : PROCESS (background1, mode_var, 
   pat_var, rw_state, test_h)
BEGIN
   IF test_h = '1'
   THEN
      CASE pat_var
      IS
         WHEN '0' => 
            CASE mode_var
            IS
               WHEN md_none => 
                  expect_DO_0 <= "00000000";
               WHEN md_r => 
                  expect_DO_0 <= background1;
               WHEN md_rwr => 
                  IF rw_state <= "001"
                  THEN
                     expect_DO_0 <= NOT background1;
                  ELSIF rw_state > "001"
                  THEN
                     expect_DO_0 <= background1;
                  ELSE
                     expect_DO_0 <= "00000000";
                  END IF;
               WHEN md_w => 
                  expect_DO_0 <= "00000000";
               WHEN OTHERS =>
                  expect_DO_0 <= "00000000";
            END CASE;
         WHEN '1' => 
            CASE mode_var
            IS
               WHEN md_none => 
                  expect_DO_0 <= "00000000";
               WHEN md_r => 
                  expect_DO_0 <= NOT background1;
               WHEN md_rwr => 
                  IF rw_state <= "001"
                  THEN
                     expect_DO_0 <= background1;
                  ELSIF rw_state > "001"
                  THEN
                     expect_DO_0 <= NOT background1;
                  ELSE
                     expect_DO_0 <= "00000000";
                  END IF;
               WHEN md_w => 
                  expect_DO_0 <= "00000000";
               WHEN OTHERS =>
                  expect_DO_0 <= "00000000";
            END CASE;
         WHEN OTHERS =>
            expect_DO_0 <= "00000000";
      END CASE;
   ELSE
      expect_DO_0 <= "00000000";
   END IF;
END PROCESS mbist_expect_process;
mbist_compare_process : PROCESS (DO_0, expect_DO_0, test_h)
BEGIN
   IF test_h = '1'
   THEN
      IF DO_0 = expect_DO_0
      THEN
         compare_DO_0 <= '1';
      ELSE
         compare_DO_0 <= '0';
      END IF;
   ELSE
      compare_DO_0 <= '1';
   END IF;
END PROCESS mbist_compare_process;
mbist_collate_process : PROCESS (compare_DO_0, rslt_reg, 
   test_h, tstate)
BEGIN
   IF test_h = '1'
   THEN
      CASE tstate
      IS
         WHEN start_state => 
            new_rslt_reg <= '0';
         WHEN s_march2_march2_wBackgroundUp_0 => 
            new_rslt_reg <= rslt_reg;
         WHEN s_march2_march2_rwrInvBackgroundUp_0 => 
            IF (compare_DO_0 = '1')
            THEN
               new_rslt_reg <= rslt_reg;
            ELSE
               new_rslt_reg <= '1';
            END IF;
         WHEN s_march2_march2_rwrBackgroundUp_0 => 
            IF (compare_DO_0 = '1')
            THEN
               new_rslt_reg <= rslt_reg;
            ELSE
               new_rslt_reg <= '1';
            END IF;
         WHEN s_march2_march2_rwrInvBackgroundDown_0 => 
            IF (compare_DO_0 = '1')
            THEN
               new_rslt_reg <= rslt_reg;
            ELSE
               new_rslt_reg <= '1';
            END IF;
         WHEN s_march2_march2_rwrBackgroundDown_0 => 
            IF (compare_DO_0 = '1')
            THEN
               new_rslt_reg <= rslt_reg;
            ELSE
               new_rslt_reg <= '1';
            END IF;
         WHEN s_march2_march2_rBackgroundDown_0 => 
            IF (compare_DO_0 = '1')
            THEN
               new_rslt_reg <= rslt_reg;
            ELSE
               new_rslt_reg <= '1';
            END IF;
         WHEN complete_state => 
            new_rslt_reg <= rslt_reg;
         WHEN OTHERS =>
            new_rslt_reg <= rslt_reg;
      END CASE;
   ELSE
      new_rslt_reg <= rslt_reg;
   END IF;
END PROCESS mbist_collate_process;


--  Next State
next_state : PROCESS (tstate)
BEGIN
   CASE tstate
   IS
      WHEN start_state => 
         addr_op_var <= AddrNoOp;
         addr_reg_start_var <= "00000";
         mode_var <= md_none;
         new_tstate <= s_march2_march2_wBackgroundUp_0;
         pat_var <= '0';
         tst_done_var <= '0';
      WHEN s_march2_march2_wBackgroundUp_0 => 
         addr_op_var <= AddrOp_Unary_0_31_up_5_31;
         addr_reg_start_var <= "00000";
         mode_var <= md_w;
         new_tstate <= s_march2_march2_rwrInvBackgroundUp_0;
         pat_var <= '0';
         tst_done_var <= '0';
      WHEN s_march2_march2_rwrInvBackgroundUp_0 => 
         addr_op_var <= AddrOp_Unary_0_31_up_5_31;
         addr_reg_start_var <= "00000";
         mode_var <= md_rwr;
         new_tstate <= s_march2_march2_rwrBackgroundUp_0;
         pat_var <= '1';
         tst_done_var <= '0';
      WHEN s_march2_march2_rwrBackgroundUp_0 => 
         addr_op_var <= AddrOp_Unary_0_31_up_5_31;
         addr_reg_start_var <= "11111";
         mode_var <= md_rwr;
         new_tstate <= 
            s_march2_march2_rwrInvBackgroundDown_0;
         pat_var <= '0';
         tst_done_var <= '0';
      WHEN s_march2_march2_rwrInvBackgroundDown_0 => 
         addr_op_var <= AddrOp_Unary_31_0_down_5_31;
         addr_reg_start_var <= "11111";
         mode_var <= md_rwr;
         new_tstate <= s_march2_march2_rwrBackgroundDown_0;
         pat_var <= '1';
         tst_done_var <= '0';
      WHEN s_march2_march2_rwrBackgroundDown_0 => 
         addr_op_var <= AddrOp_Unary_31_0_down_5_31;
         addr_reg_start_var <= "11111";
         mode_var <= md_rwr;
         new_tstate <= s_march2_march2_rBackgroundDown_0;
         pat_var <= '0';
         tst_done_var <= '0';
      WHEN s_march2_march2_rBackgroundDown_0 => 
         addr_op_var <= AddrOp_Unary_31_0_down_5_31;
         addr_reg_start_var <= "00000";
         mode_var <= md_r;
         new_tstate <= complete_state;
         pat_var <= '0';
         tst_done_var <= '0';
      WHEN complete_state => 
         addr_op_var <= AddrNoOp;
         addr_reg_start_var <= "00000";
         mode_var <= md_none;
         new_tstate <= complete_state;
         pat_var <= '0';
         tst_done_var <= '1';
      WHEN OTHERS =>
         addr_op_var <= AddrNoOp;
         addr_reg_start_var <= "00000";
         mode_var <= md_none;
         new_tstate <= complete_state;
         pat_var <= '0';
         tst_done_var <= '0';
   END CASE;
END PROCESS next_state;

--  Update the registers


update: process ( bist_clk, rst_l)
begin
	if (rst_l = '0') then
	   tstate <= "000";
	   addr_reg <= "11111";
	   rw_state <= "000";
	   wen_state <= '0';
	   tst_done_reg <= '0';
	   rslt_reg <= '0';
	elsif (bist_clk'event and bist_clk = '1') then
	   tst_done_reg <= tst_done_var;

	   if ((test_h = '1') and (tstate /= complete_state)) then
	      if (rw_state = "101") then
	         rw_state <= "000";
	      elsif ((mode_var = md_r) and (rw_state = "001")) then
	         rw_state <= "000";
	      elsif ((mode_var = md_w) and (rw_state = "001")) then
	         rw_state <= "000";
	      elsif ((mode_var = md_none) and (rw_state = "000")) then
	         rw_state <= "000";
	      else
	         rw_state <= to_stdulogicvector(to_stdlogicvector(rw_state) + "001");
	      end if;
	      if (((rw_state = "000") and (mode_var = md_w)) or ((rw_state = "010") and 
	         (mode_var = md_rwr))) then
	         wen_state <= '1';
	      else
	         wen_state <= '0';
	      end if;
	      if (((((mode_var = md_r) and (rw_state = "001")) or ((mode_var = md_rwr) and 
	         (rw_state = "101"))) or ((mode_var = md_w) and (rw_state = "001"))) or 
	         ((mode_var = md_none) and (rw_state = "000"))) then
                 CASE addr_op_var
                 IS
                    WHEN AddrOp_Unary_0_31_up_5_31 => 
                       IF addr_reg = "11111"
                       THEN
                          addr_reg <= addr_reg_start_var;
                          tstate <= new_tstate;
                       ELSE
                          addr_reg <= 
                             to_stdulogicvector(to_stdlogicvector(addr_reg) 
                             + "00001");
                       END IF;
                    WHEN AddrOp_Unary_31_0_down_5_31 => 
                       IF addr_reg = "00000"
                       THEN
                          addr_reg <= addr_reg_start_var;
                          tstate <= new_tstate;
                       ELSE
                          addr_reg <= 
                             to_stdulogicvector(to_stdlogicvector(addr_reg) 
                             - "00001");
                       END IF;
                    WHEN AddrNoOp => 
                       addr_reg <= addr_reg_start_var;
                       tstate <= new_tstate;
                    WHEN OTHERS =>
                       addr_reg <= addr_reg_start_var;
                       tstate <= new_tstate;
                 END CASE;

	      end if;
	   end if;
	   if ((test_h = '1') and (tstate /= complete_state)) then
	      if ((((rw_state = "000") and (mode_var = md_r)) or ((rw_state = "000") and 
	         (mode_var = md_rwr))) or ((rw_state = "100") and 
	         (mode_var = md_rwr))) then
	         rslt_reg <= new_rslt_reg;
	      end if;
	   end if;
	end if;
end process update;

end behavior;



--  Memory Collar Block for: ram32x8_bist_ram32x8_block
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;
ENTITY ram32x8_bist_ram32x8_block IS
   PORT(
      DO: out STD_ULOGIC_VECTOR(7 downto 0);
      test_DO: out STD_ULOGIC_VECTOR(7 downto 0);
      test_h: in STD_ULOGIC;
      A: in STD_ULOGIC_VECTOR(4 downto 0);
      test_A: in STD_ULOGIC_VECTOR(4 downto 0);
      DI: in STD_ULOGIC_VECTOR(7 downto 0);
      test_DI: in STD_ULOGIC_VECTOR(7 downto 0);
      WEN: in STD_ULOGIC;
      test_WEN: in STD_ULOGIC);
END ram32x8_bist_ram32x8_block;

ARCHITECTURE behavior OF ram32x8_bist_ram32x8_block IS    -- <<< JTAG_132
   COMPONENT ram32x8
      PORT(
         DO: out STD_LOGIC_VECTOR(7 downto 0);
         A: in STD_LOGIC_VECTOR(4 downto 0);
         DI: in STD_LOGIC_VECTOR(7 downto 0);
         WEN: in STD_LOGIC);
   END COMPONENT;


   signal wire_0: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_1: STD_LOGIC_VECTOR(7 downto 0);
   signal wire_10: STD_ULOGIC;
   signal wire_2: STD_ULOGIC_VECTOR(4 downto 0);
   signal wire_3: STD_ULOGIC_VECTOR(4 downto 0);
   signal wire_4: STD_ULOGIC_VECTOR(4 downto 0);
   signal wire_5: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_6: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_7: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_8: STD_ULOGIC;
   signal wire_9: STD_ULOGIC;

BEGIN
   ram32x8_instance_0 : ram32x8
      PORT MAP(
         DO => wire_1,
         A => to_stdlogicvector(wire_2),
         DI => to_stdlogicvector(wire_5),
         WEN => wire_8);

   wire_0 <= to_stdulogicvector(wire_1);
   DO <= wire_0;
   test_DO <= wire_0;
   wire_4 <= A;
   wire_2 <= wire_3 WHEN (test_h = '1') ELSE wire_4;
   wire_3 <= test_A;
   wire_7 <= DI;
   wire_5 <= wire_6 WHEN (test_h = '1') ELSE wire_7;
   wire_6 <= test_DI;
   wire_10 <= WEN;
   wire_8 <= wire_9 WHEN (test_h = '1') ELSE wire_10;
   wire_9 <= test_WEN;
END behavior;
