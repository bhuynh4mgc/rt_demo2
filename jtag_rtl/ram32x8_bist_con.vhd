--
--  Tue Jul  8 12:04:09 2008
--  MBISTArchitect  v8.2008_1.10  Tue Feb 26 21:08:59 GMT 2008
--  Vhdl model:  ram32x8_bist_con
--  Vhdl model which connects a bist block to memories for ram32x8_bist.vhd
--

--  Connection Block: ram32x8_bist_con
library ieee;
use ieee.std_logic_1164.all;
ENTITY ram32x8_bist_con IS
   PORT(
      tst_done: out STD_ULOGIC;
      fail_h: out STD_ULOGIC;
      test_h: in STD_ULOGIC;
      bist_clk: in STD_ULOGIC;
      rst_l: in STD_ULOGIC;
      DO_0: out STD_ULOGIC_VECTOR(7 downto 0);
      A_0: in STD_ULOGIC_VECTOR(4 downto 0);
      DI_0: in STD_ULOGIC_VECTOR(7 downto 0);
      WEN_0: in STD_ULOGIC);
END ram32x8_bist_con;

ARCHITECTURE behavior OF ram32x8_bist_con IS
-- <<< JTAG_132
   COMPONENT ram32x8_bist
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
   END COMPONENT;

   COMPONENT ram32x8_bist_ram32x8_block
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
   END COMPONENT;


   signal wire_A_0: STD_ULOGIC_VECTOR(4 downto 0);
   signal wire_DI_0: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_DO_0: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_Test_A_0: STD_ULOGIC_VECTOR(4 downto 0);
   signal wire_Test_DI_0: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_Test_DO_0: STD_ULOGIC_VECTOR(7 downto 0);
   signal wire_Test_WEN_0: STD_ULOGIC;
   signal wire_bist_clk: STD_ULOGIC;
   signal wire_fail_h: STD_ULOGIC;
   signal wire_rst_l: STD_ULOGIC;
   signal wire_test_h: STD_ULOGIC;
   signal wire_tst_done: STD_ULOGIC;

BEGIN
   ram32x8_bist_instance : ram32x8_bist
      PORT MAP(
         Test_A_0 => wire_Test_A_0,
         Test_DI_0 => wire_Test_DI_0,
         Test_WEN_0 => wire_Test_WEN_0,
         tst_done => wire_tst_done,
         fail_h => wire_fail_h,
         Test_DO_0 => wire_Test_DO_0,
         test_h => wire_test_h,
         bist_clk => wire_bist_clk,
         rst_l => wire_rst_l);

   ram32x8_bist_ram32x8_block_instance_0 : ram32x8_bist_ram32x8_block
      PORT MAP(
         DO => wire_DO_0,
         test_DO => wire_Test_DO_0,
         test_h => wire_test_h,
         A => wire_A_0,
         test_A => wire_Test_A_0,
         DI => wire_DI_0,
         test_DI => wire_Test_DI_0,
         WEN => WEN_0,
         test_WEN => wire_Test_WEN_0);

   tst_done <= wire_tst_done;
   fail_h <= wire_fail_h;
   wire_test_h <= test_h;
   wire_bist_clk <= bist_clk;
   wire_rst_l <= rst_l;
   DO_0 <= wire_DO_0;
   wire_A_0 <= A_0;
   wire_DI_0 <= DI_0;
END behavior;
