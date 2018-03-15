-- ----------------------------------------- --
-- BSDArchitect :  VHDL write                --
-- Version : v8.2008_1.10                    --
-- Date : Tue Feb 26 12:34:19 PST 2008       --
--                                           --
-- ----------------------------------------- --

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity dr_mux is
  port (
    mux_select : in std_ulogic_vector (1 downto 0);
    data_reg_out : out std_ulogic ;
    boundary : in std_ulogic ;
    bypass : in std_ulogic ;
    mbist_reg : in std_ulogic );
end dr_mux;

architecture rtl of dr_mux is
-- <<< JTAG_112
begin
  sel_proc : 
  process (mux_select, boundary, bypass, mbist_reg)
  begin
    case mux_select is
      when "00" =>
        data_reg_out <= boundary;
      when "01" =>
        data_reg_out <= bypass;
      when "10" =>
        data_reg_out <= mbist_reg;
      when  others  =>
        data_reg_out <= bypass;
    end case;
  end process sel_proc;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity instruction_decoder is
  port (
    instr : in std_ulogic_vector (3 downto 0);
    mux_select : out std_ulogic_vector (1 downto 0);
    mode : out std_ulogic ;
    boundary : out std_ulogic ;
    bypass : out std_ulogic ;
    mbist_reg_reset : out std_ulogic ;
    mbist_instr : out std_ulogic );
end instruction_decoder;

architecture rtl of instruction_decoder is
begin
  decode_process : 
  process (instr)
  begin
    case instr is
      when "0000" =>
        -- <<< JTAG_112, JTAG_121
        mode <= '1';
        boundary <= '1';
        bypass <= '0';
        mbist_reg_reset <= '0';
        mbist_instr <= '0';
        mux_select <= std_ulogic_vector'("00");        
      when "1111" =>
        -- <<< JTAG_117
        mode <= '0';
        boundary <= '0';
        bypass <= '1';
        mbist_reg_reset <= '0';
        mbist_instr <= '0';
        mux_select <= std_ulogic_vector'("01");
      when "0001" =>
        -- <<< JTAG_112, JTAG_119
        mode <= '0';
        boundary <= '1';
        bypass <= '0';
        mbist_reg_reset <= '0';
        mbist_instr <= '0';
        mux_select <= std_ulogic_vector'("00");
      when "0010" =>
        -- <<< JTAG_123
        mode <= '0';
        boundary <= '0';
        bypass <= '1';
        mbist_reg_reset <= '1';
        mbist_instr <= '0';
        mux_select <= std_ulogic_vector'("01");
      when "0011" =>
        -- <<< JTAG_125
        mode <= '0';
        boundary <= '0';
        bypass <= '0';
        mbist_reg_reset <= '0';
        mbist_instr <= '1';
        mux_select <= std_ulogic_vector'("10");
      when  others  =>
        -- <<< JTAG_130        
        mode <= '0';
        boundary <= '0';
        bypass <= '1';
        mbist_reg_reset <= '0';
        mbist_instr <= '0';
        mux_select <= std_ulogic_vector'("01");
    end case;
  end process decode_process;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity tap_ctrl is
  port (
    tms : in std_ulogic ;
    tck : in std_ulogic ;
    trstl : in std_ulogic ;
    resetl : out std_ulogic ;
    selekt : out std_ulogic ;
    enable : out std_ulogic ;
    shiftir : out std_ulogic ;
    clockir : out std_ulogic ;
    updateir : out std_ulogic ;
    pauseir : out std_ulogic ;
    shiftdr : out std_ulogic ;
    clockdr : out std_ulogic ;
    updatedr : out std_ulogic ;
    idle : out std_ulogic ;
    shiftir_d : out std_ulogic ;
    shiftdr_d : out std_ulogic ;
    exit1ir : out std_ulogic ;
    exit1dr : out std_ulogic ;
    capturedr : out std_ulogic ;
    pausedr : out std_ulogic ;
    exit2dr : out std_ulogic );
end tap_ctrl;

architecture rtl of tap_ctrl is
-- <<< JTAG_110
  constant exit2_dr : std_ulogic_vector (3 downto 0):= "0000" ;
  constant exit1_dr : std_ulogic_vector (3 downto 0):= "0001" ;
  constant shift_dr : std_ulogic_vector (3 downto 0):= "0010" ;
  constant pause_dr : std_ulogic_vector (3 downto 0):= "0011" ;
  constant select_ir_scan : std_ulogic_vector (3 downto 0):= "0100" ;
  constant update_dr : std_ulogic_vector (3 downto 0):= "0101" ;
  constant capture_dr : std_ulogic_vector (3 downto 0):= "0110" ;
  constant select_dr_scan : std_ulogic_vector (3 downto 0):= "0111" ;
  constant exit2_ir : std_ulogic_vector (3 downto 0):= "1000" ;
  constant exit1_ir : std_ulogic_vector (3 downto 0):= "1001" ;
  constant shift_ir : std_ulogic_vector (3 downto 0):= "1010" ;
  constant pause_ir : std_ulogic_vector (3 downto 0):= "1011" ;
  constant run_test_idle : std_ulogic_vector (3 downto 0):= "1100" ;
  constant update_ir : std_ulogic_vector (3 downto 0):= "1101" ;
  constant capture_ir : std_ulogic_vector (3 downto 0):= "1110" ;
  constant test_logic_reset : std_ulogic_vector (3 downto 0):= "1111" ;
  signal pstate : std_ulogic_vector (3 downto 0)
--synopsys translate_off
:= "0000" 
--synopsys translate_on
;
  signal reset_d : std_ulogic ;
  signal enable_d : std_ulogic ;
  signal capturedr_d : std_ulogic ;
  signal n_0_shiftir_d : std_ulogic ;
  signal n_1_shiftdr_d : std_ulogic ;
begin
  shiftir_d <= n_0_shiftir_d;
  shiftdr_d <= n_1_shiftdr_d;
  b32_line_1406 : 
  process (pstate)
  begin
    if ( (pstate = test_logic_reset) ) then
      reset_d <= '0';
    else
      reset_d <= '1';
    end if;
  end process b32_line_1406;
  b34_line_1408 : 
  process (pstate)
  begin
    if ( ((pstate = shift_dr) or (pstate = shift_ir)) ) then
      enable_d <= '1';
    else
      enable_d <= '0';
    end if;
  end process b34_line_1408;
  b35_line_1409 : 
  process (pstate)
  begin
    if ( (pstate = shift_ir) ) then
      n_0_shiftir_d <= '1';
    else
      n_0_shiftir_d <= '0';
    end if;
  end process b35_line_1409;
  b36_line_1410 : 
  process (pstate)
  begin
    if ( ((pstate = capture_ir) or (pstate = shift_ir)) ) then
      clockir <= '1';
    else
      clockir <= '0';
    end if;
  end process b36_line_1410;
  b37_line_1411 : 
  process (pstate)
  begin
    if ( (pstate = update_ir) ) then
      updateir <= '1';
    else
      updateir <= '0';
    end if;
  end process b37_line_1411;
  b38_line_1412 : 
  process (pstate)
  begin
    if ( (pstate = shift_dr) ) then
      n_1_shiftdr_d <= '1';
    else
      n_1_shiftdr_d <= '0';
    end if;
  end process b38_line_1412;
  b39_line_1413 : 
  process (pstate)
  begin
    if ( ((pstate = capture_dr) or (pstate = shift_dr)) ) then
      clockdr <= '1';
    else
      clockdr <= '0';
    end if;
  end process b39_line_1413;
  b40_line_1414 : 
  process (pstate)
  begin
    if ( (pstate = update_dr) ) then
      updatedr <= '1';
    else
      updatedr <= '0';
    end if;
  end process b40_line_1414;
  b41_line_1415 : 
  process (pstate)
  begin
    if ( (pstate = run_test_idle) ) then
      idle <= '1';
    else
      idle <= '0';
    end if;
  end process b41_line_1415;
  b42_line_1416 : 
  process (pstate)
  begin
    if ( (pstate = capture_dr) ) then
      capturedr_d <= '1';
    else
      capturedr_d <= '0';
    end if;
  end process b42_line_1416;
  b43_line_1417 : 
  process (pstate)
  begin
    if ( (pstate = pause_dr) ) then
      pausedr <= '1';
    else
      pausedr <= '0';
    end if;
  end process b43_line_1417;
  b44_line_1418 : 
  process (pstate)
  begin
    if ( (pstate = pause_ir) ) then
      pauseir <= '1';
    else
      pauseir <= '0';
    end if;
  end process b44_line_1418;
  b45_line_1419 : 
  process (pstate)
  begin
    if ( (pstate = exit1_ir) ) then
      exit1ir <= '1';
    else
      exit1ir <= '0';
    end if;
  end process b45_line_1419;
  b46_line_1420 : 
  process (pstate)
  begin
    if ( (pstate = exit1_dr) ) then
      exit1dr <= '1';
    else
      exit1dr <= '0';
    end if;
  end process b46_line_1420;
  b47_line_1421 : 
  process (pstate)
  begin
    if ( (pstate = exit2_dr) ) then
      exit2dr <= '1';
    else
      exit2dr <= '0';
    end if;
  end process b47_line_1421;
  final_ff : 
  process (trstl, tck)
  begin
    if ( ((trstl = '0') and true) ) then
      resetl <= '0';
      enable <= '0';
      shiftir <= '0';
      shiftdr <= '0';
      capturedr <= '0';
    elsif tck'event and tck = '0' then
      resetl <= reset_d;
      enable <= enable_d;
      shiftir <= n_0_shiftir_d;
      shiftdr <= n_1_shiftdr_d;
      capturedr <= capturedr_d;
    end if;
  end process final_ff;
  fsm : 
  process (trstl, tck)
  begin
    if ( ((trstl = '0') and true) ) then
-- **********************************
-- This is where JTAG_103 needs to be tagged

-- **********************************
      pstate <= test_logic_reset;
    elsif tck'event and tck = '1' then
      -- <<< JTAG_110
      case pstate is
        when "1111" =>
          if ( (tms = '0') ) then
            pstate <= run_test_idle;
          end if;
        when "1100" =>
          if ( (tms = '1') ) then
            pstate <= select_dr_scan;
          end if;
        when "0111" =>
          if ( (tms = '0') ) then
            pstate <= capture_dr;
          else
            pstate <= select_ir_scan;
          end if;
        when "0110" =>
          if ( (tms = '0') ) then
            pstate <= shift_dr;
          else
            pstate <= exit1_dr;
          end if;
        when "0010" =>
          if ( (tms = '1') ) then
            pstate <= exit1_dr;
          end if;
        when "0001" =>
          if ( (tms = '0') ) then
            pstate <= pause_dr;
          else
            pstate <= update_dr;
          end if;
        when "0011" =>
          if ( (tms = '1') ) then
            pstate <= exit2_dr;
          end if;
        when "0000" =>
          if ( (tms = '0') ) then
            pstate <= shift_dr;
          else
            pstate <= update_dr;
          end if;
        when "0101" =>
          if ( (tms = '0') ) then
            pstate <= run_test_idle;
          else
            pstate <= select_dr_scan;
          end if;
        when "0100" =>
          if ( (tms = '0') ) then
            pstate <= capture_ir;
          else
            pstate <= test_logic_reset;
          end if;
        when "1110" =>
          if ( (tms = '0') ) then
            pstate <= shift_ir;
          else
            pstate <= exit1_ir;
          end if;
        when "1010" =>
          if ( (tms = '1') ) then
            pstate <= exit1_ir;
          end if;
        when "1001" =>
          if ( (tms = '0') ) then
            pstate <= pause_ir;
          else
            pstate <= update_ir;
          end if;
        when "1011" =>
          if ( (tms = '1') ) then
            pstate <= exit2_ir;
          end if;
        when "1000" =>
          if ( (tms = '0') ) then
            pstate <= shift_ir;
          else
            pstate <= update_ir;
          end if;
        when "1101" =>
          if ( (tms = '0') ) then
            pstate <= run_test_idle;
          else
            pstate <= select_dr_scan;
          end if;
        when  others  =>
      end case;
    end if;
  end process fsm;
  selekt <= pstate(3);
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity bypass_reg is
  port (
    tdi : in std_ulogic ;
    tck : in std_ulogic ;
    shiftdr : in std_ulogic ;
    clockdr : in std_ulogic ;
    bypass_enable : in std_ulogic ;
    bypass_reg_out : out std_ulogic );
end bypass_reg;

architecture rtl of bypass_reg is
-- <<< JTAG_117
  signal i_bypass_reg_out : std_ulogic ;
  signal bypass_reg_in : std_ulogic ;
  signal hold_mux_sel : std_ulogic ;
  signal hold_mux_out : std_ulogic ;
begin
  b16_line_1557 : 
  process (hold_mux_sel, bypass_reg_in, i_bypass_reg_out)
  begin
    if ( (hold_mux_sel = '1') ) then
      hold_mux_out <= bypass_reg_in;
    else
      hold_mux_out <= i_bypass_reg_out;
    end if;
  end process b16_line_1557;
  tmp2 : 
  process 
  begin
    wait until tck'event and tck = '1';
    i_bypass_reg_out <= hold_mux_out;
  end process tmp2;
  bypass_reg_in <= (tdi and shiftdr);
  hold_mux_sel <= (clockdr and bypass_enable);
  bypass_reg_out <= i_bypass_reg_out;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity instruction_reg is
  port (
    tck : in std_ulogic ;
    tdi : in std_ulogic ;
    resetl : in std_ulogic ;
    shiftir : in std_ulogic ;
    clockir : in std_ulogic ;
    updateir : in std_ulogic ;
    trstl : in std_ulogic ;
    instr_reg_out : out std_ulogic ;
    pinstr : out std_ulogic_vector (3 downto 0));
end instruction_reg;

architecture rtl of instruction_reg is
  constant capture_pattern : std_ulogic_vector (3 downto 0):= "0001" ;
  constant bypass : std_ulogic_vector (3 downto 0):= "1111" ;
  signal i_pinstr : std_ulogic_vector (3 downto 0);
  signal instr_reg : std_ulogic_vector (3 downto 0);
  signal shift_hold_mux_in : std_ulogic_vector (3 downto 0);
  signal shift_hold_mux_out : std_ulogic_vector (3 downto 0);
  signal update_hold_mux_out : std_ulogic_vector (3 downto 0);
begin
  tmp3 : 
  process (shiftir, tdi, instr_reg)
  begin
    if ( (shiftir = '0') ) then
      shift_hold_mux_in <= capture_pattern;
    else
      shift_hold_mux_in <= (tdi & instr_reg(3 downto 1));
    end if;
  end process tmp3;
  b26_line_1595 : 
  process (clockir, shift_hold_mux_in, instr_reg)
  begin
    if ( (clockir = '1') ) then
      shift_hold_mux_out <= shift_hold_mux_in;
    else
      shift_hold_mux_out <= instr_reg;
    end if;
  end process b26_line_1595;
  tmp4 : 
  process 
  begin
    wait until tck'event and tck = '1';
    instr_reg <= shift_hold_mux_out;
  end process tmp4;
  b27_line_1604 : 
  process (updateir, instr_reg, i_pinstr)
  begin
    if ( (updateir = '1') ) then
      update_hold_mux_out <= instr_reg;
    else
      update_hold_mux_out <= i_pinstr;
    end if;
  end process b27_line_1604;
  tmp5 : 
  process (trstl, tck)
  begin
    if ( (trstl = '0') ) then
      i_pinstr <= bypass;
    elsif tck'event and tck = '0' then
      if ( (resetl = '0') ) then
        -- <<< JTAG_105
        i_pinstr <= bypass;
      else
        i_pinstr <= update_hold_mux_out;
      end if;
    end if;
  end process tmp5;
  instr_reg_out <= instr_reg(0);
  pinstr <= i_pinstr;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity tap_regs is
  port (
    tdi : in std_ulogic ;
    tck : in std_ulogic ;
    resetl : in std_ulogic ;
    selekt : in std_ulogic ;
    shiftir : in std_ulogic ;
    clockir : in std_ulogic ;
    updateir : in std_ulogic ;
    shiftdr : in std_ulogic ;
    clockdr : in std_ulogic ;
    data_reg_out : in std_ulogic ;
    trstl : in std_ulogic ;
    bypass_enable : in std_ulogic ;
    pinstr : out std_ulogic_vector (3 downto 0);
    bypass_reg_out : out std_ulogic ;
    out_bit_to_tdo : out std_ulogic );
end tap_regs;

architecture rtl of tap_regs is
  component bypass_reg
    port (
      tdi : in std_ulogic ;
      tck : in std_ulogic ;
      shiftdr : in std_ulogic ;
      clockdr : in std_ulogic ;
      bypass_enable : in std_ulogic ;
      bypass_reg_out : out std_ulogic );
  end component;

  component instruction_reg
    port (
      tck : in std_ulogic ;
      tdi : in std_ulogic ;
      resetl : in std_ulogic ;
      shiftir : in std_ulogic ;
      clockir : in std_ulogic ;
      updateir : in std_ulogic ;
      trstl : in std_ulogic ;
      instr_reg_out : out std_ulogic ;
      pinstr : out std_ulogic_vector (3 downto 0));
  end component;

  signal ins_or_data : std_ulogic ;
  signal instr_reg_out : std_ulogic ;
  signal i_out_bit : std_ulogic ;
begin
  tmp6 : 
  process 
  begin
    wait until tck'event and tck = '0';
    i_out_bit <= ins_or_data;
  end process tmp6;
  b25_line_1727 : 
  process (selekt, instr_reg_out, data_reg_out)
  begin
    if ( (selekt = '1') ) then
      -- <<< JTAG_107
      ins_or_data <= instr_reg_out;
    else
      ins_or_data <= data_reg_out;
    end if;
  end process b25_line_1727;
  out_bit_to_tdo <= i_out_bit;
bypassreg:
  bypass_reg
    port map(tdi => tdi,
             tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr,
             bypass_enable => bypass_enable,
             bypass_reg_out => bypass_reg_out);
instrreg:
  instruction_reg
    port map(tck => tck,
             tdi => tdi,
             resetl => resetl,
             shiftir => shiftir,
             clockir => clockir,
             updateir => updateir,
             trstl => trstl,
             instr_reg_out => instr_reg_out,
             pinstr(3) => pinstr(3), 
             pinstr(2) => pinstr(2), 
             pinstr(1) => pinstr(1), 
             pinstr(0) => pinstr(0));
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity tap is
  port (
    tdi : in std_ulogic ;
    tdo : out std_ulogic ;
    tms : in std_ulogic ;
    tck : in std_ulogic ;
    boundary_in : in std_ulogic ;
    mbist_reg_in : in std_ulogic ;
    mode : out std_ulogic ;
    mbist_reg_reset : out std_ulogic ;
    mbist_instr : out std_ulogic ;
    trstl : in std_ulogic ;
    enable : out std_ulogic ;
    shiftdr : out std_ulogic ;
    clockdr : out std_ulogic ;
    updatedr : out std_ulogic ;
    resetl : out std_ulogic ;
    updateir : out std_ulogic ;
    clockdr_bsr : out std_ulogic ;
    updatedr_bsr : out std_ulogic ;
    capturedr_bsr : out std_ulogic ;
    tap_mbist_clk_o : out std_ulogic ;
    tap_mbist_rst_n_o : out std_ulogic );
end tap;

architecture rtl of tap is
-- <<< JTAG_117, JTAG_130
  component dr_mux
    port (
      mux_select : in std_ulogic_vector (1 downto 0);
      data_reg_out : out std_ulogic ;
      boundary : in std_ulogic ;
      bypass : in std_ulogic ;
      mbist_reg : in std_ulogic );
  end component;

  component instruction_decoder
    port (
      instr : in std_ulogic_vector (3 downto 0);
      mux_select : out std_ulogic_vector (1 downto 0);
      mode : out std_ulogic ;
      boundary : out std_ulogic ;
      bypass : out std_ulogic ;
      mbist_reg_reset : out std_ulogic ;
      mbist_instr : out std_ulogic );
  end component;

  component tap_ctrl
    port (
      tms : in std_ulogic ;
      tck : in std_ulogic ;
      trstl : in std_ulogic ;
      resetl : out std_ulogic ;
      selekt : out std_ulogic ;
      enable : out std_ulogic ;
      shiftir : out std_ulogic ;
      clockir : out std_ulogic ;
      updateir : out std_ulogic ;
      pauseir : out std_ulogic ;
      shiftdr : out std_ulogic ;
      clockdr : out std_ulogic ;
      updatedr : out std_ulogic ;
      idle : out std_ulogic ;
      shiftir_d : out std_ulogic ;
      shiftdr_d : out std_ulogic ;
      exit1ir : out std_ulogic ;
      exit1dr : out std_ulogic ;
      capturedr : out std_ulogic ;
      pausedr : out std_ulogic ;
      exit2dr : out std_ulogic );
  end component;

  component tap_regs
    port (
      tdi : in std_ulogic ;
      tck : in std_ulogic ;
      resetl : in std_ulogic ;
      selekt : in std_ulogic ;
      shiftir : in std_ulogic ;
      clockir : in std_ulogic ;
      updateir : in std_ulogic ;
      shiftdr : in std_ulogic ;
      clockdr : in std_ulogic ;
      data_reg_out : in std_ulogic ;
      trstl : in std_ulogic ;
      bypass_enable : in std_ulogic ;
      pinstr : out std_ulogic_vector (3 downto 0);
      bypass_reg_out : out std_ulogic ;
      out_bit_to_tdo : out std_ulogic );
  end component;

  signal boundary : std_ulogic ;
  signal capturedr : std_ulogic ;
  signal clockir : std_ulogic ;
  signal idle : std_ulogic ;
  signal pausedr : std_ulogic ;
  signal pauseir : std_ulogic ;
  signal selekt : std_ulogic ;
  signal shiftir : std_ulogic ;
  signal nx_dr_mux_i_data_reg_out : std_ulogic ;
  signal nx_instr_dec_i_bypass : std_ulogic ;
  signal nx_instr_dec_i_mbist_reg_reset : std_ulogic ;
  signal nx_instr_dec_i_mux_select : std_ulogic_vector (1 downto 0);
  signal nx_tap_ctrl_i_clockdr : std_ulogic ;
  signal nx_tap_ctrl_i_exit1dr : std_ulogic ;
  signal nx_tap_ctrl_i_exit1ir : std_ulogic ;
  signal nx_tap_ctrl_i_exit2dr : std_ulogic ;
  signal nx_tap_ctrl_i_resetl : std_ulogic ;
  signal nx_tap_ctrl_i_shiftdr : std_ulogic ;
  signal nx_tap_ctrl_i_shiftdr_d : std_ulogic ;
  signal nx_tap_ctrl_i_shiftir_d : std_ulogic ;
  signal nx_tap_ctrl_i_updatedr : std_ulogic ;
  signal nx_tap_ctrl_i_updateir : std_ulogic ;
  signal nx_tap_regs_i_bypass_reg_out : std_ulogic ;
  signal nx_tap_regs_i_pinstr : std_ulogic_vector (3 downto 0);
begin
  clockdr <= nx_tap_ctrl_i_clockdr;
  mbist_reg_reset <= nx_instr_dec_i_mbist_reg_reset;
  resetl <= nx_tap_ctrl_i_resetl;
  shiftdr <= nx_tap_ctrl_i_shiftdr;
  updatedr <= nx_tap_ctrl_i_updatedr;
  updateir <= nx_tap_ctrl_i_updateir;
  clockdr_bsr <= (boundary and nx_tap_ctrl_i_clockdr);
  updatedr_bsr <= (boundary and nx_tap_ctrl_i_updatedr);
  capturedr_bsr <= (boundary and capturedr);
  tap_mbist_clk_o <= tck;
  tap_mbist_rst_n_o <= (nx_instr_dec_i_mbist_reg_reset nand 
    nx_tap_ctrl_i_updateir);
dr_mux_i:
  dr_mux
    port map(mux_select(1) => nx_instr_dec_i_mux_select(1), 
             mux_select(0) => nx_instr_dec_i_mux_select(0),
             data_reg_out => nx_dr_mux_i_data_reg_out,
             boundary => boundary_in,
             bypass => nx_tap_regs_i_bypass_reg_out,
             mbist_reg => mbist_reg_in);
instr_dec_i:
  instruction_decoder
    port map(instr(3) => nx_tap_regs_i_pinstr(3), 
             instr(2) => nx_tap_regs_i_pinstr(2), 
             instr(1) => nx_tap_regs_i_pinstr(1), 
             instr(0) => nx_tap_regs_i_pinstr(0),
             mux_select(1) => nx_instr_dec_i_mux_select(1), 
             mux_select(0) => nx_instr_dec_i_mux_select(0),
             mode => mode,
             boundary => boundary,
             bypass => nx_instr_dec_i_bypass,
             mbist_reg_reset => nx_instr_dec_i_mbist_reg_reset,
             mbist_instr => mbist_instr);
tap_ctrl_i:
  tap_ctrl
    port map(tms => tms,
             tck => tck,
             trstl => trstl,
             resetl => nx_tap_ctrl_i_resetl,
             selekt => selekt,
             enable => enable,
             shiftir => shiftir,
             clockir => clockir,
             updateir => nx_tap_ctrl_i_updateir,
             pauseir => pauseir,
             shiftdr => nx_tap_ctrl_i_shiftdr,
             clockdr => nx_tap_ctrl_i_clockdr,
             updatedr => nx_tap_ctrl_i_updatedr,
             idle => idle,
             shiftir_d => nx_tap_ctrl_i_shiftir_d,
             shiftdr_d => nx_tap_ctrl_i_shiftdr_d,
             exit1ir => nx_tap_ctrl_i_exit1ir,
             exit1dr => nx_tap_ctrl_i_exit1dr,
             capturedr => capturedr,
             pausedr => pausedr,
             exit2dr => nx_tap_ctrl_i_exit2dr);
tap_regs_i:
  tap_regs
    port map(tdi => tdi,
             tck => tck,
             resetl => nx_tap_ctrl_i_resetl,
             selekt => selekt,
             shiftir => shiftir,
             clockir => clockir,
             updateir => nx_tap_ctrl_i_updateir,
             shiftdr => nx_tap_ctrl_i_shiftdr,
             clockdr => nx_tap_ctrl_i_clockdr,
             data_reg_out => nx_dr_mux_i_data_reg_out,
             trstl => trstl,
             bypass_enable => nx_instr_dec_i_bypass,
             pinstr(3) => nx_tap_regs_i_pinstr(3), 
             pinstr(2) => nx_tap_regs_i_pinstr(2), 
             pinstr(1) => nx_tap_regs_i_pinstr(1), 
             pinstr(0) => nx_tap_regs_i_pinstr(0),
             bypass_reg_out => nx_tap_regs_i_bypass_reg_out,
             out_bit_to_tdo => tdo);
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

-- Copyright Mentor Graphics Corporation 2002  All Rights Reserved.
entity bc_1 is
  port (
    tck : in std_ulogic ;
    shiftdr : in std_ulogic ;
    clockdr : in std_ulogic ;
    updatedr : in std_ulogic ;
    parallel_input : in std_ulogic ;
    serial_input : in std_ulogic ;
    mode : in std_ulogic ;
    serial_output : out std_ulogic ;
    parallel_output : out std_ulogic );
end bc_1;

architecture rtl of bc_1 is
	-- <<< JTAG_119, JTAG_121
  signal latch_out : std_ulogic ;
  signal shift_reg_in : std_ulogic ;
  signal int_serial_output : std_ulogic ;
  signal shift_mux_out : std_ulogic ;
  signal update_mux_out : std_ulogic ;
begin
  b274_line_32 : 
  process (shiftdr, parallel_input, serial_input)
  begin
    if ( (shiftdr = '0') ) then
      shift_reg_in <= parallel_input;
    else
      shift_reg_in <= serial_input;
    end if;
  end process b274_line_32;
  b275_line_35 : 
  process (clockdr, int_serial_output, shift_reg_in)
  begin
    if ( (clockdr = '0') ) then
      shift_mux_out <= int_serial_output;
    else
      shift_mux_out <= shift_reg_in;
    end if;
  end process b275_line_35;
  shift_register : 
  process 
  begin
    wait until tck'event and tck = '1';
    int_serial_output <= shift_mux_out;
  end process shift_register;
  b276_line_45 : 
  process (updatedr, latch_out, int_serial_output)
  begin
    if ( (updatedr = '0') ) then
      update_mux_out <= latch_out;
    else
      update_mux_out <= int_serial_output;
    end if;
  end process b276_line_45;
  update_register : 
  process 
  begin
    wait until tck'event and tck = '0';
    latch_out <= update_mux_out;
  end process update_register;
  b277_line_55 : 
  process (mode, parallel_input, latch_out)
  begin
    if ( (mode = '0') ) then
      parallel_output <= parallel_input;
    else
      parallel_output <= latch_out;
    end if;
  end process b277_line_55;
  serial_output <= int_serial_output;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity bsr_instance_1 is
  port (
    bsr_in : in std_ulogic ;
    bsr_out : out std_ulogic ;
    mode : in std_ulogic ;
    shiftdr : in std_ulogic ;
    updatedr_i : in std_ulogic ;
    updatedr_o : in std_ulogic ;
    clockdr_i : in std_ulogic ;
    clockdr_o : in std_ulogic ;
    tck : in std_ulogic ;
    pi_bsc_v_DO_0_7 : in std_ulogic ;
    po_bsc_v_DO_0_7 : out std_ulogic ;
    pi_bsc_v_DO_0_6 : in std_ulogic ;
    po_bsc_v_DO_0_6 : out std_ulogic ;
    pi_bsc_v_DO_0_5 : in std_ulogic ;
    po_bsc_v_DO_0_5 : out std_ulogic ;
    pi_bsc_v_DO_0_4 : in std_ulogic ;
    po_bsc_v_DO_0_4 : out std_ulogic ;
    pi_bsc_v_DO_0_3 : in std_ulogic ;
    po_bsc_v_DO_0_3 : out std_ulogic ;
    pi_bsc_v_DO_0_2 : in std_ulogic ;
    po_bsc_v_DO_0_2 : out std_ulogic ;
    pi_bsc_v_DO_0_1 : in std_ulogic ;
    po_bsc_v_DO_0_1 : out std_ulogic ;
    pi_bsc_v_DO_0_0 : in std_ulogic ;
    po_bsc_v_DO_0_0 : out std_ulogic ;
    pi_bsc_v_dout_7 : in std_ulogic ;
    po_bsc_v_dout_7 : out std_ulogic ;
    pi_bsc_v_dout_6 : in std_ulogic ;
    po_bsc_v_dout_6 : out std_ulogic ;
    pi_bsc_v_dout_5 : in std_ulogic ;
    po_bsc_v_dout_5 : out std_ulogic ;
    pi_bsc_v_dout_4 : in std_ulogic ;
    po_bsc_v_dout_4 : out std_ulogic ;
    pi_bsc_v_dout_3 : in std_ulogic ;
    po_bsc_v_dout_3 : out std_ulogic ;
    pi_bsc_v_dout_2 : in std_ulogic ;
    po_bsc_v_dout_2 : out std_ulogic ;
    pi_bsc_v_dout_1 : in std_ulogic ;
    po_bsc_v_dout_1 : out std_ulogic ;
    pi_bsc_v_dout_0 : in std_ulogic ;
    po_bsc_v_dout_0 : out std_ulogic ;
    pi_bsc_tc : in std_ulogic ;
    po_bsc_tc : out std_ulogic ;
    pi_bsc_zero : in std_ulogic ;
    po_bsc_zero : out std_ulogic ;
    pi_bsc_v_A_0_4 : in std_ulogic ;
    po_bsc_v_A_0_4 : out std_ulogic ;
    pi_bsc_v_A_0_3 : in std_ulogic ;
    po_bsc_v_A_0_3 : out std_ulogic ;
    pi_bsc_v_A_0_2 : in std_ulogic ;
    po_bsc_v_A_0_2 : out std_ulogic ;
    pi_bsc_v_A_0_1 : in std_ulogic ;
    po_bsc_v_A_0_1 : out std_ulogic ;
    pi_bsc_v_A_0_0 : in std_ulogic ;
    po_bsc_v_A_0_0 : out std_ulogic ;
    pi_bsc_cen : in std_ulogic ;
    po_bsc_cen : out std_ulogic ;
    pi_bsc_clk : in std_ulogic ;
    po_bsc_clk : out std_ulogic ;
    pi_bsc_v_DI_0_7 : in std_ulogic ;
    po_bsc_v_DI_0_7 : out std_ulogic ;
    pi_bsc_v_DI_0_6 : in std_ulogic ;
    po_bsc_v_DI_0_6 : out std_ulogic ;
    pi_bsc_v_DI_0_5 : in std_ulogic ;
    po_bsc_v_DI_0_5 : out std_ulogic ;
    pi_bsc_v_DI_0_4 : in std_ulogic ;
    po_bsc_v_DI_0_4 : out std_ulogic ;
    pi_bsc_v_DI_0_3 : in std_ulogic ;
    po_bsc_v_DI_0_3 : out std_ulogic ;
    pi_bsc_v_DI_0_2 : in std_ulogic ;
    po_bsc_v_DI_0_2 : out std_ulogic ;
    pi_bsc_v_DI_0_1 : in std_ulogic ;
    po_bsc_v_DI_0_1 : out std_ulogic ;
    pi_bsc_v_DI_0_0 : in std_ulogic ;
    po_bsc_v_DI_0_0 : out std_ulogic ;
    pi_bsc_v_din_7 : in std_ulogic ;
    po_bsc_v_din_7 : out std_ulogic ;
    pi_bsc_v_din_6 : in std_ulogic ;
    po_bsc_v_din_6 : out std_ulogic ;
    pi_bsc_v_din_5 : in std_ulogic ;
    po_bsc_v_din_5 : out std_ulogic ;
    pi_bsc_v_din_4 : in std_ulogic ;
    po_bsc_v_din_4 : out std_ulogic ;
    pi_bsc_v_din_3 : in std_ulogic ;
    po_bsc_v_din_3 : out std_ulogic ;
    pi_bsc_v_din_2 : in std_ulogic ;
    po_bsc_v_din_2 : out std_ulogic ;
    pi_bsc_v_din_1 : in std_ulogic ;
    po_bsc_v_din_1 : out std_ulogic ;
    pi_bsc_v_din_0 : in std_ulogic ;
    po_bsc_v_din_0 : out std_ulogic ;
    pi_bsc_ld_n : in std_ulogic ;
    po_bsc_ld_n : out std_ulogic ;
    pi_bsc_rst_n : in std_ulogic ;
    po_bsc_rst_n : out std_ulogic ;
    pi_bsc_up_dn : in std_ulogic ;
    po_bsc_up_dn : out std_ulogic ;
    pi_bsc_WEN_0 : in std_ulogic ;
    po_bsc_WEN_0 : out std_ulogic );
end bsr_instance_1;

architecture rtl of bsr_instance_1 is
-- <<< JTAG_119
  component bc_1
    port (
      tck : in std_ulogic ;
      shiftdr : in std_ulogic ;
      clockdr : in std_ulogic ;
      updatedr : in std_ulogic ;
      parallel_input : in std_ulogic ;
      serial_input : in std_ulogic ;
      mode : in std_ulogic ;
      serial_output : out std_ulogic ;
      parallel_output : out std_ulogic );
  end component;

  signal si_bsc_v_A_0_0 : std_ulogic ;
  signal si_bsc_cen : std_ulogic ;
  signal si_bsc_v_din_0 : std_ulogic ;
  signal si_bsc_ld_n : std_ulogic ;
  signal si_bsc_v_dout_0 : std_ulogic ;
  signal si_bsc_rst_n : std_ulogic ;
  signal si_bsc_v_A_0_1 : std_ulogic ;
  signal si_bsc_v_A_0_2 : std_ulogic ;
  signal si_bsc_v_A_0_3 : std_ulogic ;
  signal si_bsc_v_A_0_4 : std_ulogic ;
  signal si_bsc_zero : std_ulogic ;
  signal si_bsc_v_DI_0_1 : std_ulogic ;
  signal si_bsc_v_DI_0_2 : std_ulogic ;
  signal si_bsc_v_DI_0_3 : std_ulogic ;
  signal si_bsc_v_DI_0_4 : std_ulogic ;
  signal si_bsc_v_DI_0_5 : std_ulogic ;
  signal si_bsc_v_DI_0_6 : std_ulogic ;
  signal si_bsc_v_DI_0_7 : std_ulogic ;
  signal si_bsc_clk : std_ulogic ;
  signal si_bsc_v_din_1 : std_ulogic ;
  signal si_bsc_v_din_2 : std_ulogic ;
  signal si_bsc_v_din_3 : std_ulogic ;
  signal si_bsc_v_din_4 : std_ulogic ;
  signal si_bsc_v_din_5 : std_ulogic ;
  signal si_bsc_v_din_6 : std_ulogic ;
  signal si_bsc_v_din_7 : std_ulogic ;
  signal si_bsc_v_DI_0_0 : std_ulogic ;
  signal si_bsc_v_DO_0_1 : std_ulogic ;
  signal si_bsc_v_DO_0_2 : std_ulogic ;
  signal si_bsc_v_DO_0_3 : std_ulogic ;
  signal si_bsc_v_DO_0_4 : std_ulogic ;
  signal si_bsc_v_DO_0_5 : std_ulogic ;
  signal si_bsc_v_DO_0_6 : std_ulogic ;
  signal si_bsc_v_DO_0_7 : std_ulogic ;
  signal si_bsc_v_dout_1 : std_ulogic ;
  signal si_bsc_v_dout_2 : std_ulogic ;
  signal si_bsc_v_dout_3 : std_ulogic ;
  signal si_bsc_v_dout_4 : std_ulogic ;
  signal si_bsc_v_dout_5 : std_ulogic ;
  signal si_bsc_v_dout_6 : std_ulogic ;
  signal si_bsc_v_dout_7 : std_ulogic ;
  signal si_bsc_v_DO_0_0 : std_ulogic ;
  signal si_bsc_up_dn : std_ulogic ;
  signal si_bsc_tc : std_ulogic ;
begin
bsc_v_DO_0_7:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_7,
             serial_input => si_bsc_v_DO_0_7,
             mode => mode,
             serial_output => bsr_out,
             parallel_output => po_bsc_v_DO_0_7);
bsc_v_DO_0_6:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_6,
             serial_input => si_bsc_v_DO_0_6,
             mode => mode,
             serial_output => si_bsc_v_DO_0_7,
             parallel_output => po_bsc_v_DO_0_6);
bsc_v_DO_0_5:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_5,
             serial_input => si_bsc_v_DO_0_5,
             mode => mode,
             serial_output => si_bsc_v_DO_0_6,
             parallel_output => po_bsc_v_DO_0_5);
bsc_v_DO_0_4:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_4,
             serial_input => si_bsc_v_DO_0_4,
             mode => mode,
             serial_output => si_bsc_v_DO_0_5,
             parallel_output => po_bsc_v_DO_0_4);
bsc_v_DO_0_3:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_3,
             serial_input => si_bsc_v_DO_0_3,
             mode => mode,
             serial_output => si_bsc_v_DO_0_4,
             parallel_output => po_bsc_v_DO_0_3);
bsc_v_DO_0_2:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_2,
             serial_input => si_bsc_v_DO_0_2,
             mode => mode,
             serial_output => si_bsc_v_DO_0_3,
             parallel_output => po_bsc_v_DO_0_2);
bsc_v_DO_0_1:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_1,
             serial_input => si_bsc_v_DO_0_1,
             mode => mode,
             serial_output => si_bsc_v_DO_0_2,
             parallel_output => po_bsc_v_DO_0_1);
bsc_v_DO_0_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_DO_0_0,
             serial_input => si_bsc_v_DO_0_0,
             mode => mode,
             serial_output => si_bsc_v_DO_0_1,
             parallel_output => po_bsc_v_DO_0_0);
bsc_v_dout_7:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_7,
             serial_input => si_bsc_v_dout_7,
             mode => mode,
             serial_output => si_bsc_v_DO_0_0,
             parallel_output => po_bsc_v_dout_7);
bsc_v_dout_6:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_6,
             serial_input => si_bsc_v_dout_6,
             mode => mode,
             serial_output => si_bsc_v_dout_7,
             parallel_output => po_bsc_v_dout_6);
bsc_v_dout_5:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_5,
             serial_input => si_bsc_v_dout_5,
             mode => mode,
             serial_output => si_bsc_v_dout_6,
             parallel_output => po_bsc_v_dout_5);
bsc_v_dout_4:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_4,
             serial_input => si_bsc_v_dout_4,
             mode => mode,
             serial_output => si_bsc_v_dout_5,
             parallel_output => po_bsc_v_dout_4);
bsc_v_dout_3:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_3,
             serial_input => si_bsc_v_dout_3,
             mode => mode,
             serial_output => si_bsc_v_dout_4,
             parallel_output => po_bsc_v_dout_3);
bsc_v_dout_2:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_2,
             serial_input => si_bsc_v_dout_2,
             mode => mode,
             serial_output => si_bsc_v_dout_3,
             parallel_output => po_bsc_v_dout_2);
bsc_v_dout_1:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_1,
             serial_input => si_bsc_v_dout_1,
             mode => mode,
             serial_output => si_bsc_v_dout_2,
             parallel_output => po_bsc_v_dout_1);
bsc_v_dout_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_v_dout_0,
             serial_input => si_bsc_v_dout_0,
             mode => mode,
             serial_output => si_bsc_v_dout_1,
             parallel_output => po_bsc_v_dout_0);
bsc_tc:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_tc,
             serial_input => si_bsc_tc,
             mode => mode,
             serial_output => si_bsc_v_dout_0,
             parallel_output => po_bsc_tc);
bsc_zero:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_o,
             updatedr => updatedr_o,
             parallel_input => pi_bsc_zero,
             serial_input => si_bsc_zero,
             mode => mode,
             serial_output => si_bsc_tc,
             parallel_output => po_bsc_zero);
bsc_v_A_0_4:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_A_0_4,
             serial_input => si_bsc_v_A_0_4,
             mode => mode,
             serial_output => si_bsc_zero,
             parallel_output => po_bsc_v_A_0_4);
bsc_v_A_0_3:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_A_0_3,
             serial_input => si_bsc_v_A_0_3,
             mode => mode,
             serial_output => si_bsc_v_A_0_4,
             parallel_output => po_bsc_v_A_0_3);
bsc_v_A_0_2:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_A_0_2,
             serial_input => si_bsc_v_A_0_2,
             mode => mode,
             serial_output => si_bsc_v_A_0_3,
             parallel_output => po_bsc_v_A_0_2);
bsc_v_A_0_1:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_A_0_1,
             serial_input => si_bsc_v_A_0_1,
             mode => mode,
             serial_output => si_bsc_v_A_0_2,
             parallel_output => po_bsc_v_A_0_1);
bsc_v_A_0_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_A_0_0,
             serial_input => si_bsc_v_A_0_0,
             mode => mode,
             serial_output => si_bsc_v_A_0_1,
             parallel_output => po_bsc_v_A_0_0);
bsc_cen:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_cen,
             serial_input => si_bsc_cen,
             mode => mode,
             serial_output => si_bsc_v_A_0_0,
             parallel_output => po_bsc_cen);
bsc_clk:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_clk,
             serial_input => si_bsc_clk,
             mode => mode,
             serial_output => si_bsc_cen,
             parallel_output => po_bsc_clk);
bsc_v_DI_0_7:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_7,
             serial_input => si_bsc_v_DI_0_7,
             mode => mode,
             serial_output => si_bsc_clk,
             parallel_output => po_bsc_v_DI_0_7);
bsc_v_DI_0_6:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_6,
             serial_input => si_bsc_v_DI_0_6,
             mode => mode,
             serial_output => si_bsc_v_DI_0_7,
             parallel_output => po_bsc_v_DI_0_6);
bsc_v_DI_0_5:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_5,
             serial_input => si_bsc_v_DI_0_5,
             mode => mode,
             serial_output => si_bsc_v_DI_0_6,
             parallel_output => po_bsc_v_DI_0_5);
bsc_v_DI_0_4:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_4,
             serial_input => si_bsc_v_DI_0_4,
             mode => mode,
             serial_output => si_bsc_v_DI_0_5,
             parallel_output => po_bsc_v_DI_0_4);
bsc_v_DI_0_3:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_3,
             serial_input => si_bsc_v_DI_0_3,
             mode => mode,
             serial_output => si_bsc_v_DI_0_4,
             parallel_output => po_bsc_v_DI_0_3);
bsc_v_DI_0_2:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_2,
             serial_input => si_bsc_v_DI_0_2,
             mode => mode,
             serial_output => si_bsc_v_DI_0_3,
             parallel_output => po_bsc_v_DI_0_2);
bsc_v_DI_0_1:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_1,
             serial_input => si_bsc_v_DI_0_1,
             mode => mode,
             serial_output => si_bsc_v_DI_0_2,
             parallel_output => po_bsc_v_DI_0_1);
bsc_v_DI_0_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_DI_0_0,
             serial_input => si_bsc_v_DI_0_0,
             mode => mode,
             serial_output => si_bsc_v_DI_0_1,
             parallel_output => po_bsc_v_DI_0_0);
bsc_v_din_7:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_7,
             serial_input => si_bsc_v_din_7,
             mode => mode,
             serial_output => si_bsc_v_DI_0_0,
             parallel_output => po_bsc_v_din_7);
bsc_v_din_6:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_6,
             serial_input => si_bsc_v_din_6,
             mode => mode,
             serial_output => si_bsc_v_din_7,
             parallel_output => po_bsc_v_din_6);
bsc_v_din_5:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_5,
             serial_input => si_bsc_v_din_5,
             mode => mode,
             serial_output => si_bsc_v_din_6,
             parallel_output => po_bsc_v_din_5);
bsc_v_din_4:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_4,
             serial_input => si_bsc_v_din_4,
             mode => mode,
             serial_output => si_bsc_v_din_5,
             parallel_output => po_bsc_v_din_4);
bsc_v_din_3:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_3,
             serial_input => si_bsc_v_din_3,
             mode => mode,
             serial_output => si_bsc_v_din_4,
             parallel_output => po_bsc_v_din_3);
bsc_v_din_2:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_2,
             serial_input => si_bsc_v_din_2,
             mode => mode,
             serial_output => si_bsc_v_din_3,
             parallel_output => po_bsc_v_din_2);
bsc_v_din_1:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_1,
             serial_input => si_bsc_v_din_1,
             mode => mode,
             serial_output => si_bsc_v_din_2,
             parallel_output => po_bsc_v_din_1);
bsc_v_din_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_v_din_0,
             serial_input => si_bsc_v_din_0,
             mode => mode,
             serial_output => si_bsc_v_din_1,
             parallel_output => po_bsc_v_din_0);
bsc_ld_n:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_ld_n,
             serial_input => si_bsc_ld_n,
             mode => mode,
             serial_output => si_bsc_v_din_0,
             parallel_output => po_bsc_ld_n);
bsc_rst_n:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_rst_n,
             serial_input => si_bsc_rst_n,
             mode => mode,
             serial_output => si_bsc_ld_n,
             parallel_output => po_bsc_rst_n);
bsc_up_dn:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_up_dn,
             serial_input => si_bsc_up_dn,
             mode => mode,
             serial_output => si_bsc_rst_n,
             parallel_output => po_bsc_up_dn);
bsc_WEN_0:
  bc_1
    port map(tck => tck,
             shiftdr => shiftdr,
             clockdr => clockdr_i,
             updatedr => updatedr_i,
             parallel_input => pi_bsc_WEN_0,
             serial_input => bsr_in,
             mode => mode,
             serial_output => si_bsc_up_dn,
             parallel_output => po_bsc_WEN_0);
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity io_pad_out is
  port (
    dout : in std_logic ;
    pin : out std_logic );
end io_pad_out;

architecture io_pad_out of io_pad_out is
-- <<< JTAG_119, JTAG_121
begin
  pin <= dout;
end io_pad_out;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

-- Copyright Mentor Graphics Corporation 2007  All Rights Reserved.
entity io_pad_in is
  port (
    pin : in std_logic ;
    din : out std_logic );
end io_pad_in;

architecture io_pad_in of io_pad_in is
-- <<< JTAG_119, JTAG_121
begin
  din <= pin;
end io_pad_in;

library ieee;
use ieee.std_logic_1164.all;
use WORK.ALL;

entity tri_enable_high is
  port (
    dout : in std_ulogic ;
    oe : in std_ulogic ;
    pin : out std_ulogic );
end tri_enable_high;

architecture tri_enable_high of tri_enable_high is
-- <<< JTAG_119, JTAG_121
begin
  b456_line_122 : 
  process (oe, dout)
  begin
    if ( (oe = '1') ) then
      pin <= dout;
    else
      pin <= 'Z';
    end if;
  end process b456_line_122;
end tri_enable_high;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity pad_instance_1 is
  port (
    pi_pad_v_DO_0_0 : in std_ulogic ;
    po_pad_v_DO_0_0 : out std_ulogic ;
    pi_pad_v_DO_0_1 : in std_ulogic ;
    po_pad_v_DO_0_1 : out std_ulogic ;
    pi_pad_v_DO_0_2 : in std_ulogic ;
    po_pad_v_DO_0_2 : out std_ulogic ;
    pi_pad_v_DO_0_3 : in std_ulogic ;
    po_pad_v_DO_0_3 : out std_ulogic ;
    pi_pad_v_DO_0_4 : in std_ulogic ;
    po_pad_v_DO_0_4 : out std_ulogic ;
    pi_pad_v_DO_0_5 : in std_ulogic ;
    po_pad_v_DO_0_5 : out std_ulogic ;
    pi_pad_v_DO_0_6 : in std_ulogic ;
    po_pad_v_DO_0_6 : out std_ulogic ;
    pi_pad_v_DO_0_7 : in std_ulogic ;
    po_pad_v_DO_0_7 : out std_ulogic ;
    pi_pad_v_dout_0 : in std_ulogic ;
    po_pad_v_dout_0 : out std_ulogic ;
    pi_pad_v_dout_1 : in std_ulogic ;
    po_pad_v_dout_1 : out std_ulogic ;
    pi_pad_v_dout_2 : in std_ulogic ;
    po_pad_v_dout_2 : out std_ulogic ;
    pi_pad_v_dout_3 : in std_ulogic ;
    po_pad_v_dout_3 : out std_ulogic ;
    pi_pad_v_dout_4 : in std_ulogic ;
    po_pad_v_dout_4 : out std_ulogic ;
    pi_pad_v_dout_5 : in std_ulogic ;
    po_pad_v_dout_5 : out std_ulogic ;
    pi_pad_v_dout_6 : in std_ulogic ;
    po_pad_v_dout_6 : out std_ulogic ;
    pi_pad_v_dout_7 : in std_ulogic ;
    po_pad_v_dout_7 : out std_ulogic ;
    pi_pad_tc : in std_ulogic ;
    po_pad_tc : out std_ulogic ;
    pi_pad_zero : in std_ulogic ;
    po_pad_zero : out std_ulogic ;
    pi_pad_v_A_0_0 : in std_ulogic ;
    po_pad_v_A_0_0 : out std_ulogic ;
    pi_pad_v_A_0_1 : in std_ulogic ;
    po_pad_v_A_0_1 : out std_ulogic ;
    pi_pad_v_A_0_2 : in std_ulogic ;
    po_pad_v_A_0_2 : out std_ulogic ;
    pi_pad_v_A_0_3 : in std_ulogic ;
    po_pad_v_A_0_3 : out std_ulogic ;
    pi_pad_v_A_0_4 : in std_ulogic ;
    po_pad_v_A_0_4 : out std_ulogic ;
    pi_pad_cen : in std_ulogic ;
    po_pad_cen : out std_ulogic ;
    pi_pad_clk : in std_ulogic ;
    po_pad_clk : out std_ulogic ;
    pi_pad_v_DI_0_0 : in std_ulogic ;
    po_pad_v_DI_0_0 : out std_ulogic ;
    pi_pad_v_DI_0_1 : in std_ulogic ;
    po_pad_v_DI_0_1 : out std_ulogic ;
    pi_pad_v_DI_0_2 : in std_ulogic ;
    po_pad_v_DI_0_2 : out std_ulogic ;
    pi_pad_v_DI_0_3 : in std_ulogic ;
    po_pad_v_DI_0_3 : out std_ulogic ;
    pi_pad_v_DI_0_4 : in std_ulogic ;
    po_pad_v_DI_0_4 : out std_ulogic ;
    pi_pad_v_DI_0_5 : in std_ulogic ;
    po_pad_v_DI_0_5 : out std_ulogic ;
    pi_pad_v_DI_0_6 : in std_ulogic ;
    po_pad_v_DI_0_6 : out std_ulogic ;
    pi_pad_v_DI_0_7 : in std_ulogic ;
    po_pad_v_DI_0_7 : out std_ulogic ;
    pi_pad_v_din_0 : in std_ulogic ;
    po_pad_v_din_0 : out std_ulogic ;
    pi_pad_v_din_1 : in std_ulogic ;
    po_pad_v_din_1 : out std_ulogic ;
    pi_pad_v_din_2 : in std_ulogic ;
    po_pad_v_din_2 : out std_ulogic ;
    pi_pad_v_din_3 : in std_ulogic ;
    po_pad_v_din_3 : out std_ulogic ;
    pi_pad_v_din_4 : in std_ulogic ;
    po_pad_v_din_4 : out std_ulogic ;
    pi_pad_v_din_5 : in std_ulogic ;
    po_pad_v_din_5 : out std_ulogic ;
    pi_pad_v_din_6 : in std_ulogic ;
    po_pad_v_din_6 : out std_ulogic ;
    pi_pad_v_din_7 : in std_ulogic ;
    po_pad_v_din_7 : out std_ulogic ;
    pi_pad_ld_n : in std_ulogic ;
    po_pad_ld_n : out std_ulogic ;
    pi_pad_rst_n : in std_ulogic ;
    po_pad_rst_n : out std_ulogic ;
    pi_pad_up_dn : in std_ulogic ;
    po_pad_up_dn : out std_ulogic ;
    pi_pad_WEN_0 : in std_ulogic ;
    po_pad_WEN_0 : out std_ulogic ;
    pi_pad_tdi : in std_ulogic ;
    po_pad_tdi : out std_ulogic ;
    pi_pad_tms : in std_ulogic ;
    po_pad_tms : out std_ulogic ;
    pi_pad_tck : in std_ulogic ;
    po_pad_tck : out std_ulogic ;
    tdo_in : in std_ulogic ;
    tdo_out : out std_ulogic ;
    tdo_enable : in std_ulogic ;
    pi_pad_trst : in std_ulogic ;
    po_pad_trst : out std_ulogic );
end pad_instance_1;

architecture rtl of pad_instance_1 is
-- <<< JTAG_117, JTAG_119
  component io_pad_out
    port (
      dout : in std_logic ;
      pin : out std_logic );
  end component;

  component io_pad_in
    port (
      pin : in std_logic ;
      din : out std_logic );
  end component;

  component tri_enable_high
    port (
      dout : in std_ulogic ;
      oe : in std_ulogic ;
      pin : out std_ulogic );
  end component;

begin
pad_v_DO_0_0:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_0,
             pin => po_pad_v_DO_0_0);
pad_v_DO_0_1:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_1,
             pin => po_pad_v_DO_0_1);
pad_v_DO_0_2:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_2,
             pin => po_pad_v_DO_0_2);
pad_v_DO_0_3:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_3,
             pin => po_pad_v_DO_0_3);
pad_v_DO_0_4:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_4,
             pin => po_pad_v_DO_0_4);
pad_v_DO_0_5:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_5,
             pin => po_pad_v_DO_0_5);
pad_v_DO_0_6:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_6,
             pin => po_pad_v_DO_0_6);
pad_v_DO_0_7:
  io_pad_out
    port map(dout => pi_pad_v_DO_0_7,
             pin => po_pad_v_DO_0_7);
pad_v_dout_0:
  io_pad_out
    port map(dout => pi_pad_v_dout_0,
             pin => po_pad_v_dout_0);
pad_v_dout_1:
  io_pad_out
    port map(dout => pi_pad_v_dout_1,
             pin => po_pad_v_dout_1);
pad_v_dout_2:
  io_pad_out
    port map(dout => pi_pad_v_dout_2,
             pin => po_pad_v_dout_2);
pad_v_dout_3:
  io_pad_out
    port map(dout => pi_pad_v_dout_3,
             pin => po_pad_v_dout_3);
pad_v_dout_4:
  io_pad_out
    port map(dout => pi_pad_v_dout_4,
             pin => po_pad_v_dout_4);
pad_v_dout_5:
  io_pad_out
    port map(dout => pi_pad_v_dout_5,
             pin => po_pad_v_dout_5);
pad_v_dout_6:
  io_pad_out
    port map(dout => pi_pad_v_dout_6,
             pin => po_pad_v_dout_6);
pad_v_dout_7:
  io_pad_out
    port map(dout => pi_pad_v_dout_7,
             pin => po_pad_v_dout_7);
pad_tc:
  io_pad_out
    port map(dout => pi_pad_tc,
             pin => po_pad_tc);
pad_zero:
  io_pad_out
    port map(dout => pi_pad_zero,
             pin => po_pad_zero);
pad_v_A_0_0:
  io_pad_in
    port map(pin => pi_pad_v_A_0_0,
             din => po_pad_v_A_0_0);
pad_v_A_0_1:
  io_pad_in
    port map(pin => pi_pad_v_A_0_1,
             din => po_pad_v_A_0_1);
pad_v_A_0_2:
  io_pad_in
    port map(pin => pi_pad_v_A_0_2,
             din => po_pad_v_A_0_2);
pad_v_A_0_3:
  io_pad_in
    port map(pin => pi_pad_v_A_0_3,
             din => po_pad_v_A_0_3);
pad_v_A_0_4:
  io_pad_in
    port map(pin => pi_pad_v_A_0_4,
             din => po_pad_v_A_0_4);
pad_cen:
  io_pad_in
    port map(pin => pi_pad_cen,
             din => po_pad_cen);
pad_clk:
  io_pad_in
    port map(pin => pi_pad_clk,
             din => po_pad_clk);
pad_v_DI_0_0:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_0,
             din => po_pad_v_DI_0_0);
pad_v_DI_0_1:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_1,
             din => po_pad_v_DI_0_1);
pad_v_DI_0_2:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_2,
             din => po_pad_v_DI_0_2);
pad_v_DI_0_3:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_3,
             din => po_pad_v_DI_0_3);
pad_v_DI_0_4:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_4,
             din => po_pad_v_DI_0_4);
pad_v_DI_0_5:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_5,
             din => po_pad_v_DI_0_5);
pad_v_DI_0_6:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_6,
             din => po_pad_v_DI_0_6);
pad_v_DI_0_7:
  io_pad_in
    port map(pin => pi_pad_v_DI_0_7,
             din => po_pad_v_DI_0_7);
pad_v_din_0:
  io_pad_in
    port map(pin => pi_pad_v_din_0,
             din => po_pad_v_din_0);
pad_v_din_1:
  io_pad_in
    port map(pin => pi_pad_v_din_1,
             din => po_pad_v_din_1);
pad_v_din_2:
  io_pad_in
    port map(pin => pi_pad_v_din_2,
             din => po_pad_v_din_2);
pad_v_din_3:
  io_pad_in
    port map(pin => pi_pad_v_din_3,
             din => po_pad_v_din_3);
pad_v_din_4:
  io_pad_in
    port map(pin => pi_pad_v_din_4,
             din => po_pad_v_din_4);
pad_v_din_5:
  io_pad_in
    port map(pin => pi_pad_v_din_5,
             din => po_pad_v_din_5);
pad_v_din_6:
  io_pad_in
    port map(pin => pi_pad_v_din_6,
             din => po_pad_v_din_6);
pad_v_din_7:
  io_pad_in
    port map(pin => pi_pad_v_din_7,
             din => po_pad_v_din_7);
pad_ld_n:
  io_pad_in
    port map(pin => pi_pad_ld_n,
             din => po_pad_ld_n);
pad_rst_n:
  io_pad_in
    port map(pin => pi_pad_rst_n,
             din => po_pad_rst_n);
pad_up_dn:
  io_pad_in
    port map(pin => pi_pad_up_dn,
             din => po_pad_up_dn);
pad_WEN_0:
  io_pad_in
    port map(pin => pi_pad_WEN_0,
             din => po_pad_WEN_0);
pad_tdi:
  io_pad_in
    port map(pin => pi_pad_tdi,
             din => po_pad_tdi);
pad_tms:
  io_pad_in
    port map(pin => pi_pad_tms,
             din => po_pad_tms);
pad_tck:
  io_pad_in
    port map(pin => pi_pad_tck,
             din => po_pad_tck);
pad_tdo:
  tri_enable_high
    port map(dout => tdo_in,
             oe => tdo_enable,
             pin => tdo_out);
pad_trst:
  io_pad_in
    port map(pin => pi_pad_trst,
             din => po_pad_trst);
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity mbist_reg is
  port (
    si : in std_ulogic ;
    so : out std_ulogic ;
    clockdr : in std_ulogic ;
    shiftdr : in std_ulogic ;
    reg_en : in std_ulogic ;
    tck : in std_ulogic ;
    resetl : in std_ulogic ;
    updatedr : in std_ulogic ;
    pi_0 : in std_ulogic ;
    pi_1 : in std_ulogic ;
    po_2 : out std_ulogic );
end mbist_reg;

architecture rtl of mbist_reg is
-- <<< JTAG_132, JTAG_133
  signal clockdr_reg : std_ulogic ;
  signal shift_only : std_ulogic ;
  signal updatedr_reg : std_ulogic ;
  signal nx_capture_mux_0_o : std_ulogic ;
  signal nx_capture_mux_1_o : std_ulogic ;
  signal nx_ff_0_q : std_ulogic ;
  signal nx_ff_1_q : std_ulogic ;
  signal nx_ff_2_q : std_ulogic ;
  signal nx_hold_mux_0_o : std_ulogic ;
  signal nx_hold_mux_1_o : std_ulogic ;
  signal nx_hold_mux_2_o : std_ulogic ;
  signal nx_upd_ff_2_q : std_ulogic ;
  signal nx_upd_hmux_2_o : std_ulogic ;
begin
  po_2 <= nx_upd_ff_2_q;
  so <= nx_ff_0_q;
  clockdr_reg <= (clockdr and reg_en);
  updatedr_reg <= (updatedr and reg_en);
  shift_only <= (clockdr and shiftdr and reg_en);
  process (tck, resetl)
  begin 
    if ((not resetl) = '1')
    then 
      nx_ff_0_q <= '0';
    elsif (tck'event and tck = '1')
    then 
      nx_ff_0_q <= nx_hold_mux_0_o;
    end if;
  end process;
  process (tck, resetl)
  begin 
    if ((not resetl) = '1')
    then 
      nx_ff_1_q <= '0';
    elsif (tck'event and tck = '1')
    then 
      nx_ff_1_q <= nx_hold_mux_1_o;
    end if;
  end process;
  process (tck, resetl)
  begin 
    if ((not resetl) = '1')
    then 
      nx_ff_2_q <= '0';
    elsif (tck'event and tck = '1')
    then 
      nx_ff_2_q <= nx_hold_mux_2_o;
    end if;
  end process;
  process (tck, resetl)
  begin 
    if ((not resetl) = '1')
    then 
      nx_upd_ff_2_q <= '0';
    elsif (tck'event and tck = '0')
    then 
      nx_upd_ff_2_q <= nx_upd_hmux_2_o;
    end if;
  end process;
  hold_mux_0 : 
  process (nx_ff_0_q, nx_capture_mux_0_o, clockdr_reg)
  begin
    if ( clockdr_reg = '1' ) then
      nx_hold_mux_0_o <= nx_capture_mux_0_o;
    else
      nx_hold_mux_0_o <= nx_ff_0_q;
    end if;
  end process hold_mux_0;
  capture_mux_0 : 
  process (pi_0, nx_ff_1_q, shiftdr)
  begin
    if ( shiftdr = '1' ) then
      nx_capture_mux_0_o <= nx_ff_1_q;
    else
      nx_capture_mux_0_o <= pi_0;
    end if;
  end process capture_mux_0;
  hold_mux_1 : 
  process (nx_ff_1_q, nx_capture_mux_1_o, clockdr_reg)
  begin
    if ( clockdr_reg = '1' ) then
      nx_hold_mux_1_o <= nx_capture_mux_1_o;
    else
      nx_hold_mux_1_o <= nx_ff_1_q;
    end if;
  end process hold_mux_1;
  capture_mux_1 : 
  process (pi_1, nx_ff_2_q, shiftdr)
  begin
    if ( shiftdr = '1' ) then
      nx_capture_mux_1_o <= nx_ff_2_q;
    else
      nx_capture_mux_1_o <= pi_1;
    end if;
  end process capture_mux_1;
  hold_mux_2 : 
  process (nx_ff_2_q, si, shift_only)
  begin
    if ( shift_only = '1' ) then
      nx_hold_mux_2_o <= si;
    else
      nx_hold_mux_2_o <= nx_ff_2_q;
    end if;
  end process hold_mux_2;
  upd_hmux_2 : 
  process (nx_upd_ff_2_q, nx_ff_2_q, updatedr_reg)
  begin
    if ( updatedr_reg = '1' ) then
      nx_upd_hmux_2_o <= nx_ff_2_q;
    else
      nx_upd_hmux_2_o <= nx_upd_ff_2_q;
    end if;
  end process upd_hmux_2;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use WORK.ALL;

entity ip_core_bscan is
  port (
    --** RAM SIGNALS
    A_0 : in std_ulogic_vector (4 downto 0);
    DI_0 : in std_ulogic_vector (7 downto 0);
    WEN_0 : in std_ulogic ;
    DO_0 : out std_ulogic_vector (7 downto 0);
    
    --** Counter signals
    clk : in std_ulogic ;
    rst_n : in std_ulogic ;
    cen : in std_ulogic ;
    ld_n : in std_ulogic ;
    up_dn : in std_ulogic ;
    din : in std_ulogic_vector (7 downto 0);
    dout : out std_ulogic_vector (7 downto 0);
    tc : out std_ulogic ;
    zero : out std_ulogic ;
    
    -- JTAG SIGNALS
    tdi : in std_ulogic := '1' ;
    tms : in std_ulogic := '1' ;
    tck : in std_ulogic ;
    tdo : out std_ulogic ;
    trst : in std_ulogic := '1' );
end ip_core_bscan;

architecture rtl of ip_core_bscan is
-- <<< JTAG_119, JTAG_121
  component tap
    port (
      tdi : in std_ulogic ;
      tdo : out std_ulogic ;
      tms : in std_ulogic ;
      tck : in std_ulogic ;
      boundary_in : in std_ulogic ;
      mbist_reg_in : in std_ulogic ;
      mode : out std_ulogic ;
      mbist_reg_reset : out std_ulogic ;
      mbist_instr : out std_ulogic ;
      trstl : in std_ulogic ;
      enable : out std_ulogic ;
      shiftdr : out std_ulogic ;
      clockdr : out std_ulogic ;
      updatedr : out std_ulogic ;
      resetl : out std_ulogic ;
      updateir : out std_ulogic ;
      clockdr_bsr : out std_ulogic ;
      updatedr_bsr : out std_ulogic ;
      capturedr_bsr : out std_ulogic ;
      tap_mbist_clk_o : out std_ulogic ;
      tap_mbist_rst_n_o : out std_ulogic );
  end component;

  component ip_core
    port (
      clk : in std_ulogic ;
      rst_n : in std_ulogic ;
      ld_n : in std_ulogic ;
      cen : in std_ulogic ;
      up_dn : in std_ulogic ;
      din : in std_ulogic_vector (7 downto 0);
      tc : out std_ulogic ;
      zero : out std_ulogic ;
      dout : out std_ulogic_vector (7 downto 0);
      mbist_clk : in std_ulogic ;
      mbist_rst_n : in std_ulogic ;
      test_h : in std_ulogic ;
      A_0 : in std_ulogic_vector (4 downto 0);
      DI_0 : in std_ulogic_vector (7 downto 0);
      WEN_0 : in std_ulogic ;
      DO_0 : out std_ulogic_vector (7 downto 0);
      tst_done : out std_ulogic ;
      fail_h : out std_ulogic );
  end component;

  component bsr_instance_1
    port (
      bsr_in : in std_ulogic ;
      bsr_out : out std_ulogic ;
      mode : in std_ulogic ;
      shiftdr : in std_ulogic ;
      updatedr_i : in std_ulogic ;
      updatedr_o : in std_ulogic ;
      clockdr_i : in std_ulogic ;
      clockdr_o : in std_ulogic ;
      tck : in std_ulogic ;
      pi_bsc_v_DO_0_7 : in std_ulogic ;
      po_bsc_v_DO_0_7 : out std_ulogic ;
      pi_bsc_v_DO_0_6 : in std_ulogic ;
      po_bsc_v_DO_0_6 : out std_ulogic ;
      pi_bsc_v_DO_0_5 : in std_ulogic ;
      po_bsc_v_DO_0_5 : out std_ulogic ;
      pi_bsc_v_DO_0_4 : in std_ulogic ;
      po_bsc_v_DO_0_4 : out std_ulogic ;
      pi_bsc_v_DO_0_3 : in std_ulogic ;
      po_bsc_v_DO_0_3 : out std_ulogic ;
      pi_bsc_v_DO_0_2 : in std_ulogic ;
      po_bsc_v_DO_0_2 : out std_ulogic ;
      pi_bsc_v_DO_0_1 : in std_ulogic ;
      po_bsc_v_DO_0_1 : out std_ulogic ;
      pi_bsc_v_DO_0_0 : in std_ulogic ;
      po_bsc_v_DO_0_0 : out std_ulogic ;
      pi_bsc_v_dout_7 : in std_ulogic ;
      po_bsc_v_dout_7 : out std_ulogic ;
      pi_bsc_v_dout_6 : in std_ulogic ;
      po_bsc_v_dout_6 : out std_ulogic ;
      pi_bsc_v_dout_5 : in std_ulogic ;
      po_bsc_v_dout_5 : out std_ulogic ;
      pi_bsc_v_dout_4 : in std_ulogic ;
      po_bsc_v_dout_4 : out std_ulogic ;
      pi_bsc_v_dout_3 : in std_ulogic ;
      po_bsc_v_dout_3 : out std_ulogic ;
      pi_bsc_v_dout_2 : in std_ulogic ;
      po_bsc_v_dout_2 : out std_ulogic ;
      pi_bsc_v_dout_1 : in std_ulogic ;
      po_bsc_v_dout_1 : out std_ulogic ;
      pi_bsc_v_dout_0 : in std_ulogic ;
      po_bsc_v_dout_0 : out std_ulogic ;
      pi_bsc_tc : in std_ulogic ;
      po_bsc_tc : out std_ulogic ;
      pi_bsc_zero : in std_ulogic ;
      po_bsc_zero : out std_ulogic ;
      pi_bsc_v_A_0_4 : in std_ulogic ;
      po_bsc_v_A_0_4 : out std_ulogic ;
      pi_bsc_v_A_0_3 : in std_ulogic ;
      po_bsc_v_A_0_3 : out std_ulogic ;
      pi_bsc_v_A_0_2 : in std_ulogic ;
      po_bsc_v_A_0_2 : out std_ulogic ;
      pi_bsc_v_A_0_1 : in std_ulogic ;
      po_bsc_v_A_0_1 : out std_ulogic ;
      pi_bsc_v_A_0_0 : in std_ulogic ;
      po_bsc_v_A_0_0 : out std_ulogic ;
      pi_bsc_cen : in std_ulogic ;
      po_bsc_cen : out std_ulogic ;
      pi_bsc_clk : in std_ulogic ;
      po_bsc_clk : out std_ulogic ;
      pi_bsc_v_DI_0_7 : in std_ulogic ;
      po_bsc_v_DI_0_7 : out std_ulogic ;
      pi_bsc_v_DI_0_6 : in std_ulogic ;
      po_bsc_v_DI_0_6 : out std_ulogic ;
      pi_bsc_v_DI_0_5 : in std_ulogic ;
      po_bsc_v_DI_0_5 : out std_ulogic ;
      pi_bsc_v_DI_0_4 : in std_ulogic ;
      po_bsc_v_DI_0_4 : out std_ulogic ;
      pi_bsc_v_DI_0_3 : in std_ulogic ;
      po_bsc_v_DI_0_3 : out std_ulogic ;
      pi_bsc_v_DI_0_2 : in std_ulogic ;
      po_bsc_v_DI_0_2 : out std_ulogic ;
      pi_bsc_v_DI_0_1 : in std_ulogic ;
      po_bsc_v_DI_0_1 : out std_ulogic ;
      pi_bsc_v_DI_0_0 : in std_ulogic ;
      po_bsc_v_DI_0_0 : out std_ulogic ;
      pi_bsc_v_din_7 : in std_ulogic ;
      po_bsc_v_din_7 : out std_ulogic ;
      pi_bsc_v_din_6 : in std_ulogic ;
      po_bsc_v_din_6 : out std_ulogic ;
      pi_bsc_v_din_5 : in std_ulogic ;
      po_bsc_v_din_5 : out std_ulogic ;
      pi_bsc_v_din_4 : in std_ulogic ;
      po_bsc_v_din_4 : out std_ulogic ;
      pi_bsc_v_din_3 : in std_ulogic ;
      po_bsc_v_din_3 : out std_ulogic ;
      pi_bsc_v_din_2 : in std_ulogic ;
      po_bsc_v_din_2 : out std_ulogic ;
      pi_bsc_v_din_1 : in std_ulogic ;
      po_bsc_v_din_1 : out std_ulogic ;
      pi_bsc_v_din_0 : in std_ulogic ;
      po_bsc_v_din_0 : out std_ulogic ;
      pi_bsc_ld_n : in std_ulogic ;
      po_bsc_ld_n : out std_ulogic ;
      pi_bsc_rst_n : in std_ulogic ;
      po_bsc_rst_n : out std_ulogic ;
      pi_bsc_up_dn : in std_ulogic ;
      po_bsc_up_dn : out std_ulogic ;
      pi_bsc_WEN_0 : in std_ulogic ;
      po_bsc_WEN_0 : out std_ulogic );
  end component;

  component pad_instance_1
    port (
      pi_pad_v_DO_0_0 : in std_ulogic ;
      po_pad_v_DO_0_0 : out std_ulogic ;
      pi_pad_v_DO_0_1 : in std_ulogic ;
      po_pad_v_DO_0_1 : out std_ulogic ;
      pi_pad_v_DO_0_2 : in std_ulogic ;
      po_pad_v_DO_0_2 : out std_ulogic ;
      pi_pad_v_DO_0_3 : in std_ulogic ;
      po_pad_v_DO_0_3 : out std_ulogic ;
      pi_pad_v_DO_0_4 : in std_ulogic ;
      po_pad_v_DO_0_4 : out std_ulogic ;
      pi_pad_v_DO_0_5 : in std_ulogic ;
      po_pad_v_DO_0_5 : out std_ulogic ;
      pi_pad_v_DO_0_6 : in std_ulogic ;
      po_pad_v_DO_0_6 : out std_ulogic ;
      pi_pad_v_DO_0_7 : in std_ulogic ;
      po_pad_v_DO_0_7 : out std_ulogic ;
      pi_pad_v_dout_0 : in std_ulogic ;
      po_pad_v_dout_0 : out std_ulogic ;
      pi_pad_v_dout_1 : in std_ulogic ;
      po_pad_v_dout_1 : out std_ulogic ;
      pi_pad_v_dout_2 : in std_ulogic ;
      po_pad_v_dout_2 : out std_ulogic ;
      pi_pad_v_dout_3 : in std_ulogic ;
      po_pad_v_dout_3 : out std_ulogic ;
      pi_pad_v_dout_4 : in std_ulogic ;
      po_pad_v_dout_4 : out std_ulogic ;
      pi_pad_v_dout_5 : in std_ulogic ;
      po_pad_v_dout_5 : out std_ulogic ;
      pi_pad_v_dout_6 : in std_ulogic ;
      po_pad_v_dout_6 : out std_ulogic ;
      pi_pad_v_dout_7 : in std_ulogic ;
      po_pad_v_dout_7 : out std_ulogic ;
      pi_pad_tc : in std_ulogic ;
      po_pad_tc : out std_ulogic ;
      pi_pad_zero : in std_ulogic ;
      po_pad_zero : out std_ulogic ;
      pi_pad_v_A_0_0 : in std_ulogic ;
      po_pad_v_A_0_0 : out std_ulogic ;
      pi_pad_v_A_0_1 : in std_ulogic ;
      po_pad_v_A_0_1 : out std_ulogic ;
      pi_pad_v_A_0_2 : in std_ulogic ;
      po_pad_v_A_0_2 : out std_ulogic ;
      pi_pad_v_A_0_3 : in std_ulogic ;
      po_pad_v_A_0_3 : out std_ulogic ;
      pi_pad_v_A_0_4 : in std_ulogic ;
      po_pad_v_A_0_4 : out std_ulogic ;
      pi_pad_cen : in std_ulogic ;
      po_pad_cen : out std_ulogic ;
      pi_pad_clk : in std_ulogic ;
      po_pad_clk : out std_ulogic ;
      pi_pad_v_DI_0_0 : in std_ulogic ;
      po_pad_v_DI_0_0 : out std_ulogic ;
      pi_pad_v_DI_0_1 : in std_ulogic ;
      po_pad_v_DI_0_1 : out std_ulogic ;
      pi_pad_v_DI_0_2 : in std_ulogic ;
      po_pad_v_DI_0_2 : out std_ulogic ;
      pi_pad_v_DI_0_3 : in std_ulogic ;
      po_pad_v_DI_0_3 : out std_ulogic ;
      pi_pad_v_DI_0_4 : in std_ulogic ;
      po_pad_v_DI_0_4 : out std_ulogic ;
      pi_pad_v_DI_0_5 : in std_ulogic ;
      po_pad_v_DI_0_5 : out std_ulogic ;
      pi_pad_v_DI_0_6 : in std_ulogic ;
      po_pad_v_DI_0_6 : out std_ulogic ;
      pi_pad_v_DI_0_7 : in std_ulogic ;
      po_pad_v_DI_0_7 : out std_ulogic ;
      pi_pad_v_din_0 : in std_ulogic ;
      po_pad_v_din_0 : out std_ulogic ;
      pi_pad_v_din_1 : in std_ulogic ;
      po_pad_v_din_1 : out std_ulogic ;
      pi_pad_v_din_2 : in std_ulogic ;
      po_pad_v_din_2 : out std_ulogic ;
      pi_pad_v_din_3 : in std_ulogic ;
      po_pad_v_din_3 : out std_ulogic ;
      pi_pad_v_din_4 : in std_ulogic ;
      po_pad_v_din_4 : out std_ulogic ;
      pi_pad_v_din_5 : in std_ulogic ;
      po_pad_v_din_5 : out std_ulogic ;
      pi_pad_v_din_6 : in std_ulogic ;
      po_pad_v_din_6 : out std_ulogic ;
      pi_pad_v_din_7 : in std_ulogic ;
      po_pad_v_din_7 : out std_ulogic ;
      pi_pad_ld_n : in std_ulogic ;
      po_pad_ld_n : out std_ulogic ;
      pi_pad_rst_n : in std_ulogic ;
      po_pad_rst_n : out std_ulogic ;
      pi_pad_up_dn : in std_ulogic ;
      po_pad_up_dn : out std_ulogic ;
      pi_pad_WEN_0 : in std_ulogic ;
      po_pad_WEN_0 : out std_ulogic ;
      pi_pad_tdi : in std_ulogic ;
      po_pad_tdi : out std_ulogic ;
      pi_pad_tms : in std_ulogic ;
      po_pad_tms : out std_ulogic ;
      pi_pad_tck : in std_ulogic ;
      po_pad_tck : out std_ulogic ;
      tdo_in : in std_ulogic ;
      tdo_out : out std_ulogic ;
      tdo_enable : in std_ulogic ;
      pi_pad_trst : in std_ulogic ;
      po_pad_trst : out std_ulogic );
  end component;

  component mbist_reg
    port (
      si : in std_ulogic ;
      so : out std_ulogic ;
      clockdr : in std_ulogic ;
      shiftdr : in std_ulogic ;
      reg_en : in std_ulogic ;
      tck : in std_ulogic ;
      resetl : in std_ulogic ;
      updatedr : in std_ulogic ;
      pi_0 : in std_ulogic ;
      pi_1 : in std_ulogic ;
      po_2 : out std_ulogic );
  end component;

  signal bsr_out : std_ulogic ;
  signal bsc_cen : std_ulogic ;
  signal bsc_clk : std_ulogic ;
  signal bsc_ld_n : std_ulogic ;
  signal bsc_rst_n : std_ulogic ;
  signal bsc_tc : std_ulogic ;
  signal bsc_up_dn : std_ulogic ;
  signal bsc_v_A_0_0 : std_ulogic ;
  signal bsc_v_A_0_1 : std_ulogic ;
  signal bsc_v_A_0_2 : std_ulogic ;
  signal bsc_v_A_0_3 : std_ulogic ;
  signal bsc_v_A_0_4 : std_ulogic ;
  signal bsc_v_DI_0_0 : std_ulogic ;
  signal bsc_v_DI_0_1 : std_ulogic ;
  signal bsc_v_DI_0_2 : std_ulogic ;
  signal bsc_v_DI_0_3 : std_ulogic ;
  signal bsc_v_DI_0_4 : std_ulogic ;
  signal bsc_v_DI_0_5 : std_ulogic ;
  signal bsc_v_DI_0_6 : std_ulogic ;
  signal bsc_v_DI_0_7 : std_ulogic ;
  signal bsc_v_din_0 : std_ulogic ;
  signal bsc_v_din_1 : std_ulogic ;
  signal bsc_v_din_2 : std_ulogic ;
  signal bsc_v_din_3 : std_ulogic ;
  signal bsc_v_din_4 : std_ulogic ;
  signal bsc_v_din_5 : std_ulogic ;
  signal bsc_v_din_6 : std_ulogic ;
  signal bsc_v_din_7 : std_ulogic ;
  signal bsc_v_DO_0_0 : std_ulogic ;
  signal bsc_v_DO_0_1 : std_ulogic ;
  signal bsc_v_DO_0_2 : std_ulogic ;
  signal bsc_v_DO_0_3 : std_ulogic ;
  signal bsc_v_DO_0_4 : std_ulogic ;
  signal bsc_v_DO_0_5 : std_ulogic ;
  signal bsc_v_DO_0_6 : std_ulogic ;
  signal bsc_v_DO_0_7 : std_ulogic ;
  signal bsc_v_dout_0 : std_ulogic ;
  signal bsc_v_dout_1 : std_ulogic ;
  signal bsc_v_dout_2 : std_ulogic ;
  signal bsc_v_dout_3 : std_ulogic ;
  signal bsc_v_dout_4 : std_ulogic ;
  signal bsc_v_dout_5 : std_ulogic ;
  signal bsc_v_dout_6 : std_ulogic ;
  signal bsc_v_dout_7 : std_ulogic ;
  signal bsc_WEN_0 : std_ulogic ;
  signal bsc_zero : std_ulogic ;
  signal ip_core_DO_0_7 : std_ulogic ;
  signal ip_core_DO_0_6 : std_ulogic ;
  signal ip_core_DO_0_5 : std_ulogic ;
  signal ip_core_DO_0_4 : std_ulogic ;
  signal ip_core_DO_0_3 : std_ulogic ;
  signal ip_core_DO_0_2 : std_ulogic ;
  signal ip_core_DO_0_1 : std_ulogic ;
  signal ip_core_DO_0_0 : std_ulogic ;
  signal ip_core_dout_7 : std_ulogic ;
  signal ip_core_dout_6 : std_ulogic ;
  signal ip_core_dout_5 : std_ulogic ;
  signal ip_core_dout_4 : std_ulogic ;
  signal ip_core_dout_3 : std_ulogic ;
  signal ip_core_dout_2 : std_ulogic ;
  signal ip_core_dout_1 : std_ulogic ;
  signal ip_core_dout_0 : std_ulogic ;
  signal ip_core_fail_h : std_ulogic ;
  signal ip_core_tc : std_ulogic ;
  signal ip_core_tst_done : std_ulogic ;
  signal ip_core_zero : std_ulogic ;
  signal pad_cen : std_ulogic ;
  signal pad_clk : std_ulogic ;
  signal pad_ld_n : std_ulogic ;
  signal pad_rst_n : std_ulogic ;
  signal pad_tck : std_ulogic ;
  signal pad_tdi : std_ulogic ;
  signal pad_tms : std_ulogic ;
  signal pad_trst : std_ulogic ;
  signal pad_up_dn : std_ulogic ;
  signal pad_v_A_0_0 : std_ulogic ;
  signal pad_v_A_0_1 : std_ulogic ;
  signal pad_v_A_0_2 : std_ulogic ;
  signal pad_v_A_0_3 : std_ulogic ;
  signal pad_v_A_0_4 : std_ulogic ;
  signal pad_v_DI_0_0 : std_ulogic ;
  signal pad_v_DI_0_1 : std_ulogic ;
  signal pad_v_DI_0_2 : std_ulogic ;
  signal pad_v_DI_0_3 : std_ulogic ;
  signal pad_v_DI_0_4 : std_ulogic ;
  signal pad_v_DI_0_5 : std_ulogic ;
  signal pad_v_DI_0_6 : std_ulogic ;
  signal pad_v_DI_0_7 : std_ulogic ;
  signal pad_v_din_0 : std_ulogic ;
  signal pad_v_din_1 : std_ulogic ;
  signal pad_v_din_2 : std_ulogic ;
  signal pad_v_din_3 : std_ulogic ;
  signal pad_v_din_4 : std_ulogic ;
  signal pad_v_din_5 : std_ulogic ;
  signal pad_v_din_6 : std_ulogic ;
  signal pad_v_din_7 : std_ulogic ;
  signal pad_WEN_0 : std_ulogic ;
  signal tap_capturedr_bsr : std_ulogic ;
  signal tap_clockdr : std_ulogic ;
  signal tap_clockdr_bsr : std_ulogic ;
  signal tap_enable : std_ulogic ;
  signal tap_mbist_instr : std_ulogic ;
  signal tap_mbist_reg_reset : std_ulogic ;
  signal tap_mode : std_ulogic ;
  signal tap_resetl : std_ulogic ;
  signal tap_shiftdr : std_ulogic ;
  signal bsda_tdo : std_ulogic ;
  signal tap_updatedr : std_ulogic ;
  signal tap_updatedr_bsr : std_ulogic ;
  signal tap_updateir : std_ulogic ;
  signal nx_mbist_reg_i_po_2 : std_ulogic ;
  signal nx_mbist_reg_i_so : std_ulogic ;
  signal nx_tap_i_tap_mbist_clk_o : std_ulogic ;
  signal nx_tap_i_tap_mbist_rst_n_o : std_ulogic ;
  signal n_2_DO_0 : std_ulogic_vector (7 downto 0);
  signal n_3_dout : std_ulogic_vector (7 downto 0);
begin
  DO_0 <= n_2_DO_0;
  dout <= n_3_dout;
tap_i:
  tap
    port map(tdi => pad_tdi,
             tdo => bsda_tdo,
             tms => pad_tms,
             tck => pad_tck,
             boundary_in => bsr_out,
             mbist_reg_in => nx_mbist_reg_i_so,
             mode => tap_mode,
             mbist_reg_reset => tap_mbist_reg_reset,
             mbist_instr => tap_mbist_instr,
             trstl => pad_trst,
             enable => tap_enable,
             shiftdr => tap_shiftdr,
             clockdr => tap_clockdr,
             updatedr => tap_updatedr,
             resetl => tap_resetl,
             updateir => tap_updateir,
             clockdr_bsr => tap_clockdr_bsr,
             updatedr_bsr => tap_updatedr_bsr,
             capturedr_bsr => tap_capturedr_bsr,
             tap_mbist_clk_o => nx_tap_i_tap_mbist_clk_o,
             tap_mbist_rst_n_o => nx_tap_i_tap_mbist_rst_n_o);
ip_core_i:
  ip_core
    port map(clk => bsc_clk,
             rst_n => bsc_rst_n,
             ld_n => bsc_ld_n,
             cen => bsc_cen,
             up_dn => bsc_up_dn,
             din(7) => bsc_v_din_7, din(6) => bsc_v_din_6, din(5) => bsc_v_din_5
               , din(4) => bsc_v_din_4, din(3) => bsc_v_din_3, din(2) => 
               bsc_v_din_2, din(1) => bsc_v_din_1, din(0) => bsc_v_din_0,
             tc => ip_core_tc,
             zero => ip_core_zero,
             dout(7) => ip_core_dout_7, dout(6) => ip_core_dout_6, dout(5) => 
               ip_core_dout_5, dout(4) => ip_core_dout_4, dout(3) => 
               ip_core_dout_3, dout(2) => ip_core_dout_2, dout(1) => 
               ip_core_dout_1, dout(0) => ip_core_dout_0,
             mbist_clk => nx_tap_i_tap_mbist_clk_o,
             mbist_rst_n => nx_tap_i_tap_mbist_rst_n_o,
             test_h => nx_mbist_reg_i_po_2,
             A_0(4) => bsc_v_A_0_4, A_0(3) => bsc_v_A_0_3, A_0(2) => bsc_v_A_0_2
               , A_0(1) => bsc_v_A_0_1, A_0(0) => bsc_v_A_0_0,
             DI_0(7) => bsc_v_DI_0_7, DI_0(6) => bsc_v_DI_0_6, DI_0(5) => 
               bsc_v_DI_0_5, DI_0(4) => bsc_v_DI_0_4, DI_0(3) => bsc_v_DI_0_3, 
               DI_0(2) => bsc_v_DI_0_2, DI_0(1) => bsc_v_DI_0_1, DI_0(0) => 
               bsc_v_DI_0_0,
             WEN_0 => bsc_WEN_0,
             DO_0(7) => ip_core_DO_0_7, DO_0(6) => ip_core_DO_0_6, DO_0(5) => 
               ip_core_DO_0_5, DO_0(4) => ip_core_DO_0_4, DO_0(3) => 
               ip_core_DO_0_3, DO_0(2) => ip_core_DO_0_2, DO_0(1) => 
               ip_core_DO_0_1, DO_0(0) => ip_core_DO_0_0,
             tst_done => ip_core_tst_done,
             fail_h => ip_core_fail_h);
bsr_i1:
  bsr_instance_1
    port map(bsr_in => pad_tdi,
             bsr_out => bsr_out,
             mode => tap_mode,
             shiftdr => tap_shiftdr,
             updatedr_i => tap_updatedr_bsr,
             updatedr_o => tap_updatedr_bsr,
             clockdr_i => tap_clockdr_bsr,
             clockdr_o => tap_clockdr_bsr,
             tck => pad_tck,
             pi_bsc_v_DO_0_7 => ip_core_DO_0_7,
             po_bsc_v_DO_0_7 => bsc_v_DO_0_7,
             pi_bsc_v_DO_0_6 => ip_core_DO_0_6,
             po_bsc_v_DO_0_6 => bsc_v_DO_0_6,
             pi_bsc_v_DO_0_5 => ip_core_DO_0_5,
             po_bsc_v_DO_0_5 => bsc_v_DO_0_5,
             pi_bsc_v_DO_0_4 => ip_core_DO_0_4,
             po_bsc_v_DO_0_4 => bsc_v_DO_0_4,
             pi_bsc_v_DO_0_3 => ip_core_DO_0_3,
             po_bsc_v_DO_0_3 => bsc_v_DO_0_3,
             pi_bsc_v_DO_0_2 => ip_core_DO_0_2,
             po_bsc_v_DO_0_2 => bsc_v_DO_0_2,
             pi_bsc_v_DO_0_1 => ip_core_DO_0_1,
             po_bsc_v_DO_0_1 => bsc_v_DO_0_1,
             pi_bsc_v_DO_0_0 => ip_core_DO_0_0,
             po_bsc_v_DO_0_0 => bsc_v_DO_0_0,
             pi_bsc_v_dout_7 => ip_core_dout_7,
             po_bsc_v_dout_7 => bsc_v_dout_7,
             pi_bsc_v_dout_6 => ip_core_dout_6,
             po_bsc_v_dout_6 => bsc_v_dout_6,
             pi_bsc_v_dout_5 => ip_core_dout_5,
             po_bsc_v_dout_5 => bsc_v_dout_5,
             pi_bsc_v_dout_4 => ip_core_dout_4,
             po_bsc_v_dout_4 => bsc_v_dout_4,
             pi_bsc_v_dout_3 => ip_core_dout_3,
             po_bsc_v_dout_3 => bsc_v_dout_3,
             pi_bsc_v_dout_2 => ip_core_dout_2,
             po_bsc_v_dout_2 => bsc_v_dout_2,
             pi_bsc_v_dout_1 => ip_core_dout_1,
             po_bsc_v_dout_1 => bsc_v_dout_1,
             pi_bsc_v_dout_0 => ip_core_dout_0,
             po_bsc_v_dout_0 => bsc_v_dout_0,
             pi_bsc_tc => ip_core_tc,
             po_bsc_tc => bsc_tc,
             pi_bsc_zero => ip_core_zero,
             po_bsc_zero => bsc_zero,
             pi_bsc_v_A_0_4 => pad_v_A_0_4,
             po_bsc_v_A_0_4 => bsc_v_A_0_4,
             pi_bsc_v_A_0_3 => pad_v_A_0_3,
             po_bsc_v_A_0_3 => bsc_v_A_0_3,
             pi_bsc_v_A_0_2 => pad_v_A_0_2,
             po_bsc_v_A_0_2 => bsc_v_A_0_2,
             pi_bsc_v_A_0_1 => pad_v_A_0_1,
             po_bsc_v_A_0_1 => bsc_v_A_0_1,
             pi_bsc_v_A_0_0 => pad_v_A_0_0,
             po_bsc_v_A_0_0 => bsc_v_A_0_0,
             pi_bsc_cen => pad_cen,
             po_bsc_cen => bsc_cen,
             pi_bsc_clk => pad_clk,
             po_bsc_clk => bsc_clk,
             pi_bsc_v_DI_0_7 => pad_v_DI_0_7,
             po_bsc_v_DI_0_7 => bsc_v_DI_0_7,
             pi_bsc_v_DI_0_6 => pad_v_DI_0_6,
             po_bsc_v_DI_0_6 => bsc_v_DI_0_6,
             pi_bsc_v_DI_0_5 => pad_v_DI_0_5,
             po_bsc_v_DI_0_5 => bsc_v_DI_0_5,
             pi_bsc_v_DI_0_4 => pad_v_DI_0_4,
             po_bsc_v_DI_0_4 => bsc_v_DI_0_4,
             pi_bsc_v_DI_0_3 => pad_v_DI_0_3,
             po_bsc_v_DI_0_3 => bsc_v_DI_0_3,
             pi_bsc_v_DI_0_2 => pad_v_DI_0_2,
             po_bsc_v_DI_0_2 => bsc_v_DI_0_2,
             pi_bsc_v_DI_0_1 => pad_v_DI_0_1,
             po_bsc_v_DI_0_1 => bsc_v_DI_0_1,
             pi_bsc_v_DI_0_0 => pad_v_DI_0_0,
             po_bsc_v_DI_0_0 => bsc_v_DI_0_0,
             pi_bsc_v_din_7 => pad_v_din_7,
             po_bsc_v_din_7 => bsc_v_din_7,
             pi_bsc_v_din_6 => pad_v_din_6,
             po_bsc_v_din_6 => bsc_v_din_6,
             pi_bsc_v_din_5 => pad_v_din_5,
             po_bsc_v_din_5 => bsc_v_din_5,
             pi_bsc_v_din_4 => pad_v_din_4,
             po_bsc_v_din_4 => bsc_v_din_4,
             pi_bsc_v_din_3 => pad_v_din_3,
             po_bsc_v_din_3 => bsc_v_din_3,
             pi_bsc_v_din_2 => pad_v_din_2,
             po_bsc_v_din_2 => bsc_v_din_2,
             pi_bsc_v_din_1 => pad_v_din_1,
             po_bsc_v_din_1 => bsc_v_din_1,
             pi_bsc_v_din_0 => pad_v_din_0,
             po_bsc_v_din_0 => bsc_v_din_0,
             pi_bsc_ld_n => pad_ld_n,
             po_bsc_ld_n => bsc_ld_n,
             pi_bsc_rst_n => pad_rst_n,
             po_bsc_rst_n => bsc_rst_n,
             pi_bsc_up_dn => pad_up_dn,
             po_bsc_up_dn => bsc_up_dn,
             pi_bsc_WEN_0 => pad_WEN_0,
             po_bsc_WEN_0 => bsc_WEN_0);
pad_i1:
  pad_instance_1
    port map(pi_pad_v_DO_0_0 => bsc_v_DO_0_0,
             po_pad_v_DO_0_0 => n_2_DO_0(0),
             pi_pad_v_DO_0_1 => bsc_v_DO_0_1,
             po_pad_v_DO_0_1 => n_2_DO_0(1),
             pi_pad_v_DO_0_2 => bsc_v_DO_0_2,
             po_pad_v_DO_0_2 => n_2_DO_0(2),
             pi_pad_v_DO_0_3 => bsc_v_DO_0_3,
             po_pad_v_DO_0_3 => n_2_DO_0(3),
             pi_pad_v_DO_0_4 => bsc_v_DO_0_4,
             po_pad_v_DO_0_4 => n_2_DO_0(4),
             pi_pad_v_DO_0_5 => bsc_v_DO_0_5,
             po_pad_v_DO_0_5 => n_2_DO_0(5),
             pi_pad_v_DO_0_6 => bsc_v_DO_0_6,
             po_pad_v_DO_0_6 => n_2_DO_0(6),
             pi_pad_v_DO_0_7 => bsc_v_DO_0_7,
             po_pad_v_DO_0_7 => n_2_DO_0(7),
             pi_pad_v_dout_0 => bsc_v_dout_0,
             po_pad_v_dout_0 => n_3_dout(0),
             pi_pad_v_dout_1 => bsc_v_dout_1,
             po_pad_v_dout_1 => n_3_dout(1),
             pi_pad_v_dout_2 => bsc_v_dout_2,
             po_pad_v_dout_2 => n_3_dout(2),
             pi_pad_v_dout_3 => bsc_v_dout_3,
             po_pad_v_dout_3 => n_3_dout(3),
             pi_pad_v_dout_4 => bsc_v_dout_4,
             po_pad_v_dout_4 => n_3_dout(4),
             pi_pad_v_dout_5 => bsc_v_dout_5,
             po_pad_v_dout_5 => n_3_dout(5),
             pi_pad_v_dout_6 => bsc_v_dout_6,
             po_pad_v_dout_6 => n_3_dout(6),
             pi_pad_v_dout_7 => bsc_v_dout_7,
             po_pad_v_dout_7 => n_3_dout(7),
             pi_pad_tc => bsc_tc,
             po_pad_tc => tc,
             pi_pad_zero => bsc_zero,
             po_pad_zero => zero,
             pi_pad_v_A_0_0 => A_0(0),
             po_pad_v_A_0_0 => pad_v_A_0_0,
             pi_pad_v_A_0_1 => A_0(1),
             po_pad_v_A_0_1 => pad_v_A_0_1,
             pi_pad_v_A_0_2 => A_0(2),
             po_pad_v_A_0_2 => pad_v_A_0_2,
             pi_pad_v_A_0_3 => A_0(3),
             po_pad_v_A_0_3 => pad_v_A_0_3,
             pi_pad_v_A_0_4 => A_0(4),
             po_pad_v_A_0_4 => pad_v_A_0_4,
             pi_pad_cen => cen,
             po_pad_cen => pad_cen,
             pi_pad_clk => clk,
             po_pad_clk => pad_clk,
             pi_pad_v_DI_0_0 => DI_0(0),
             po_pad_v_DI_0_0 => pad_v_DI_0_0,
             pi_pad_v_DI_0_1 => DI_0(1),
             po_pad_v_DI_0_1 => pad_v_DI_0_1,
             pi_pad_v_DI_0_2 => DI_0(2),
             po_pad_v_DI_0_2 => pad_v_DI_0_2,
             pi_pad_v_DI_0_3 => DI_0(3),
             po_pad_v_DI_0_3 => pad_v_DI_0_3,
             pi_pad_v_DI_0_4 => DI_0(4),
             po_pad_v_DI_0_4 => pad_v_DI_0_4,
             pi_pad_v_DI_0_5 => DI_0(5),
             po_pad_v_DI_0_5 => pad_v_DI_0_5,
             pi_pad_v_DI_0_6 => DI_0(6),
             po_pad_v_DI_0_6 => pad_v_DI_0_6,
             pi_pad_v_DI_0_7 => DI_0(7),
             po_pad_v_DI_0_7 => pad_v_DI_0_7,
             pi_pad_v_din_0 => din(0),
             po_pad_v_din_0 => pad_v_din_0,
             pi_pad_v_din_1 => din(1),
             po_pad_v_din_1 => pad_v_din_1,
             pi_pad_v_din_2 => din(2),
             po_pad_v_din_2 => pad_v_din_2,
             pi_pad_v_din_3 => din(3),
             po_pad_v_din_3 => pad_v_din_3,
             pi_pad_v_din_4 => din(4),
             po_pad_v_din_4 => pad_v_din_4,
             pi_pad_v_din_5 => din(5),
             po_pad_v_din_5 => pad_v_din_5,
             pi_pad_v_din_6 => din(6),
             po_pad_v_din_6 => pad_v_din_6,
             pi_pad_v_din_7 => din(7),
             po_pad_v_din_7 => pad_v_din_7,
             pi_pad_ld_n => ld_n,
             po_pad_ld_n => pad_ld_n,
             pi_pad_rst_n => rst_n,
             po_pad_rst_n => pad_rst_n,
             pi_pad_up_dn => up_dn,
             po_pad_up_dn => pad_up_dn,
             pi_pad_WEN_0 => WEN_0,
             po_pad_WEN_0 => pad_WEN_0,
             pi_pad_tdi => tdi,
             po_pad_tdi => pad_tdi,
             pi_pad_tms => tms,
             po_pad_tms => pad_tms,
             pi_pad_tck => tck,
             po_pad_tck => pad_tck,
             tdo_in => bsda_tdo,
             tdo_out => tdo,
             tdo_enable => tap_enable,
             pi_pad_trst => trst,
             po_pad_trst => pad_trst);
mbist_reg_i:
  mbist_reg
    port map(si => pad_tdi,
             so => nx_mbist_reg_i_so,
             clockdr => tap_clockdr,
             shiftdr => tap_shiftdr,
             reg_en => tap_mbist_instr,
             tck => pad_tck,
             resetl => tap_resetl,
             updatedr => tap_updatedr,
             pi_0 => ip_core_fail_h,
             pi_1 => ip_core_tst_done,
             po_2 => nx_mbist_reg_i_po_2);
end rtl;

