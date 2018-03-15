library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

library std;
use std.textio.all;

library work;
use work.all;
use work.bscan_tb_utils.all;

entity ip_core_bscan_tb is
end ip_core_bscan_tb;

architecture bsda_tb_arch of ip_core_bscan_tb is
  --signals for controlling TAP pins
  signal tck_r : std_ulogic;
  signal tms_r : std_ulogic;
  signal tdi_r : std_ulogic;
  signal trst_r : std_ulogic;
  signal tdo_s : std_ulogic;


  constant number_of_bscells : integer := 45;

  constant d50 : string(50 downto 1) := (others => '-');


  signal bscell_types : types_arr;
  signal bscell_delays : times_arr;
  signal bscell_init_values : signals_arr;

  --boundary register contents shifted out through tdo
  signal bsreg_value : signals_arr;
  signal inv_bsreg_value : signals_arr;

  --signals for connecting to input and output side BSCells
  signal bscell_inputs : signals_arr;
  signal bscell_diff_inputs : signals_arr;
  signal bscell_outputs : signals_arr;

  signal bscell_diff_outputs : signals_arr;

  --active value of the control bscells
  signal active_value_ctrl_bscells : signals_arr;

  --pull value of the bidi/tristate cells
  signal pull_value_bidi_tristate_cells : signals_arr;
  --active value of the open collector bscells
  signal active_value_open_collector_bscells : signals_arr;

  --for parametric test 
  signal para_pattern_z : signals_arr;
  signal para_pattern_0 : signals_arr;
  signal para_pattern_1 : signals_arr;
  signal para_pattern_01 : signals_arr;
  signal para_pattern_10 : signals_arr;

  --these enables trigger the various check tasks
  signal bypass_check_enable : boolean := false;
  signal sample_tdo_for_bsreg_value : boolean := false;
  signal boundary_check_0_enable : boolean := false;
  signal boundary_check_1_enable : boolean := false;
  signal boundary_check_01_enable : boolean := false;
  signal boundary_check_01_enable_sample : boolean := false;
  signal boundary_check_10_enable : boolean := false;
  signal boundary_check_10_enable_sample : boolean := false;
  signal capture_pattern_check_enable : boolean := false;
  signal ireg_shift_pause_check_enable : boolean := false;
  signal bsr_shift_pause_check_enable : boolean := false;
  signal mbist_instr_mbist_reg_check_enable : boolean := false;

  --variables to count simulation mismatches
  signal n_error_tdo : integer := 0;
  signal n_error_tdi : integer := 0;

  --variable to stop clock after simulations completes
  signal run_clock : boolean := true;

  --tap signals to be connected to the DUT
  signal tck : std_ulogic;
  signal tms : std_ulogic;
  signal tdi : std_ulogic;
  signal tdo : std_ulogic;
  signal trst : std_ulogic;

  --DUT core signals
  signal A_0 : std_ulogic_vector(4 downto 0);
  signal cen : std_ulogic;
  signal clk : std_ulogic;
  signal DI_0 : std_ulogic_vector(7 downto 0);
  signal din : std_ulogic_vector(7 downto 0);
  signal DO_0 : std_ulogic_vector(7 downto 0);
  signal dout : std_ulogic_vector(7 downto 0);
  signal ld_n : std_ulogic;
  signal rst_n : std_ulogic;
  signal tc : std_ulogic;
  signal up_dn : std_ulogic;
  signal WEN_0 : std_ulogic;
  signal zero : std_ulogic;


  --component declaration for the DUT
  component ip_core_bscan
    port(A_0 : in std_ulogic_vector(4 downto 0);
         cen : in std_ulogic;
         clk : in std_ulogic;
         DI_0 : in std_ulogic_vector(7 downto 0);
         din : in std_ulogic_vector(7 downto 0);
         DO_0 : out std_ulogic_vector(7 downto 0);
         dout : out std_ulogic_vector(7 downto 0);
         ld_n : in std_ulogic;
         rst_n : in std_ulogic;
         tc : out std_ulogic;
         up_dn : in std_ulogic;
         WEN_0 : in std_ulogic;
         zero : out std_ulogic;
         tdi : in std_ulogic;
         tms : in std_ulogic;
         tck : in std_ulogic;
         tdo : out std_ulogic;
         trst : in std_ulogic);
  end component;

begin
  --connect TB tap signals to DUT tap signals with pad delays
  tck <= tck_r'delayed(tck_delay);
  tms <= tms_r'delayed(tms_delay);
  tdi <= tdi_r'delayed(tdi_delay);
  tdo_s <= tdo;
  trst <= trst_r'delayed(trst_delay);

  --instantiate DUT
  chip : ip_core_bscan
    port map(A_0 => A_0,
             cen => cen,
             clk => clk,
             DI_0 => DI_0,
             din => din,
             DO_0 => DO_0,
             dout => dout,
             ld_n => ld_n,
             rst_n => rst_n,
             tc => tc,
             up_dn => up_dn,
             WEN_0 => WEN_0,
             zero => zero,
             tdi => tdi,
             tms => tms,
             tck => tck,
             tdo => tdo,
             trst => trst);

  bsr_chain : block
  begin
    --connect DUT I/O to bscell_inputs and bscell_outputs arrays
    bscell_outputs(0) <= DO_0(7);
    bscell_outputs(1) <= DO_0(6);
    bscell_outputs(2) <= DO_0(5);
    bscell_outputs(3) <= DO_0(4);
    bscell_outputs(4) <= DO_0(3);
    bscell_outputs(5) <= DO_0(2);
    bscell_outputs(6) <= DO_0(1);
    bscell_outputs(7) <= DO_0(0);
    bscell_outputs(8) <= dout(7);
    bscell_outputs(9) <= dout(6);
    bscell_outputs(10) <= dout(5);
    bscell_outputs(11) <= dout(4);
    bscell_outputs(12) <= dout(3);
    bscell_outputs(13) <= dout(2);
    bscell_outputs(14) <= dout(1);
    bscell_outputs(15) <= dout(0);
    bscell_outputs(16) <= tc;
    bscell_outputs(17) <= zero;
    A_0(4) <= bscell_inputs(18);
    A_0(3) <= bscell_inputs(19);
    A_0(2) <= bscell_inputs(20);
    A_0(1) <= bscell_inputs(21);
    A_0(0) <= bscell_inputs(22);
    cen <= bscell_inputs(23);
    clk <= bscell_inputs(24);
    DI_0(7) <= bscell_inputs(25);
    DI_0(6) <= bscell_inputs(26);
    DI_0(5) <= bscell_inputs(27);
    DI_0(4) <= bscell_inputs(28);
    DI_0(3) <= bscell_inputs(29);
    DI_0(2) <= bscell_inputs(30);
    DI_0(1) <= bscell_inputs(31);
    DI_0(0) <= bscell_inputs(32);
    din(7) <= bscell_inputs(33);
    din(6) <= bscell_inputs(34);
    din(5) <= bscell_inputs(35);
    din(4) <= bscell_inputs(36);
    din(3) <= bscell_inputs(37);
    din(2) <= bscell_inputs(38);
    din(1) <= bscell_inputs(39);
    din(0) <= bscell_inputs(40);
    ld_n <= bscell_inputs(41);
    rst_n <= bscell_inputs(42);
    up_dn <= bscell_inputs(43);
    WEN_0 <= bscell_inputs(44);

    --celltype assignments
    bscell_types(0) <= sig_type_output1;
    bscell_types(1) <= sig_type_output1;
    bscell_types(2) <= sig_type_output1;
    bscell_types(3) <= sig_type_output1;
    bscell_types(4) <= sig_type_output1;
    bscell_types(5) <= sig_type_output1;
    bscell_types(6) <= sig_type_output1;
    bscell_types(7) <= sig_type_output1;
    bscell_types(8) <= sig_type_output1;
    bscell_types(9) <= sig_type_output1;
    bscell_types(10) <= sig_type_output1;
    bscell_types(11) <= sig_type_output1;
    bscell_types(12) <= sig_type_output1;
    bscell_types(13) <= sig_type_output1;
    bscell_types(14) <= sig_type_output1;
    bscell_types(15) <= sig_type_output1;
    bscell_types(16) <= sig_type_output1;
    bscell_types(17) <= sig_type_output1;
    bscell_types(18) <= sig_type_input1;
    bscell_types(19) <= sig_type_input1;
    bscell_types(20) <= sig_type_input1;
    bscell_types(21) <= sig_type_input1;
    bscell_types(22) <= sig_type_input1;
    bscell_types(23) <= sig_type_input1;
    bscell_types(24) <= sig_type_input1;
    bscell_types(25) <= sig_type_input1;
    bscell_types(26) <= sig_type_input1;
    bscell_types(27) <= sig_type_input1;
    bscell_types(28) <= sig_type_input1;
    bscell_types(29) <= sig_type_input1;
    bscell_types(30) <= sig_type_input1;
    bscell_types(31) <= sig_type_input1;
    bscell_types(32) <= sig_type_input1;
    bscell_types(33) <= sig_type_input1;
    bscell_types(34) <= sig_type_input1;
    bscell_types(35) <= sig_type_input1;
    bscell_types(36) <= sig_type_input1;
    bscell_types(37) <= sig_type_input1;
    bscell_types(38) <= sig_type_input1;
    bscell_types(39) <= sig_type_input1;
    bscell_types(40) <= sig_type_input1;
    bscell_types(41) <= sig_type_input1;
    bscell_types(42) <= sig_type_input1;
    bscell_types(43) <= sig_type_input1;
    bscell_types(44) <= sig_type_input1;

    --initial values for input type bscells
    bscell_init_values(18) <= '1';
    bscell_init_values(19) <= '1';
    bscell_init_values(20) <= '1';
    bscell_init_values(21) <= '1';
    bscell_init_values(22) <= '1';
    bscell_init_values(23) <= '1';
    bscell_init_values(24) <= '1';
    bscell_init_values(25) <= '1';
    bscell_init_values(26) <= '1';
    bscell_init_values(27) <= '1';
    bscell_init_values(28) <= '1';
    bscell_init_values(29) <= '1';
    bscell_init_values(30) <= '1';
    bscell_init_values(31) <= '1';
    bscell_init_values(32) <= '1';
    bscell_init_values(33) <= '1';
    bscell_init_values(34) <= '1';
    bscell_init_values(35) <= '1';
    bscell_init_values(36) <= '1';
    bscell_init_values(37) <= '1';
    bscell_init_values(38) <= '1';
    bscell_init_values(39) <= '1';
    bscell_init_values(40) <= '1';
    bscell_init_values(41) <= '1';
    bscell_init_values(42) <= '1';
    bscell_init_values(43) <= '1';
    bscell_init_values(44) <= '1';

    --pad delays for input type bscells
    bscell_delays(18) <= 0 ns;
    bscell_delays(19) <= 0 ns;
    bscell_delays(20) <= 0 ns;
    bscell_delays(21) <= 0 ns;
    bscell_delays(22) <= 0 ns;
    bscell_delays(23) <= 0 ns;
    bscell_delays(24) <= 0 ns;
    bscell_delays(25) <= 0 ns;
    bscell_delays(26) <= 0 ns;
    bscell_delays(27) <= 0 ns;
    bscell_delays(28) <= 0 ns;
    bscell_delays(29) <= 0 ns;
    bscell_delays(30) <= 0 ns;
    bscell_delays(31) <= 0 ns;
    bscell_delays(32) <= 0 ns;
    bscell_delays(33) <= 0 ns;
    bscell_delays(34) <= 0 ns;
    bscell_delays(35) <= 0 ns;
    bscell_delays(36) <= 0 ns;
    bscell_delays(37) <= 0 ns;
    bscell_delays(38) <= 0 ns;
    bscell_delays(39) <= 0 ns;
    bscell_delays(40) <= 0 ns;
    bscell_delays(41) <= 0 ns;
    bscell_delays(42) <= 0 ns;
    bscell_delays(43) <= 0 ns;
    bscell_delays(44) <= 0 ns;


  end block;

  --clock generator
  tck_gen: process
  begin
    tck_r <= '0';
    wait for (tck_period/2);
    tck_r <= '1';
    wait for (tck_period-tck_period/2);
    if (run_clock = false) then
      wait;
    end if;
  end process;

  --process to trigger the various check_ tasks corresponding to
  --enables from the main process block (bsda_tb)
  tdo_check: process
  begin
    wait until(bypass_check_enable'event or
               ireg_shift_pause_check_enable'event or
               bsr_shift_pause_check_enable'event or
               sample_tdo_for_bsreg_value'event or
               capture_pattern_check_enable'event or
               mbist_instr_mbist_reg_check_enable'event or
               boundary_check_0_enable'event or
               boundary_check_1_enable'event or
               boundary_check_01_enable'event or
               boundary_check_01_enable_sample'event or
               boundary_check_10_enable_sample'event or
               boundary_check_10_enable'event);

	if (bypass_check_enable) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking bypass register output *** ");
      check_bypass_reg(n_error_tdo, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (ireg_shift_pause_check_enable) then
      wait for (ir_length*tck_period);
      message(n_error_tdo, 0, "Checking instruction register shift with pause");
      check_shift_pause_pattern(n_error_tdo, instr_bsr_overshift_pattern,
                              tdo_s);
    elsif (bsr_shift_pause_check_enable) then
      wait for (number_of_bscells*tck_period);
      message(n_error_tdo, 0, "Checking boundary register shift with pause");
      check_shift_pause_pattern(n_error_tdo, instr_bsr_overshift_pattern,
                              tdo_s);
    elsif (boundary_check_10_enable) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...1010' *** ");
      check_boundary_reg_with_patterns_10(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (boundary_check_01_enable) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...0101' *** ");
      check_boundary_reg_with_patterns_01(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (boundary_check_10_enable_sample) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...1010' *** ");
      check_boundary_reg_with_patterns_10_sample(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (boundary_check_01_enable_sample) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...0101' *** ");
      check_boundary_reg_with_patterns_01_sample(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (boundary_check_0_enable) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...0000' *** ");
      check_boundary_reg_with_patterns_0(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (boundary_check_1_enable) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking boundary register serial output with sampled inputs and bidir. inputs '...1111' *** ");
      check_boundary_reg_with_patterns_1(n_error_tdo, bscell_types, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (mbist_instr_mbist_reg_check_enable) then
      wait for (3*tck_period);
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** checking the usr defined instruction: mbist_instr shift value");
      check_overshift_pattern(n_error_tdo, user_reg_overshift_pattern, tdo_s);
      wait for tck_period;
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (capture_pattern_check_enable) then
      wait for (4*tck_period);
      message(n_error_tdo, 0, "Checking for capture pattern");
      check_capture_pattern(n_error_tdo, tdo_s);
      check_tdo_Z(n_error_tdo, tdo_s);
    elsif (sample_tdo_for_bsreg_value) then
      wait for (3*tck_period);
      message(n_error_tdo, 0, "*** shifting out the sampled core logic output signal values ***");
      sample_tdo(tdo_s, bsreg_value);
    end if;
  end process;

  --sequence of tests
  bsda_tb: process
    variable report_line : line;
    variable bsr_shift_pattern : signals_arr := (others => '0');
    variable userreg_shift_pattern : std_ulogic_vector(6 downto 0) :=
      (others => '0');
      
    --** added variable to track failure count before & aftereach test
    variable pre_test_failcount_v : integer;
    
  begin
  trst_r <= '1';

  tms_r <= '1';
    wait for (tck_delay + tck_period/2);

    wait for (tck_delay + 3*tck_period/4);

  trst_r <= '0';

    userreg_shift_pattern(3 downto 0) := user_reg_overshift_pattern;

    -- <<< REQ_TP_3.1
	--1. Initialization
    --reset TAP controller, initialize PI of input bscells
    --design specific logic to reset CORE should go here
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("Initialization"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Initializing BS logic by resetting TAP controller");
    async_tap_reset(trst_r, tms_r);
    message(n_error_tdi, 0, "Initializing PI of input bscells");
    initialize_input_bscells(bscell_inputs, bscell_diff_inputs, bscell_init_values,
                             bscell_delays, bscell_types,
                             tdi_r, tms_r);
    wait for tck_period;
    message(n_error_tdi, 0, "*** Boundary Scan logic has been reset ***");
    message(n_error_tdi, 0, "<<< REQ_TP_3.1 PASS");
		
    --2. Check instruction register after TAP reset
    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("Check instruction register after TAP reset"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Checking Instruction Register after TAP reset");
    check_default_instruction(n_error_tdi, tms_r, tdo_s);
	
    -- <<< REQ_TP_3.3
    --3. Test for capture-pattern
    --** Added to capture pre-test fail count
    pre_test_failcount_v := n_error_tdo;
    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("3. Test for capture-pattern"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    capture_pattern_check_enable <= true;
    set_instruction(bypass, tdi_r, tms_r);
    capture_pattern_check_enable <= false;

    --** Added immediate assertion for Test3 : Will be stored in the UCDB.
    --** Compares failcount pre & post test to see if more errors have occurred
    Test03_Assert: assert ((n_error_tdo - pre_test_failcount_v) = 0) report "### Incorrect Capture Pattern Detected (Test 3) ***" severity ERROR;
    if ((n_error_tdo - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.3 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.3 FAIL");
    end if;

    -- <<< REQ_TP_3.4
    --4. Run through all TAP controller states
    --** store pre-test fail count
    pre_test_failcount_v := n_error_tdo;

    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("Run through all TAP controller states"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Synchronously resetting TAP controller");
    sync_tap_reset(tms_r);
    message(n_error_tdi, 0, "Running all TAP controller states and state transitions");
    run_all_tap_states(tdi_r, tms_r,
                       ireg_shift_pause_check_enable,
                       bsr_shift_pause_check_enable,
                       extest);

    --** Added immediate assertion for Test4 : Will be stored in the UCDB.
    --** Compares failcount pre & post test to see if more errors have occurred
    Test04_Assert: assert ((n_error_tdo - pre_test_failcount_v) = 0) report "### Failed To Run Through All TAP States (Test 4) ***" severity ERROR;
    if ((n_error_tdo - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.4 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.4 FAIL");
    end if;

    -- <<< REQ_TP_3.5
    --5. Test MODE-MUX for all instructions
    --** Store the failure count prior to checking current results
    pre_test_failcount_v := n_error_tdo;

    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("Test MODE-MUX for all instructions"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    set_instruction(sample, tdi_r, tms_r);
    sample_tdo_for_bsreg_value <= true;
    insert_serial_data_tdi(bsr_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    sample_tdo_for_bsreg_value <= false;
    
    --** Modifed to pass 1 additional parameter, thus enabling move of procedure into packag
    --**    create_out_bscells_pattern_inverted(bscell_types, bsreg_value,
    --**                                    inv_bsreg_value);
    create_out_bscells_pattern_inverted(bscell_types, bsreg_value, active_value_ctrl_bscells, inv_bsreg_value);
                                        
    message(n_error_tdi, 0, "*** shifting the inverted core logic output signal values into the update register of the boundary scan cells ***");
    insert_serial_data_tdi(inv_bsreg_value, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);

    writeline(report_file, report_line);
    message(n_error_tdi, 0, "*** Testing Mode-mux during EXTEST instruction***");
    message(n_error_tdi, 0, "*** Loading instruction EXTEST ***");
    set_instruction(extest, tdi_r, tms_r);
        
    check_output_bscells_extest(n_error_tdi, bscell_outputs, bscell_diff_outputs,
                                bsreg_value, bscell_types, inv_bsreg_value);

    --** Added immediate assertion for Test5 : Will be stored in the UCDB.
    Test05_Assert: assert ((n_error_tdo - pre_test_failcount_v) = 0) report "### Incorrect Output Value During MODE-MUX Test Detected (Test 5) ***" severity ERROR;
    if ((n_error_tdo - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.5 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.5 FAIL");
    end if;

    -- <<< REQ_TP_3.6
    --6. Test for instruction: BYPASS
    --** Store the failure count prior to checking current results
    pre_test_failcount_v := n_error_tdi;

    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("6. Test for instruction: BYPASS"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Testing opcode: 1111 for BYPASS instruction");
    set_instruction(bypass, tdi_r, tms_r);
    bypass_check_enable <= true;
    insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    bypass_check_enable <= false;

    --** Added immediate assertion for Test6 : Will be stored in the UCDB.
    Test06_Assert: assert ((n_error_tdi - pre_test_failcount_v) = 0) report "### BYPASS INSTRUCTION TESTS FAILED (Test 6) ***" severity ERROR;
    if ((n_error_tdi - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.6 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.6 FAIL");
    end if;

    -- <<< REQ_TP_3.7
    --7. Test for instruction: SAMPLE
    --** Store the failure count prior to checking current results
    pre_test_failcount_v := n_error_tdi;
    
    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("7. Test for instruction: SAMPLE"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Testing opcode: 0001 for SAMPLE instruction");
    message(n_error_tdi, 0, "Loading opcode: 0001");
    set_instruction(sample, tdi_r, tms_r);
    message(n_error_tdi, 0, "*** setting input signals '...1010' ***");
    set_input_bscells_10_sample(bscell_inputs, bscell_diff_inputs, bscell_delays, bscell_types);
    boundary_check_10_enable_sample <= true;
    insert_serial_data_tdi(bsr_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    boundary_check_10_enable_sample <= false;
    message(n_error_tdi, 0, "*** setting bidi-input signals to 'z' ***");
    set_bidi_pads_Z(bscell_inputs, bscell_delays, bscell_types);

    message(n_error_tdi, 0, "*** setting input signals '...0101' ***");
    set_input_bscells_01_sample(bscell_inputs, bscell_diff_inputs, bscell_delays, bscell_types);
    boundary_check_01_enable_sample <= true;
    insert_serial_data_tdi(bsr_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    boundary_check_01_enable_sample <= false;

    message(n_error_tdi, 0, "*** setting bidi-input signals to 'z' ***");
    set_bidi_pads_Z(bscell_inputs, bscell_delays, bscell_types);


    message(n_error_tdi, 0, "Test for the PRELOAD functionality in SAMPLE_PRELOAD instruction");
    message(n_error_tdi, 0, "*** loading opcode: 0001***");
    check_preload_shift(n_error_tdi, tdi_r, tms_r, tdo_s);

    --** Added immediate assertion for Test7 : Will be stored in the UCDB.
    Test07_Assert: assert ((n_error_tdi - pre_test_failcount_v) = 0) report "### SAMPLE INSTRUCTION TESTS FAILED (Test 7) ***" severity ERROR;
    if ((n_error_tdi - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.7 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.7 FAIL");
    end if;

    -- <<< REQ_TP_3.8
    --8. Test for instruction: EXTEST
    --** Store the failure count prior to checking current results
    pre_test_failcount_v := n_error_tdo;

    writeline(report_file, report_line);
	message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("8. Test for instruction: EXTEST"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "Testing opcode: 0000 for EXTEST instruction");
    message(n_error_tdi, 0, "Loading opcode: 0000");
    set_instruction(extest, tdi_r, tms_r);
    message(n_error_tdi, 0, "Testing on input ports");
    
    --** modified to pass 2 additional parameters, thus enabling moving of procedure into VHDL package
    --** create_out_bscells_pattern_z(bscell_types, para_pattern_z);
    create_out_bscells_pattern_z(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_z);    
    
    message(n_error_tdi, 0, "*** setting input signals to 'z' ***");
    insert_serial_data_tdi(para_pattern_z, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** setting input signals to '...1010' ***");
    set_input_bscells_10(bscell_inputs, bscell_diff_inputs, bscell_delays, bscell_types);
    boundary_check_10_enable <= true;
    insert_serial_data_tdi(para_pattern_z, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    boundary_check_10_enable <= false;
    
    --** modified to pass 2 additional parameters, thus enabling moving of procedure into VHDL package
    --** create_out_bscells_pattern_z(bscell_types, para_pattern_z);
    create_out_bscells_pattern_z(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_z);    
    
    message(n_error_tdi, 0, "*** setting input signals to 'z' ***");
    insert_serial_data_tdi(para_pattern_z, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** setting input signals to '...0101' ***");
    set_input_bscells_01(bscell_inputs, bscell_diff_inputs, bscell_delays, bscell_types);
    boundary_check_01_enable <= true;
    insert_serial_data_tdi(para_pattern_z, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    boundary_check_01_enable <= false;

    message(n_error_tdi, 0, "Testing on output ports");
    message(n_error_tdi, 0, "*** creating active pattern to enable tri/bidi/oc and oc bidis ***");

    --** modified to pass 2 additional parameters, thus enabling moving of procedure into VHDL package
    --** create_out_bscells_pattern_0(bscell_types, para_pattern_0);
    create_out_bscells_pattern_0(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_0);   
   
    message(n_error_tdi, 0, "*** shifting in active pattern and zero pattern into the BSR ***");
    insert_serial_data_tdi(para_pattern_0, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** checking for zero on simple outputs, bidis, tristates,oc , oc bidis and diff outputs ***");
    check_output_bscells_0(n_error_tdi, bscell_outputs,  bscell_diff_outputs,bscell_types);
    message(n_error_tdi, 0, "*** creating active pattern to enable tri/bidi/oc and oc bidis ***");

    --** Modifed to pass 2 additional parameters, thus enabling move of procedure into packag
    --** create_out_bscells_pattern_1(bscell_types, para_pattern_1);
    create_out_bscells_pattern_1(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_1);
   
    message(n_error_tdi, 0, "*** shifting in active pattern and zero pattern into the BSR' ***");
    insert_serial_data_tdi(para_pattern_1, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** checking for one on simple outputs, bidis, tristates,oc , oc bidis and diff outputs ***");
    check_output_bscells_1(n_error_tdi, bscell_outputs,  bscell_diff_outputs,bscell_types);
    message(n_error_tdi, 0, "*** creating checkerboard pattern of 0101.. ***");
 
    --** Modifed to pass 2 additional parameters, thus enabling move of procedure into packag
    --** create_out_bscells_pattern_01(bscell_types, para_pattern_01);
    create_out_bscells_pattern_01(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_01);   
   
    message(n_error_tdi, 0, "*** shifting pattern of 0101..' ***");
    insert_serial_data_tdi(para_pattern_01, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** checking for 0101.. on simple outputs, bidis, tristates, oc, oc bidis and diff outputs ***");
    check_output_bscells_01(n_error_tdi, bscell_outputs,  bscell_diff_outputs,bscell_types);
    message(n_error_tdi, 0, "*** creating checkerboard pattern of 1010.. ***");

    --** Modifed to pass 2 additional parameters, thus enabling move of procedure into packag
    --** create_out_bscells_pattern_10(bscell_types, para_pattern_10);
    create_out_bscells_pattern_10(bscell_types, active_value_ctrl_bscells, active_value_open_collector_bscells, para_pattern_10);
   
    message(n_error_tdi, 0, "*** shifting pattern of 1010.. ***");
    insert_serial_data_tdi(para_pattern_10, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
    message(n_error_tdi, 0, "*** checking for 1010.. on simple outputs, bidis, tristates, oc, oc bidis and diff outputs ***");
    check_output_bscells_10(n_error_tdi, bscell_outputs,  bscell_diff_outputs,bscell_types);
    message(n_error_tdi, 0, "*** setting bidi-input signals to 'z' ***");
    set_bidi_pads_Z(bscell_inputs, bscell_delays, bscell_types);

    --** Compares failcount pre & post test to see if more errors have occurred
    Test08_Assert: assert ((n_error_tdo - pre_test_failcount_v) = 0) report "### Failed When Testing User Defined Instruction mbist1_ins (Test 8) ***" severity ERROR;
    if ((n_error_tdo - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.8 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.8 FAIL");
    end if;


    -- <<< REQ_TP_3.11
    --11. Test unassigned opcodes for equivalence to bypass
    --** Store the failure count prior to checking current results
    pre_test_failcount_v := n_error_tdo;
    
    writeline(report_file, report_line);
    message(n_error_tdi, 0, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    write(report_line, string'("11. Test unassigned opcodes for equivalence to bypass"));
    writeline(report_file, report_line);
    write(report_line, d50);
    writeline(report_file, report_line);
    if (test_unused_opcode_0111) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 0111");
      set_instruction(unassigned0, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1011) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1011");
      set_instruction(unassigned1, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1101) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1101");
      set_instruction(unassigned2, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1110) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1110");
      set_instruction(unassigned3, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1000) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1000");
      set_instruction(unassigned4, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_0100) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 0100");
      set_instruction(unassigned5, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_0101) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 0101");
      set_instruction(unassigned6, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1010) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1010");
      set_instruction(unassigned7, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_0110) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 0110");
      set_instruction(unassigned8, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1001) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1001");
      set_instruction(unassigned9, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    if (test_unused_opcode_1100) then
      writeline(report_file, report_line);
      message(n_error_tdi, 0, "Loading opcode: 1100");
      set_instruction(unassigned10, tdi_r, tms_r);
      bypass_check_enable <= true;
      insert_serial_data_tdi('0' & bypass_shift_pattern, tdi_r, tms_r, bscell_inputs, bscell_delays, bscell_types);
      bypass_check_enable <= false;
    end if;

    --** Compares failcount pre & post test to see if more errors have occurred
    Test11_Assert: assert ((n_error_tdo - pre_test_failcount_v) = 0) report "### Failed When Testing Unassigned Opcodes (Test 11) ***" severity ERROR;
    if ((n_error_tdo - pre_test_failcount_v) = 0) then
        message(n_error_tdi, 0, "<<< REQ_TP_3.11 PASS");
    else
        message(n_error_tdi, 0, "<<< REQ_TP_3.11 FAIL");
    end if;


    message(n_error_tdi, 3, "*** Simulation finished !!!' ***");
     run_clock <= false;
    wait;
  end process;
end bsda_tb_arch;

