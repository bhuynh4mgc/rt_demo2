library ieee;
use ieee.std_logic_1164.all;

library std;
use std.textio.all;

package bscan_tb_utils is 
  
  --------------------------------------
  --------------------------------------
  --
  --   CONSTANT DECLARATIONS
  --
  --------------------------------------
  --------------------------------------
  constant ir_length : integer := 4;
  constant trstL_implemented : boolean := true;

  type instruction is array(3 downto 0) of std_ulogic;
  constant extest          : instruction := "0000";
  constant bypass          : instruction := "1111";
  constant sample          : instruction := "0001";
  constant preload         : instruction := "0001";
  constant mbist_reg_reset : instruction := "0010";
  constant mbist_instr     : instruction := "0011";
  
  constant unassigned0 : instruction := "0111";
  constant unassigned1 : instruction := "1011";
  constant unassigned2 : instruction := "1101";
  constant unassigned3 : instruction := "1110";
  constant unassigned4 : instruction := "1000";
  constant unassigned5 : instruction := "0100";
  constant unassigned6 : instruction := "0101";
  constant unassigned7 : instruction := "1010";
  constant unassigned8 : instruction := "0110";
  constant unassigned9 : instruction := "1001";
  constant unassigned10 : instruction := "1100";

  constant capture_pattern : instruction := "0001";

  --true value for these parameters will enable testing of these
  --opcodes for equivalence to BYPASS instruction
  constant test_unused_opcode_0111 : boolean := true;
  constant test_unused_opcode_1011 : boolean := true;
  constant test_unused_opcode_1101 : boolean := true;
  constant test_unused_opcode_1110 : boolean := true;
  constant test_unused_opcode_1000 : boolean := true;
  constant test_unused_opcode_0100 : boolean := true;
  constant test_unused_opcode_0101 : boolean := true;
  constant test_unused_opcode_1010 : boolean := true;
  constant test_unused_opcode_0110 : boolean := true;
  constant test_unused_opcode_1001 : boolean := true;
  constant test_unused_opcode_1100 : boolean := true;

  constant number_of_bscells : integer := 45;

  constant d50 : string(50 downto 1) := (others => '-');

  --patterns for checking shift registers
  constant bypass_shift_pattern        : std_ulogic_vector(10 downto 0) := "11110011111";
  constant idreg_overshift_pattern     : std_ulogic_vector(3 downto 0)  := "1110";
  constant user_reg_overshift_pattern  : std_ulogic_vector(3 downto 0)  := "0111";
  constant instr_bsr_overshift_pattern : std_ulogic_vector(11 downto 0) :=
    "000010100111";

  --TCK period and TAP ports' pad delays
  constant tck_period : time := 200 ns;
  constant tck_delay  : time := 0 ns;
  constant tms_delay  : time := 0 ns;
  constant tdi_delay  : time := 0 ns;
  constant trst_delay : time := 0 ns;

  --------------------------------------
  --------------------------------------
  --
  --   TYPE DECLARATIONS
  --
  --------------------------------------
  --------------------------------------

  --types of boundary scan cells
  type bscell_sigtype is (sig_type_output1,
                          sig_type_tristate,
                          sig_type_diff_output,
                          sig_type_open_drain,
                          sig_type_control,
                          sig_type_input1,
                          sig_type_bidi_input,
                          sig_type_reset,
                          sig_type_clock,
                          sig_type_bidi,
                          sig_type_diff_input,
                          sig_type_diff_output_0,
                          sig_type_diff_output_1,
                          sig_type_open_collector_output_0,
                          sig_type_open_collector_output_1,
                          sig_type_bidi_output,
						              sig_type_bidi_pull0,
                  					   sig_type_bidi_pull1,
                          sig_type_bidi_weak0,
                          sig_type_bidi_weak1,
						   sig_type_tristate_pull0,
						   sig_type_tristate_pull1,
						   sig_type_tristate_weak0,
						   sig_type_tristate_weak1,
						   sig_type_open_collector_output_0_pull0,
						   sig_type_open_collector_output_0_pull1,
						   sig_type_open_collector_output_1_pull0,
						   sig_type_open_collector_output_1_pull1,
						   sig_type_open_collector_output_0_weak0,
						   sig_type_open_collector_output_0_weak1,
						   sig_type_open_collector_output_1_weak0,
						   sig_type_open_collector_output_1_weak1,
						   sig_type_open_collector_output_1_keeper,
						   sig_type_open_collector_output_0_keeper,
               sig_type_tristate_keeper,
               sig_type_bidi_keeper,
               sig_type_open_collector_bidi,
               sig_type_open_emitter_bidi,
               sig_type_bidi_output_keeper,
               sig_type_internal_both,
               sig_type_internal_output,
               sig_type_internal_input);

  --property arrays for bscells, individual bscell properties can
  --be accessed by indexing these arrays with bscell numbers
  subtype signals_arr is std_ulogic_vector(number_of_bscells-1 downto 0);
  type types_arr is array(number_of_bscells-1 downto 0) of bscell_sigtype;
  type times_arr is array(number_of_bscells-1 downto 0) of time;


  --file handles for report file generation
  file report_file : text is out "my_core_bscan_tb.rep";



  --------------------------------------
  --------------------------------------
  --
  --   FUNCTION DECLARATIONS
  --
  --------------------------------------
  --------------------------------------

  --function to find the number of digits in an integer
  function number_of_digits(
          constant number: integer) return integer;
          
  --function to convert std_ulogic to char
  function std_ulogic2str(
          constant val:  std_ulogic) return character;

  --function to convert std_ulogic_vector to string
  function std_ulogic_vector2str(
          constant val : std_ulogic_vector) return string;
                   
  --function to convert an int to string
  function int2str(
          constant val : integer) return string;
   

  --------------------------------------
  --------------------------------------
  --
  --   PROCEDURE DECLARATIONS
  --
  --------------------------------------
  --------------------------------------

  --procedure to print out testbench status messages
  procedure message(
          signal error_count : inout integer;
          constant error_state : integer;
          constant mess_txt: string);

  --Check for the IR and BSR shift paths with pause
  procedure check_shift_pause_pattern(
          signal error_count : inout integer;
          constant pat : in std_ulogic_vector;
          signal loc_tdo_s : in std_ulogic);

  --asynchronous TAP reset
  --exit: leaves TAP controller in run-test/idle state
  procedure async_tap_reset(
          signal trst : out std_ulogic;
          signal tms : out std_ulogic);

  --synchronous TAP reset
  --exit: leaves TAP controller in run-test/idle state
  procedure sync_tap_reset(
          signal tms : out std_ulogic);
                    
  --Tap Controller Finite state machine test
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves Tap controller in run-test/idle state
  procedure run_all_tap_states(signal tdi : out std_ulogic;
                               signal tms : out std_ulogic;
                               signal ireg_en : out boolean;
                               signal bsr_en : out boolean;
                               constant opcode : in instruction);

  --Loading a new instruction
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure set_instruction(constant opcode: in instruction;
                            signal tdi : out std_ulogic;
                            signal tms : out std_ulogic);

  --Insert serial data in the shift path between tdi and tdo
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure insert_serial_data_tdi(constant data : in std_ulogic_vector;
                                   signal tdi : out std_ulogic;
                                   signal tms : out std_ulogic;
                                   signal loc_bs_cell_signal : out signals_arr;
                                   signal bs_cell_delay : in times_arr;
                                   signal loc_bs_cell_type : in types_arr);

  --Force bidi inputs to high impedance
  procedure set_bidi_pads_Z(
          signal loc_bs_cell_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr);
          
  --Create a pattern of logic 0 for output type bscells
  procedure create_out_bscells_pattern_0(
          signal loc_bs_cell_type : in types_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          signal pattern : out signals_arr);
          
   --Create a pattern of logic 1 for output type bscells
  procedure create_out_bscells_pattern_1(
          signal loc_bs_cell_type : in types_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;          
          signal pattern : out signals_arr);
          
  --Create a pattern of logic 01... for output type bscells
  procedure create_out_bscells_pattern_01(
          signal loc_bs_cell_type : in types_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;          
          signal pattern : out signals_arr);

  --Create a pattern of logic 10... for output type bscells
  procedure create_out_bscells_pattern_10(
          signal loc_bs_cell_type : in types_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          signal pattern : out signals_arr);
          
  --Create a pattern of logic Z for output type bscells
  procedure create_out_bscells_pattern_z(
          signal loc_bs_cell_type : in types_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          signal pattern : out signals_arr);
          
  --Invert the pattern in output type bscells
  procedure create_out_bscells_pattern_inverted(
          signal loc_bs_cell_type : in types_arr;
          signal sampled_tdo : in signals_arr;
          signal active_value_ctrl_bscells : in signals_arr; 
          signal pattern : out signals_arr);

  --Initialize PI of input type bscells
  procedure initialize_input_bscells(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_init_value : in signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdi_s : out std_ulogic ;
          signal loc_tms_s : out std_ulogic);

   --Load "...0101" pattern in PI of input type bscells
  procedure set_input_bscells_01(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr);
          
  --Load "...1010" pattern in PI of input type bscells
  procedure set_input_bscells_10(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr);
          
  --Load "...0101" pattern in PI of input type bscells
  procedure set_input_bscells_01_sample(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr);

  --Load "...1010" pattern in PI of input type bscells
  procedure set_input_bscells_10_sample(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr);

   --Check if PO of output bscells are 0
  procedure check_output_bscells_0(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr);

  --Check if PO of output bscells are 1
  procedure check_output_bscells_1(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr);

  --Check if PO of output bscells are ...0101
  procedure check_output_bscells_01(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr);
          
  --Check if PO of output bscells are ...1010
  procedure check_output_bscells_10(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr);
          
  --Check if PO of output bscells are Z
  procedure check_output_bscells_Z(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal pull_value_bidi_tristate_cells : signals_arr;          
          signal loc_bs_cell_type : in types_arr);

  --Check if PO of output bscells are same as boundary
  --register values, this implies the mode-mux routes shift
  --register contents to PO
  procedure check_output_bscells_extest(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal sampled_tdo : in signals_arr;
          signal loc_bs_cell_type : in types_arr; 
          signal inv_sampled_tdo : in signals_arr);

  --Check if PO of output bscells are different from boundary
  --register values, this implies the mode-mux routes PI to PO
  procedure check_output_bscells_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal inv_sampled_tdo : in signals_arr;
          signal loc_bs_cell_type : in types_arr;
          signal sampled_tdo : in signals_arr);

  --Check if shift occurs properly during preload
  --entry: assumes the TAP controller is in Run-Test-Idle state
  --exit: leaves the TAP controller in Run-Test-Idle state
  procedure check_preload_shift(
          signal error_count : inout integer;
          signal tdi : out std_ulogic;
          signal tms : out std_ulogic;
          signal loc_tdo_s : in std_ulogic);

  --Find the default instruction loaded in instruction register
  --after TAP reset
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure check_default_instruction(
          signal error_count : inout integer;
          signal tms : out std_ulogic;
          signal tdo : in std_ulogic);
          
  --Check the capture pattern shifted out during shift-IR
  procedure check_capture_pattern(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic);

  --Test the bypass register shift path between tdi and tdo
  procedure check_bypass_reg(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic);
          
  --Check if the boundary register shifts the "...0000" pattern
  --applied to input type bscells thru set_input_bscells_0 task
  procedure check_boundary_reg_with_patterns_0(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic);
          
  --Check if the boundary register shifts the "...1111" pattern
  --applied to input type bscells thru set_input_bscells_1 task
  procedure check_boundary_reg_with_patterns_1(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic);

  --Check if the boundary register shifts the "...0101" pattern
  --applied to input type bscells thru set_input_bscells_01 task
  procedure check_boundary_reg_with_patterns_01(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic);
          
  --Check if the boundary register shifts the "...1010" pattern
  --applied to input type bscells thru set_input_bscells_10 task
  procedure check_boundary_reg_with_patterns_10(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic);

  --Check if the boundary register shifts the "...0101" pattern
  --applied to input type bscells thru set_input_bscells_01_sample task
  procedure check_boundary_reg_with_patterns_01_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic); 

  --Check if the boundary register shifts the "...1010" pattern
  --applied to input type bscells thru set_input_bscells_10_sample task
  procedure check_boundary_reg_with_patterns_10_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic);


  --Check if the overshift pattern specified as input matches
  --value sampled at tdo
  procedure check_overshift_pattern(
          signal error_count : inout integer;
          constant expected_value : in std_ulogic_vector;
          signal loc_tdo_s : in std_ulogic);

  --Check if tdo is 'Z'
  procedure check_tdo_Z(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic);

  --sample the data getting out of tdo
  procedure sample_tdo(
          signal loc_tdo_s : in std_ulogic;
          signal sampled_tdo : out signals_arr);

  --User Instruction (parameters  provided) test tasks
  --entry: assumes TAP controller is in run-test-idle state
  --exit: leaves TAP controller in Update-IR state
  procedure userinst_load_instruction(
          constant opcode : in instruction;
          signal tdi : out std_ulogic;
          signal tms : out std_ulogic);

  --entry: assumes TAP controller is in run-test-idle state
  --exit: leaves TAP controller in update-DR state
  procedure userinst_execute_phase(
          signal error_count   : inout integer;
		      constant test_clock  : in time;
          constant n_cycles    : in integer;
          constant shift_in_v  : in std_ulogic_vector;
          constant shift_out_v : in std_ulogic_vector;
          signal tck_r         : in std_ulogic;  
          signal tdi           : out std_ulogic;
          signal tms           : out std_ulogic;
          signal tdo           : in std_ulogic);
          
end bscan_tb_utils; 






----------------------------------------
----------------------------------------
--
--           PACKAGE BODY
--
----------------------------------------
----------------------------------------
package body bscan_tb_utils is
  
  --function to find the number of digits in an integer
  function number_of_digits(
          constant number: integer) return integer is
  begin
    case number is
      when              0 TO             9 => return  1;
      when             10 TO            99 => return  2;
      when            100 TO           999 => return  3;
      when          1_000 TO          9999 => return  4;
      when         10_000 TO         99999 => return  5;
      when        100_000 TO        999999 => return  6;
      when      1_000_000 TO       9999999 => return  7;
      when     10_000_000 TO      99999999 => return  8;
      when    100_000_000 TO     999999999 => return  9;
      when  1_000_000_000 TO 2_147_483_647 => return 10;
      when others                          => return  0;
    end case;
  end number_of_digits;

  --function to convert std_ulogic to char
  function std_ulogic2str(
          constant val:  std_ulogic) return character is
  begin
    case val is
      when '0' => return '0';
      when '1' => return '1';
      when 'X' => return 'X';
      when 'Z' => return 'Z';
      when 'U' => return 'U';
      when others => return '?';
    end case;
  end std_ulogic2str;

  --function to convert std_ulogic_vector to string
  function std_ulogic_vector2str(
          constant val : std_ulogic_vector) return string is
    alias vec : std_ulogic_vector(1 to val'length) is val;
    variable str : string(vec'range);
  begin
    for i in vec'range loop
      str(i) := std_ulogic2str(vec(i));
    end loop;

    return str;
  end std_ulogic_vector2str;

  --function to convert an int to string
  function int2str(
          constant val : integer) return string is
    variable decade_range : integer;
    variable val_lx : integer;
    variable nr : integer;
    variable str : string(number_of_digits(val) downto 1);
    variable val_hlp : integer;
  begin
    val_hlp := val;
    decade_range := number_of_digits(val);
    val_lx := decade_range;
    while (decade_range /= 0) loop
      nr := 10 ** (decade_range-1);
      case (integer(val_hlp/nr)) is
        when 0 => str(decade_range) := '0';
        when 1 => str(decade_range) := '1';
        when 2 => str(decade_range) := '2';
        when 3 => str(decade_range) := '3';
        when 4 => str(decade_range) := '4';
        when 5 => str(decade_range) := '5';
        when 6 => str(decade_range) := '6';
        when 7 => str(decade_range) := '7';
        when 8 => str(decade_range) := '8';
        when 9 => str(decade_range) := '9';
        when others => str(decade_range) := 'X';
      end case;
      val_hlp := val_hlp - integer(val_hlp/nr) * nr;
      decade_range := decade_range - 1;
    end loop;
    return str;
  end int2str;


procedure message(
        signal error_count : inout integer;
        constant error_state : integer;
        constant mess_txt: string) is
  variable report_line : line;
begin
 msg_note: assert false report mess_txt severity note;
  if (error_state = 1) then
    write(report_line, now, right, 10, ns);
    write(report_line, string'("  ERROR  "));
    error_count <= error_count + 1;
  elsif (error_state = 2) then
    write(report_line, now, right, 10, ns);
    write(report_line, string'("  NOTE   "));
  elsif (error_state = 3) then
    write(report_line,
                string'(" Note : Total number of errors in simulation = "));

      --** modifed to remove need to access testbench signals
      --**write(report_line, (n_error_tdo+n_error_tdi));
      write(report_line, (error_count));

    write(report_line, string'("   "));
    writeline(report_file, report_line);
    write(report_line, string'(" note   "));
  end_msg: assert false

      --** modifed to remove need to access testbench signals
      --** report "  Note : Total number of errors in simulation = " & int2str(n_error_tdo+n_error_tdi)
      report "  Note : Total number of errors in simulation = " & int2str(error_count)
    
    severity Note;
  stop_msg: assert false

    report "------------- Simulation stopped ---------------"
    severity NOTE;
  else
    write(report_line, now, right, 10, ns);
    write(report_line, string'("  OK     "));
  end if;
  if (error_state /= 3) then
    write(report_line, string'(mess_txt));
    writeline(report_file, report_line);
  end if;
end message;


  --asynchronous TAP reset
  --exit: leaves TAP controller in run-test/idle state
  procedure async_tap_reset(
          signal trst : out std_ulogic;
          signal tms : out std_ulogic) is
  begin
    tms <= '1';
    trst <= '0';                    --assert async reset
    wait for tck_period;
    trst <= '1';
    tms <= '0';
    wait for tck_period;
    wait for tck_period;
    wait for tck_period;
  end async_tap_reset;

  --synchronous TAP reset
  --exit: leaves TAP controller in run-test/idle state
  procedure sync_tap_reset(
          signal tms : out std_ulogic) is
  begin
    tms <= '1';
    wait for tck_period;
    wait for (5*tck_period);       --sync reset
    tms <= '0';
    wait for tck_period;
    wait for tck_period;
  end sync_tap_reset;

  --Check for the IR and BSR shift paths with pause
  procedure check_shift_pause_pattern(
          signal error_count : inout integer;
          constant pat : in std_ulogic_vector;
          signal loc_tdo_s : in std_ulogic) is
    variable i2 : integer;
    variable error_occured : boolean := false;
    alias check_pattern : std_ulogic_vector(pat'length-1 downto 0) is pat;
  begin
    for i2 in 0 to check_pattern'length-1 -4 loop
      if (loc_tdo_s /= check_pattern(i2)) then
        error_occured := true;
        message(error_count, 1, "### Shift pattern bit " & int2str(i2) & ": shifted out is " & std_ulogic2str(loc_tdo_s) & ", expected is " & std_ulogic2str(check_pattern(i2)) & " ###");
      end if;

      if (i2 = 1) then
        wait for (9*tck_period);  --account for the pause state
      else
        wait for tck_period;
      end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0,"*** Register shifts properly ***");
    end if;
    wait for tck_period;
  end check_shift_pause_pattern;

  --Instruction register shift test
  --goes into pause-IR state after shifting `ir_length+2 bits
  --waits there for 5 tck cycles, and then continues shifting
  --entry: assumes TAP controller is in Shift-IR state
  --exit: leaves TAP controller in Shift-IR state
  procedure shift_instr_register_with_pause(signal tms : out std_ulogic;
                                            signal tdi : out std_ulogic) is
    variable i1 : integer;
    variable ireg_shift_pattern : std_ulogic_vector(ir_length+7 downto 0) :=
      (others => '0');
  begin
    ireg_shift_pattern(11 downto 0) := instr_bsr_overshift_pattern;
    for i1 in 0 to ir_length loop
      tdi <= ireg_shift_pattern(i1);
      wait for tck_period;
    end loop;

    tdi <= ireg_shift_pattern(ir_length+1);
    tms <= '1';
    wait for tck_period;                --Enter Exit1-IR state
    tms <= '0';
    wait for tck_period;                --Enter Pause-IR state
    wait for (5*tck_period);            --Wait in Pause-IR state
    tms <= '1';
    wait for tck_period;                --Enter Exit2-IR state
    tms <= '0';
    wait for tck_period;                --Enter Shift-IR state

    for i1 in ir_length+2 TO ir_length+7 loop
      tdi <= ireg_shift_pattern(i1);
      if(i1 = ir_length+7) then 
        tms <= '1';
      end if;
      wait for tck_period;
    end loop;
  end shift_instr_register_with_pause;

  --Boundary register shift test
  --goes into pause-DR state after shifting
  --number_of_bscells+2 bits, waits there for 5 tck cycles,
  --and then continues shifting
  --entry: assumes TAP controller is in Shift-DR state
  --exit: leaves TAP controller in Shift-DR state
  procedure shift_boundary_register_with_pause(signal tms : out std_ulogic;
                                               signal tdi : out std_ulogic) is
    variable i : integer;
    variable bsr_shift_pattern1 : std_ulogic_vector(number_of_bscells+7 downto 0) :=
      (others => '0');
  begin
    bsr_shift_pattern1(11 downto 0) := instr_bsr_overshift_pattern;
    for i in 0 TO number_of_bscells loop
      tdi <= bsr_shift_pattern1(i);
      wait for tck_period;
    end loop;

    tdi <= bsr_shift_pattern1(number_of_bscells+1);
    tms <= '1';
    wait for tck_period;                --Enter Exit1-DR state
    tms <= '0';
    wait for tck_period;                --Enter Pause-DR state
    wait for (5*tck_period);            --Wait in Pause-DR state
    tms <= '1';
    wait for tck_period;                --Enter Exit2-DR state
    tms <= '0';
    wait for tck_period;                --Enter Shift-DR state

    for i in number_of_bscells+2 to number_of_bscells+7 loop
      tdi <= bsr_shift_pattern1(i);
      wait for tck_period;
    end loop;
  end shift_boundary_register_with_pause;


  --Tap Controller Finite state machine test
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves Tap controller in run-test/idle state
  procedure run_all_tap_states(signal tdi : out std_ulogic;
                               signal tms : out std_ulogic;
                               signal ireg_en : out boolean;
                               signal bsr_en : out boolean;
                               constant opcode : in instruction) is
    variable i : integer := 0;
  begin
    tms <= '1';
    wait for tck_period;        --Enter Select-DR state
    wait for tck_period;        --Enter Select-IR state
    tms <= '0';
    wait for tck_period;        --Enter Capture-IR state
    wait for tck_period;        --Enter Shift-IR state

    ireg_en <= true;
    shift_instr_register_with_pause(tms, tdi);
    ireg_en <= false;

    tms <= '1';
    wait for tck_period;        --Enter Update-IR state
    wait for tck_period;        --Enter Select-DR state
    wait for tck_period;        --Enter Select-IR state
    tms <= '0';
    wait for tck_period;        --Enter Capture-IR state
    tms <= '1';
    wait for tck_period;        --Enter Exit1-IR state
    tms <= '0';
    wait for tck_period;        --Enter Pause-IR state
    wait for tck_period;        --Wait in Pause-IR state
    tms <= '1';
    wait for tck_period;        --Enter Exit2-IR state
    tms <= '0';
    wait for tck_period;        --Enter Shift-IR state
    for i in 0 to ir_length-2 loop
      tdi <= opcode(i);
      wait for tck_period;
    end loop;
    tdi <= opcode(ir_length-1);
    tms <= '1';
    wait for tck_period;
    tms <= '0';
    wait for tck_period;        --Enter Pause-IR state
    tms <= '1';
    wait for tck_period;        --Enter Exit2-IR state
    wait for tck_period;        --Enter Update_IR state
    tms <= '0';
    wait for tck_period;        --Enter Run-Test/Idle state
    wait for tck_period;        --Wait in Run-Test/Idle state
    tms <= '1';
    wait for tck_period;        --Enter Select-DR state
    tms <= '0';
    wait for tck_period;        --Enter Capture-DR state
    wait for tck_period;        --Enter Shift-DR state

    bsr_en <= true;
    shift_boundary_register_with_pause(tms, tdi);
    bsr_en <= false;

    tms <= '1';
    wait for tck_period;        --Enter Exit1-DR state
    wait for tck_period;        --Enter Update-DR state
    wait for tck_period;        --Enter Select-DR state
    tms <= '0';
    wait for tck_period;        --Enter Capture-DR state
    tms <= '1';
    wait for tck_period;        --Enter Exit1-DR state
    tms <= '0';
    wait for tck_period;        --Enter Pause-DR state
    wait for tck_period;        --Wait in Pause-DR state
    tms <= '1';
    wait for tck_period;        --Enter Exit2-DR state
    tms <= '0';
    wait for tck_period;        --Enter Shift-DR state
    wait for (number_of_bscells*tck_period); --Shift data into DR
    tms <= '1';
    wait for tck_period;        --Enter Exit1-DR state
    tms <= '0';
    wait for tck_period;        --Enter Pause-DR state
    tms <= '1';
    wait for tck_period;        --Enter Exit2-DR state
    wait for tck_period;        --Enter Update-DR state
    tms <= '0';
    wait for tck_period;        --Enter Run-Test/Idle state
    tms <= '1';
    wait for tck_period;        --Enter Select-DR state
    wait for tck_period;        --Enter Select-IR state
    wait for tck_period;        --Enter Test-Logic-Reset state
    tms <= '0';
    wait for tck_period;        --Enter Run-test/idle state
  end run_all_tap_states;

  --Loading a new instruction
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure set_instruction(constant opcode: in instruction;
                            signal tdi : out std_ulogic;
                            signal tms : out std_ulogic) is
    variable i : integer := 0;
  begin
    tms <= '1';
    wait for (2*tck_period);     --Enter Select-IR state
    tms <= '0';
    wait for (2*tck_period);    --Enter Shift-IR state
    for i in 0 to ir_length-2 loop
      tdi <= opcode(i);
      wait for tck_period;
    end loop;
    tdi <= opcode(ir_length-1);
    tms <= '1';
    wait for tck_period;
    tdi <= '0';
    wait for tck_period;        --Enter Update_IR state
    tms <= '0';
    wait for tck_period;        --Enter Run-Test/Idle state
  end set_instruction;

  --Force bidi inputs to high impedance
  procedure set_bidi_pads_Z(
          signal loc_bs_cell_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable delay : time := 1 ns;
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1 or  loc_bs_cell_type(i) = sig_type_open_emitter_bidi or  loc_bs_cell_type(i) = sig_type_open_collector_bidi ) then
        delay := bs_cell_delay(i);
        loc_bs_cell_signal(i) <= 'Z' ;
      end if;
    end loop;
  end set_bidi_pads_Z;

  --Insert serial data in the shift path between tdi and tdo
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure insert_serial_data_tdi(constant data : in std_ulogic_vector;
                                   signal tdi : out std_ulogic;
                                   signal tms : out std_ulogic;
                                   signal loc_bs_cell_signal : out signals_arr;
                                   signal bs_cell_delay : in times_arr;
                                   signal loc_bs_cell_type : in types_arr) is
    alias input_data : std_ulogic_vector(data'length-1 downto 0) is data;
  begin
    tdi <= '0';
    tms <= '1';
    wait for tck_period;        --Enter Select-DR state
    tms <= '0';
    wait for (2*tck_period);    --Enter Shift-DR state
    for i in 0 to (input_data'length-2) loop
      tdi <= input_data(i);
      wait for tck_period;
    end loop;
    tdi <= input_data(input_data'length -1);
    tms <= '1';
    wait for tck_period;        --Enter Exit-DR state
    tms <= '1';
    wait for tck_period;        --Enter Update-DR state
    set_bidi_pads_Z(loc_bs_cell_signal, bs_cell_delay, loc_bs_cell_type); --Force bidi inputs to high impedance
    tms <= '0';
    wait for tck_period;        --Enter Run-Test/Idle state
  end insert_serial_data_tdi;

  --Create a pattern of logic 1 for output type bscells
  procedure create_out_bscells_pattern_1(
          signal loc_bs_cell_type : in types_arr;
          
          --** added new parameters to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;          
                   
          signal pattern : out signals_arr) is
  begin
    pattern <= (others => '0');
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_diff_output_0 or
          loc_bs_cell_type(i) = sig_type_diff_output_1 or
          loc_bs_cell_type(i) = sig_type_diff_output or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1) then
        pattern(i) <= '1';
      elsif (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= active_value_ctrl_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        pattern(i) <= active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_1;

  --Create a pattern of logic 0 for output type bscells
  procedure create_out_bscells_pattern_0(
          signal loc_bs_cell_type : in types_arr;

          --** added 2 new parameters to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          
          signal pattern : out signals_arr) is
  begin
    pattern <= (others => '1');
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_diff_output_0 or
          loc_bs_cell_type(i) = sig_type_diff_output_1 or
          loc_bs_cell_type(i) = sig_type_diff_output or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1) then
        pattern(i) <= '0';
      elsif (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= active_value_ctrl_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        pattern(i) <= active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_0;

  --Create a pattern of logic 01... for output type bscells
  procedure create_out_bscells_pattern_01(
          signal loc_bs_cell_type : in types_arr;

          --** added 2 new parameters to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          
          signal pattern : out signals_arr) is
          
  variable error_occured : boolean := false;
  variable output_value : std_ulogic;
  begin
   output_value := '0';
   pattern <= (others => '0'); 
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or 
          loc_bs_cell_type(i) = sig_type_tristate_pull1) then
        output_value := not output_value;
        pattern(i) <= output_value; 
      elsif (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= active_value_ctrl_bscells(i); 
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        pattern(i) <= active_value_open_collector_bscells(i); 
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        pattern(i) <= active_value_open_collector_bscells(i);
    elsif ((loc_bs_cell_type(i) = sig_type_diff_output_0) or
           (loc_bs_cell_type(i) = sig_type_diff_output_1) or
           (loc_bs_cell_type(i) = sig_type_diff_output)) then
        output_value := not output_value;
        pattern(i) <= output_value; 
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_01;

  --Create a pattern of logic 10... for output type bscells
  procedure create_out_bscells_pattern_10(
          signal loc_bs_cell_type : in types_arr;

          --** added 2 new parameters to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          
          signal pattern : out signals_arr) is
          
  variable error_occured : boolean := false;
  variable output_value : std_ulogic;
  begin
   output_value := '1';
   pattern <= (others => '1'); 
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1) then
        output_value := not output_value;
        pattern(i) <= output_value; 
      elsif (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= active_value_ctrl_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        pattern(i) <= active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        pattern(i) <= active_value_open_collector_bscells(i);
    elsif ((loc_bs_cell_type(i) = sig_type_diff_output_0) or
           (loc_bs_cell_type(i) = sig_type_diff_output_1) or
           (loc_bs_cell_type(i) = sig_type_diff_output)) then
        output_value := not output_value;
        pattern(i) <= output_value; 
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then
        pattern(i) <= active_value_open_collector_bscells(i);
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_10;

  --Create a pattern of logic Z for output type bscells
  procedure create_out_bscells_pattern_z(
          signal loc_bs_cell_type : in types_arr;

          --** added 2 new parameters to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          signal active_value_open_collector_bscells : signals_arr;
          
          signal pattern : out signals_arr) is
  begin
    pattern <= (others => '0');
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= not active_value_ctrl_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        pattern(i) <= not active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        pattern(i) <= not active_value_open_collector_bscells(i);
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
             loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then
        pattern(i) <= not active_value_open_collector_bscells(i);
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_z;

  --Invert the pattern in output type bscells
  procedure create_out_bscells_pattern_inverted(
          signal loc_bs_cell_type : in types_arr;
          signal sampled_tdo : in signals_arr;

          --** added 1 new parameter to avoid reading signals from main testbench
          signal active_value_ctrl_bscells : in signals_arr; 
          
          signal pattern : out signals_arr) is
    variable cval : std_ulogic;
  begin
    pattern <= (others => '0');
    for i in 0 to number_of_bscells-1 loop
      cval := sampled_tdo(i);
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_diff_output_0 or
          loc_bs_cell_type(i) = sig_type_diff_output_1 or
          loc_bs_cell_type(i) = sig_type_diff_output or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1) then
        if (cval = '0' or cval = '1') then
          pattern(i) <= not cval;
        else
          pattern(i) <= '0';
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_control) then
        pattern(i) <= active_value_ctrl_bscells(i);
      else
        pattern(i) <= cval;
      end if;
    end loop;
    wait for 0 ns;
  end create_out_bscells_pattern_inverted;


  --Initialize PI of input type bscells
  procedure initialize_input_bscells(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_init_value : in signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdi_s : out std_ulogic ;
          signal loc_tms_s : out std_ulogic) is
    variable delay : time := 1 ns;
  begin
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if ((loc_bs_cell_type(i) = sig_type_input1) or
          (loc_bs_cell_type(i) = sig_type_diff_input)) then
        loc_bs_cell_signal(i) <= bs_cell_init_value(i) after delay;
        loc_bs_cell_diff_signal(i) <= not bs_cell_init_value(i) after delay;
      elsif (loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
             loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1 or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1 ) then
        loc_bs_cell_signal(i) <= 'Z' after delay;
      end if;
    end loop;
  end initialize_input_bscells;


  --Load "...0000" pattern in PI of input type bscells
  procedure set_input_bscells_0(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '0';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input or
          loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
      end if;
    end loop;
  end set_input_bscells_0;


  --Load "...1111" pattern in PI of input type bscells
  procedure set_input_bscells_1(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '1';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input or
          loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
      end if;
    end loop;
  end set_input_bscells_1;

  
   --Load "...0101" pattern in PI of input type bscells
  procedure set_input_bscells_01(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '1';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input or
          loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input ) then
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
        input_value := not input_value;
        elsif(loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then 
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
      end if;
    end loop;
  end set_input_bscells_01;

  --Load "...1010" pattern in PI of input type bscells
  procedure set_input_bscells_10(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '0';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input or 
          loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input) then 
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
        input_value := not input_value;
        elsif(loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then 
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
      end if;
    end loop;
  end set_input_bscells_10;

  --Load "...0101" pattern in PI of input type bscells
  procedure set_input_bscells_01_sample(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '1';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input) then 
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
        input_value := not input_value;
      elsif (loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
             loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then
        loc_bs_cell_signal(i) <= 'Z' after delay;
      end if;
    end loop;
  end set_input_bscells_01_sample;

  --Load "...1010" pattern in PI of input type bscells
  procedure set_input_bscells_10_sample(
          signal loc_bs_cell_signal : out signals_arr;
          signal loc_bs_cell_diff_signal : out signals_arr;
          signal bs_cell_delay : in times_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable input_value : std_ulogic;
    variable delay : time := 1 ns;
  begin
    input_value := '0';
    for i in 0 to number_of_bscells-1 loop
      delay := bs_cell_delay(i);
      if (loc_bs_cell_type(i) = sig_type_input1 or loc_bs_cell_type(i) = sig_type_diff_input) then
        loc_bs_cell_signal(i) <= input_value after delay;
        loc_bs_cell_diff_signal(i) <= not input_value after delay;
        input_value := not input_value;
      elsif (loc_bs_cell_type(i) = sig_type_bidi or loc_bs_cell_type(i) = sig_type_bidi_input or
             loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak0 or loc_bs_cell_type(i) = sig_type_bidi_weak1) then
        loc_bs_cell_signal(i) <= 'Z' after delay;
      end if;
    end loop;
  end set_input_bscells_10_sample;

 
   --Check if PO of output bscells are 0
  procedure check_output_bscells_0(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) then
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_0 or
          loc_bs_cell_type(i) = sig_type_diff_output_1 or
          loc_bs_cell_type(i) = sig_type_diff_output) then 
        if ((loc_bs_cell_signal(i) /= '0') or (loc_bs_cell_diff_signal(i) /= '1')) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0' and '1'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1) then 
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then 
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to '0' ");
    end if;
  end check_output_bscells_0;


  --Check if PO of output bscells are 1
  procedure check_output_bscells_1(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr) is
  variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_open_collector_output_1 or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) then
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal '1'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_0 or
          loc_bs_cell_type(i) = sig_type_diff_output_1 or
          loc_bs_cell_type(i) = sig_type_diff_output) then 
        if ((loc_bs_cell_signal(i) /= '1') or (loc_bs_cell_diff_signal(i) /= '0')) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1' and '0'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1) then 
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then 
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to '1'");
    end if;
  end check_output_bscells_1;


  --Check if PO of output bscells are ...0101
  procedure check_output_bscells_01(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr) is
  variable error_occured : boolean := false;
  variable output_value : std_ulogic;
  begin
   output_value := '0';
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) then
        output_value := not output_value;
        if (loc_bs_cell_signal(i) /= output_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to " & std_ulogic2str(output_value));
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
    elsif ((loc_bs_cell_type(i) = sig_type_diff_output_0) or
           (loc_bs_cell_type(i) = sig_type_diff_output_1)or
           (loc_bs_cell_type(i) = sig_type_diff_output)) then
        output_value := not output_value;
        if (loc_bs_cell_signal(i) /= output_value and loc_bs_cell_diff_signal(i) /=  not output_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to " & std_ulogic2str(output_value));
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1) then 
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then 
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to '...0101'");
    end if;
  end check_output_bscells_01;



  --Check if PO of output bscells are ...1010
  procedure check_output_bscells_10(
          signal error_count: inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr) is
  variable error_occured : boolean := false;
  variable output_value : std_ulogic;
  begin
   output_value := '1';
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) then
        output_value := not output_value;
        if (loc_bs_cell_signal(i) /= output_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to " & std_ulogic2str(output_value));
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0) then
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1) then
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
    elsif ((loc_bs_cell_type(i) = sig_type_diff_output_0) or
           (loc_bs_cell_type(i) = sig_type_diff_output_1) or
           (loc_bs_cell_type(i) = sig_type_diff_output)) then
        output_value := not output_value;
        if (loc_bs_cell_signal(i) /= output_value and loc_bs_cell_diff_signal(i) /=  not output_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to " & std_ulogic2str(output_value));
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1) then 
        if (loc_bs_cell_signal(i) /= '0') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '0'");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then 
        if (loc_bs_cell_signal(i) /= '1') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to '1'");
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to '...1010'");
    end if;
  end check_output_bscells_10;



  --Check if PO of output bscells are Z
  procedure check_output_bscells_Z(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          
          --** Added parameter so that procedure could be placed in package
          signal pull_value_bidi_tristate_cells : signals_arr;          
  
          signal loc_bs_cell_type : in types_arr) is
          
    variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_open_collector_output_0 or
          loc_bs_cell_type(i) = sig_type_open_collector_output_1 or
          loc_bs_cell_type(i) = sig_type_tristate ) and
          (loc_bs_cell_signal(i) /= 'Z') then
        error_occured := true;
        message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal 'Z'");
      elsif(loc_bs_cell_type(i) = sig_type_bidi_pull0 or 
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0) and 
          (loc_bs_cell_signal(i) /= '0' and loc_bs_cell_signal(i) /= 'L') then
           error_occured := true;
           message(error_count, 1, "### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to the pull value ");
      elsif(loc_bs_cell_type(i) = sig_type_bidi_pull1 or 
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) and 
          (loc_bs_cell_signal(i) /= '1' and loc_bs_cell_signal(i) /= 'H') then
           error_occured := true;
           message(error_count, 1, "### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to the pull value ");
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull0 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak0 or
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak0 or
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull0) then 
        if (loc_bs_cell_signal(i) /= pull_value_bidi_tristate_cells(i) and 
            loc_bs_cell_signal(i) /= 'L') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to pull value ");
        end if;
     elsif (loc_bs_cell_type(i) = sig_type_open_collector_output_0_pull1 or 
            loc_bs_cell_type(i) = sig_type_open_collector_output_0_weak1 or
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_weak1 or
            loc_bs_cell_type(i) = sig_type_open_collector_output_1_pull1) then 
        if (loc_bs_cell_signal(i) /= pull_value_bidi_tristate_cells(i) and 
            loc_bs_cell_signal(i) /= 'H') then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to pull value ");
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to 'Z'");
    end if;
  end check_output_bscells_Z;


  --Check if PO of output bscells are Z
  procedure check_output_bscells_highz_Z(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal loc_bs_cell_type : in types_arr) is
    variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_open_collector_output_0 or
          loc_bs_cell_type(i) = sig_type_open_collector_output_1 or
          loc_bs_cell_type(i) = sig_type_tristate ) and
          (loc_bs_cell_signal(i) /= 'Z') then
        error_occured := true;
        message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal 'Z'");
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_0) then
        if ( (loc_bs_cell_signal(i) /= '0') or (loc_bs_cell_diff_signal(i) = '1')) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal '0'");
        end if;
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_1) then
        if ((loc_bs_cell_signal(i) /= '1') and (loc_bs_cell_diff_signal(i) = '0')) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal '1'");
        end if;
      elsif(loc_bs_cell_type(i) = sig_type_bidi_pull0 or 
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0) and 
          (loc_bs_cell_signal(i) /= '0' and loc_bs_cell_signal(i) /= 'L') then
           error_occured := true;
           message(error_count, 1, "### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to the pull value ");
      elsif(loc_bs_cell_type(i) = sig_type_bidi_pull1 or 
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or 
          loc_bs_cell_type(i) = sig_type_tristate_pull1) and 
          (loc_bs_cell_signal(i) /= '1' and loc_bs_cell_signal(i) /= 'H') then
           error_occured := true;
           message(error_count, 1, "### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal to the pull value ");
      end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All Outputs are set to 'Z'");
    end if;
  end check_output_bscells_highz_Z;


  --Check if PO of output bscells are same as boundary
  --register values, this implies the mode-mux routes shift
  --register contents to PO
  procedure check_output_bscells_extest(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal sampled_tdo : in signals_arr;
          signal loc_bs_cell_type : in types_arr; 
          signal inv_sampled_tdo : in signals_arr) is
    variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if ((loc_bs_cell_type(i) = sig_type_output1 or
           loc_bs_cell_type(i) = sig_type_tristate or
           loc_bs_cell_type(i) = sig_type_bidi or
           loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) and
          (loc_bs_cell_signal(i) = sampled_tdo(i))) then
        error_occured := true;
        message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal the update register value " & std_ulogic2str(sampled_tdo(i)));
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_0 or
                loc_bs_cell_type(i) = sig_type_diff_output_1 or
                loc_bs_cell_type(i) = sig_type_diff_output) then   
        if (loc_bs_cell_signal(i) = sampled_tdo(i) or loc_bs_cell_diff_signal(i) /= not inv_sampled_tdo(i)) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal the update register value " & std_ulogic2str(sampled_tdo(i)));
        end if;
      end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0,"*** Data check ok *** All outputs are set to the update register values ");
    end if;
  end check_output_bscells_extest;


  --Check if PO of output bscells are different from boundary
  --register values, this implies the mode-mux routes PI to PO
  procedure check_output_bscells_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_signal : in signals_arr;
          signal loc_bs_cell_diff_signal : in signals_arr;
          signal inv_sampled_tdo : in signals_arr;
          signal loc_bs_cell_type : in types_arr;
          signal sampled_tdo : in signals_arr) is
    variable error_occured : boolean := false;
  begin
    for i in 0 to number_of_bscells-1 loop
      if ((loc_bs_cell_type(i) = sig_type_output1 or
          loc_bs_cell_type(i) = sig_type_tristate or
          loc_bs_cell_type(i) = sig_type_bidi or 
          loc_bs_cell_type(i) = sig_type_bidi_output or
          loc_bs_cell_type(i) = sig_type_bidi_pull0 or
          loc_bs_cell_type(i) = sig_type_bidi_pull1 or
          loc_bs_cell_type(i) = sig_type_bidi_weak0 or
          loc_bs_cell_type(i) = sig_type_bidi_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_weak0 or
          loc_bs_cell_type(i) = sig_type_tristate_weak1 or
          loc_bs_cell_type(i) = sig_type_tristate_pull0 or
          loc_bs_cell_type(i) = sig_type_tristate_pull1) and  
         (loc_bs_cell_signal(i) = inv_sampled_tdo(i))) then
          error_occured := true;
          message(error_count, 1, "### ERROR ### Output signal of BS cell number " & int2str(i) & " is equal the update register value " & std_ulogic2str(inv_sampled_tdo(i)));
      elsif (loc_bs_cell_type(i) = sig_type_diff_output_0 or
                loc_bs_cell_type(i) = sig_type_diff_output_1 or
                loc_bs_cell_type(i) = sig_type_diff_output) then   
        if (loc_bs_cell_signal(i) = inv_sampled_tdo(i) or loc_bs_cell_diff_signal(i) /= not sampled_tdo(i)) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Output signal of BS cell number " & int2str(i) & " is not equal the update register value " & std_ulogic2str(inv_sampled_tdo(i)));
        end if;
     end if;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** All outputs are different to the update register values");
    end if;
 end check_output_bscells_sample;


  --Check if shift occurs properly during preload
  --entry: assumes the TAP controller is in Run-Test-Idle state
  --exit: leaves the TAP controller in Run-Test-Idle state
  procedure check_preload_shift(
          signal error_count : inout integer;
          signal tdi : out std_ulogic;
          signal tms : out std_ulogic;
          signal loc_tdo_s : in std_ulogic) is
    variable i: integer := 0;
    variable error_occured : boolean := false;
    variable local_bsr_shift_pattern : std_ulogic_vector(number_of_bscells+7 downto 0) :=
      (others => '0');
  begin
    local_bsr_shift_pattern(11 downto 0) := instr_bsr_overshift_pattern;
    tms <= '1';
    wait for tck_period;                --Enter Select-DR
    tms <= '0';
    wait for tck_period;                --Enter Capture-DR
    tms <= '0';
    wait for tck_period;                --Enter Shift-DR
    for i in 0 to number_of_bscells+6 loop
      if(i >= number_of_bscells) then
        if (loc_tdo_s /= local_bsr_shift_pattern(i-number_of_bscells)) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### Shift in preload phase for overshift pos " & int2str(i-number_of_bscells) & " is " & std_ulogic2str(loc_tdo_s) & " is not equal to " & std_ulogic2str(local_bsr_shift_pattern(i-number_of_bscells)));
        end if;
      end if;
      tdi <= local_bsr_shift_pattern(i);
      wait for tck_period;
    end loop;
    if (loc_tdo_s /= local_bsr_shift_pattern(7)) then
      error_occured := true;
          message(error_count, 1,"### ERROR ### Shift in preload phase for overshift pos " & int2str(7) & " is " & std_ulogic2str(loc_tdo_s) & " is not equal to " & std_ulogic2str(local_bsr_shift_pattern(7)));
    end if;
    tms <= '1';
    wait for tck_period;                --Enter Exit1-DR
    tms <= '1';
    wait for tck_period;                --Enter Update-DR
    tms <= '0';
    wait for tck_period;                --Enter Exit1-DR

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** Boundary register shifts properly in preload mode");
    end if;
  end check_preload_shift;

  
  --Find the default instruction loaded in instruction register
  --after TAP reset
  --entry: assumes TAP controller is in run-test/idle state
  --exit: leaves TAP controller in run-test/idle state
  procedure check_default_instruction(
          signal error_count : inout integer;
          signal tms : out std_ulogic;
          signal tdo : in std_ulogic) is
    variable error_occured : boolean := false;
  begin
    tms <= '1';
    wait for tck_period;        --enter select-DR
    tms <= '0';
    wait for tck_period;        --enter capture-DR
    wait for tck_period;        --Enter shift-DR

    --check if tdo has a '0' or '1' indicating bypass or idcode
    if (tdo = '0') then
      message(error_count, 0, "*** Active instruction is BYPASS ***");
    elsif (tdo = '1') then
      message(error_count, 0, "*** Active instruction is IDCODE ***");
    else
      message(error_count, 1, "### Active instruction is neither BYPASS nor IDCODE ***");
    end if;

    tms <= '1';
    wait for tck_period;        --Enter exit1-DR
    wait for tck_period;        --Enter update-DR
    tms <= '0';
    wait for tck_period;        --Enter run-test/idle
  end check_default_instruction;


  --Check the capture pattern shifted out during shift-IR
  procedure check_capture_pattern(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
  begin
    for i in 0 to ir_length-1 loop
      if (loc_tdo_s /= capture_pattern(i)) then
        error_occured := true;
        message(error_count, 1, "### Capture Pattern bit " & int2str(i) & ": shifted out is " & std_ulogic2str(loc_tdo_s) & ", expected is " & std_ulogic2str(capture_pattern(i)) & " ###");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
        message(error_count, 0, "*** Capture pattern shifted out properly ***");
    end if;
  end check_capture_pattern;


  --Test the bypass register shift path between tdi and tdo
  procedure check_bypass_reg(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
  begin
    if (loc_tdo_s  /= '0') then
      message(error_count, 1, "### ERROR ### first bypass register output bit is '" & std_ulogic2str(loc_tdo_s) & "', the specified value is '0'");
    end if;
    wait for tck_period;

    --check for the overshifted pattern
    for i in 0 to 10 loop
      if (bypass_shift_pattern(i) /= loc_tdo_s) then
        error_occured := true;
        message(error_count, 1,"### ERROR ### bypass register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the inserted value was '" & std_ulogic2str(bypass_shift_pattern(i)) & "'");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** bypass register output equals the inserted value '" & std_ulogic_vector2str(bypass_shift_pattern));
    end if;
  end check_bypass_reg;


  --Check if the boundary register shifts the "...0000" pattern
  --applied to input type bscells thru set_input_bscells_0 task
  procedure check_boundary_reg_with_patterns_0(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '0';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_weak0)  and loc_tdo_s /= '0') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '0'");
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull1  or loc_bs_cell_type(i) = sig_type_bidi_weak1) and loc_tdo_s /= '1') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '1'");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** all sampled input1 values '...0000' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_0;


  --Check if the boundary register shifts the "...1111" pattern
  --applied to input type bscells thru set_input_bscells_1 task
  procedure check_boundary_reg_with_patterns_1(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '1';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_weak0) and loc_tdo_s /= '0') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '0'");
         elsif((loc_bs_cell_type(i) = sig_type_bidi_pull1 or loc_bs_cell_type(i) = sig_type_bidi_weak1) and loc_tdo_s /= '1') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '1'");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** all sampled input values '...1111' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_1;


  --Check if the boundary register shifts the "...0101" pattern
  --applied to input type bscells thru set_input_bscells_01 task
  procedure check_boundary_reg_with_patterns_01(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '1';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        sample_value := not sample_value;
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_weak0) and loc_tdo_s /= '0') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '0'");
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull1 or loc_bs_cell_type(i) = sig_type_bidi_weak1) and loc_tdo_s /= '1') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '1'");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** all sampled input1 values '...0101' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_01;


  --Check if the boundary register shifts the "...1010" pattern
  --applied to input type bscells thru set_input_bscells_10 task
  procedure check_boundary_reg_with_patterns_10(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '0';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_bidi or
          loc_bs_cell_type(i) = sig_type_bidi_input or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        sample_value := not sample_value;
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull0 or loc_bs_cell_type(i) = sig_type_bidi_weak0) and loc_tdo_s /= '0') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '0'");
        elsif((loc_bs_cell_type(i) = sig_type_bidi_pull1 or loc_bs_cell_type(i) = sig_type_bidi_weak1)  and loc_tdo_s /= '1') then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '1'");
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
        message(error_count, 0, "*** Data check ok *** all sampled input1 values '...1010' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_10;


  --Check if the boundary register shifts the "...0101" pattern
  --applied to input type bscells thru set_input_bscells_01_sample task
  procedure check_boundary_reg_with_patterns_01_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '1';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1, "### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        sample_value := not sample_value;
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
      message(error_count, 0, "*** Data check ok *** all sampled input1 values '...0101' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_01_sample;


  --Check if the boundary register shifts the "...1010" pattern
  --applied to input type bscells thru set_input_bscells_10_sample task
  procedure check_boundary_reg_with_patterns_10_sample(
          signal error_count : inout integer;
          signal loc_bs_cell_type : in types_arr;
          signal loc_tdo_s : in std_ulogic) is
    variable error_occured : boolean := false;
    variable sample_value : std_ulogic := '0';
  begin
    for i in 0 to number_of_bscells-1 loop
      if (loc_bs_cell_type(i) = sig_type_input1 or
          loc_bs_cell_type(i) = sig_type_diff_input) then
        if (loc_tdo_s /= sample_value) then
          error_occured := true;
          message(error_count, 1,"### ERROR ### boundary scan register output bit number " & int2str(i) & " is '" & std_ulogic2str(loc_tdo_s) & "', the sampled value was '" & std_ulogic2str(sample_value) & "' ");
        end if;
        sample_value := not sample_value;
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
        message(error_count, 0, "*** Data check ok *** all sampled input1 values '...1010' were shifted out through the boundary scan register");
    end if;
  end check_boundary_reg_with_patterns_10_sample;


  --Check if the overshift pattern specified as input matches
  --value sampled at tdo
  procedure check_overshift_pattern(
          signal error_count : inout integer;
          constant expected_value : in std_ulogic_vector;
          signal loc_tdo_s : in std_ulogic) is
    alias exp_data : std_ulogic_vector(expected_value'length-1 downto 0) is expected_value;
    variable error_occured : boolean := false;
  begin
    for i in 0 to exp_data'length-1 loop
      if (loc_tdo_s /= exp_data(i)) then
        error_occured := true;
        message(error_count, 1, "### ERROR ### Bit no. " & int2str(i) & ": expected = " & std_ulogic2str(exp_data(i)) & ", received = " & std_ulogic2str(loc_tdo_s));
      end if;
      wait for tck_period;
    end loop;

    if (not error_occured) then
        message(error_count, 0,"*** Data check OK *** shift successful");
    end if;
  end check_overshift_pattern;


  --Check if tdo is 'Z'
  procedure check_tdo_Z(
          signal error_count : inout integer;
          signal loc_tdo_s : in std_ulogic) is
  begin
    if (loc_tdo_s = 'Z') then
      message(error_count, 0, "*** TDO is set to 'Z' ***");
    else
      message(error_count, 1, "### TDO is " & std_ulogic2str(loc_tdo_s) & ", expected is 'Z' ###");
    end if;
  end check_tdo_Z;


  --sample the data getting out of tdo
  procedure sample_tdo(
          signal loc_tdo_s : in std_ulogic;
          signal sampled_tdo : out signals_arr) is
  begin
    for i in 0 to number_of_bscells-1 loop
      sampled_tdo(i) <= loc_tdo_s;
      wait for tck_period;
    end loop;
  end sample_tdo;

  --User Instruction (parameters  provided) test tasks
  --entry: assumes TAP controller is in run-test-idle state
  --exit: leaves TAP controller in Update-IR state
  procedure userinst_load_instruction(
          constant opcode : in instruction;
          signal tdi : out std_ulogic;
          signal tms : out std_ulogic) is
  begin
    tms <= '1';
    wait for tck_period;      --enter select-DR-scan
    wait for tck_period;      --enter select-IR-scan
    tms <= '0';
    wait for tck_period;      --enter capture-IR
    wait for tck_period;      --enter shift-IR

    --shift-in opcode
    for i in 0 to ir_length-2 loop
      tdi <= opcode(i);
      wait for tck_period;
    end loop;
    tdi <= opcode(ir_length-1);
    tms <= '1';
    wait for tck_period;      --shift last bit and enter exit1-IR
    wait for tck_period;      --enter update-IR
  end userinst_load_instruction;


  --entry: assumes TAP controller is in run-test-idle state
  --exit: leaves TAP controller in update-DR state
  procedure userinst_execute_phase(
          signal error_count : inout integer;
		      constant test_clock: in time;
          constant n_cycles : in integer;
          constant shift_in_v : in std_ulogic_vector;
          constant shift_out_v : in std_ulogic_vector;
          --** pass clock into procedure so that procedure can be put into package
          signal tck_r : in std_ulogic;
          signal tdi : out std_ulogic;
          signal tms : out std_ulogic;
          signal tdo : in std_ulogic) is
          variable error_occured : boolean := false;
          constant n_shift_in : integer := shift_in_v'length;
          constant n_shift_out : integer := shift_out_v'length;
          alias shift_in : std_ulogic_vector(n_shift_in-1 downto 0) is shift_in_v;
          alias shift_out : std_ulogic_vector(n_shift_out-1 downto 0) is shift_out_v;
          variable real_out : std_ulogic_vector(n_shift_out-1 downto 0);
  begin
    message(error_count, 0, "*** Executing phase with shift-in " &
            std_ulogic_vector2str(shift_in) & " and shift-out " &
            std_ulogic_vector2str(shift_out));

    --shift pre-test shift-in pattern into register
    if (n_shift_in > 0) then
      tms <= '1';
      wait for tck_period;        --enter select-DR-scan
      tms <= '0';
      wait for tck_period;        --enter capture-DR
      wait for tck_period;        --enter shift-DR
      for i in 0 to n_shift_in-2 loop
        tdi <= shift_in(i);
        wait for tck_period;
      end loop;
      tdi <= shift_in(n_shift_in-1);
      tms <= '1';
      wait for tck_period;        --shift last bit and enter exit1-DR
      wait for tck_period;        --enter update-DR
    end if;

    tms <= '0';
    wait for tck_period;        --enter run-test-idle

    wait for (n_cycles * test_clock);      --wait for test to complete

    --Clock synchronization logic 
    if(tck_period /= test_clock) then
      if(tck_r='1') then
        wait until tck_r'event and tck_r= '0';
        wait for (tck_period/4);
      else
        wait until tck_r'event and tck_r= '1';
        wait until tck_r'event and tck_r= '0';
        wait for (tck_period/4);
      end if;
    end if;

    --shift-out pattern
    if (n_shift_out > 0) then
      tms <= '1';
      wait for tck_period;      --enter select-DR-scan
      tms <= '0';
      wait for tck_period;      --enter capture-DR-scan
      wait for tck_period;      --enter shift-DR

      for i in 0 to n_shift_out-2 loop
        real_out(i) := tdo;
        tdi <= shift_in(i);
        wait for tck_period;
      end loop;
      real_out(n_shift_out-1) := tdo;
      tdi <= shift_in(n_shift_out-1);
      tms <= '1';
      wait for tck_period;      --enter exit1-DR

      for i in 0 to n_shift_out-1 loop
        if (shift_out(i) /= 'X') then
          if (shift_out(i) /= real_out(i)) then
            error_occured := true;
            message(error_count, 1, "### ERROR ### Bit " & int2str(i) & ": shifted out is " & std_ulogic2str(real_out(i)) & ", expected is " & std_ulogic2str(shift_out(i)));
          end if;
        end if;
      end loop;

      if (not error_occured) then
        message(error_count, 0, "*** Data check ok *** Shifted out pattern matches expected pattern");
      end if;

      wait for tck_period;      --enter update-DR
    end if;
  end userinst_execute_phase;

end bscan_tb_utils;  