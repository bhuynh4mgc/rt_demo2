library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

library std;
use std.textio.all;

library work;
use work.all;

entity ip_core_bscan_tb is
end ip_core_bscan_tb;

architecture bsda_tb_arch3 of ip_core_bscan_tb is

  --define clock period
  constant clk_period : time := 100 ns;

  --signals for controlling TAP pins
  signal tck_s  : std_ulogic;
  signal tms_s  : std_ulogic;
  signal tdi_s  : std_ulogic;
  signal trst_s : std_ulogic;
  signal tdo_s  : std_ulogic;

  --signals for the counter
  signal clk_s   : std_ulogic;
  signal rst_s   : std_ulogic;
  signal cen_s   : std_ulogic;
  signal ld_n_s  : std_ulogic;
  signal up_dn_s : std_ulogic;
  signal din_s   : std_ulogic_vector(7 DOWNTO 0);
  signal dout_s  : std_ulogic_vector(7 DOWNTO 0);
  signal zero_s  : std_ulogic;
  signal tc_s    : std_ulogic;
  
  --signals for the RAM
  signal A_0_s   : std_ulogic_vector(4 downto 0);
  signal DI_0_s  : std_ulogic_vector(7 downto 0);
  signal WEN_0_s : std_ulogic;
  signal DO_0_s  : std_ulogic_vector(7 downto 0);

  signal end_of_test_s : boolean := FALSE;
  
  signal expected_count_val_s : unsigned(7 DOWNTO 0);

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

  -- tie JTAG input signals 
  tck_s   <= '0';
  tms_s   <= '0';
  tdi_s   <= '0';
  trst_s  <= '1';

  --instantiate DUT
  chip : ip_core_bscan
    port map(A_0   => A_0_s,
             cen   => cen_s,
             clk   => clk_s,
             DI_0  => DI_0_s,
             din   => din_s,
             DO_0  => DO_0_s,
             dout  => dout_s,
             ld_n  => ld_n_s,
             rst_n => rst_s,
             tc    => tc_s,
             up_dn => up_dn_s,
             WEN_0 => WEN_0_s,
             zero  => zero_s,
             tdi   => tdi_s,
             tms   => tms_s,
             tck   => tck_s,
             tdo   => tdo_s,
             trst  => trst_s);


  --counter clock generator
  cntclk_gen: process
  begin
    clk_s  <= '0';
    while NOT end_of_test_s LOOP
       clk_s <= '0';
       wait for (clk_period/2);
       clk_s <= '1';
       wait for (clk_period/2);
    end loop;

    wait for (clk_period/2);
    clk_s <= '0';
    wait for (clk_period * 2);
    wait;
  end process;



  --sequence of tests
  cnt_ram_stim: process
    
  begin
  
    -- check to see if error is reported here
    -- <<< REQ_TP_3.13
	REPORT "<<< REQ_TP_3.13 FAIL" SEVERITY NOTE;
 
  
    --14. Testing the 8 bit Up/Down Counter

    ------------------------------
    -- Hold the RAM signals tied
    -----------------------------
    A_0_s   <= "00000";
    DI_0_s  <= "00000000";
    WEN_0_s <= '0';


    -----------------------
    -- Reset The Counter
    -----------------------
    rst_s   <= '0';    --** rst_n
    cen_s   <= '0';    --** cen
    din_s   <= (OTHERS => '0'); --** din
    up_dn_s <= '1';    --** up_dn
    ld_n_s  <= '1';   --** ld_n
    
    wait for 2 * clk_period;
    rst_s <= '1'; --** rst_n
     
    ----------------------------------
    -- Counter should be held static
    -- zero O/P should be active
    ----------------------------------
    wait for 2 * clk_period;
 

    ----------------------------------
    -- Counter count up
    -- & wrap around.
    -- When count = "11111111", O/P tc
    -- should be active
    ----------------------------------
    cen_s   <= '1';  --** cen
    wait for 260 * clk_period;

    ----------------------------------
    -- Counter load 
    -- Load Counter with max value 
    -- When count = "11111111", O/P tc
    -- should be active
    ----------------------------------
    din_s <= "11111100"; --** din
    wait for clk_period;
    ld_n_s <= '0'; --** ld_n
    wait for clk_period;
    ld_n_s <= '1'; --** ld_n
    wait for 20 * clk_period;
    
    ----------------------------------
    -- Counter count down
    -- & wrap around.
    -- When count = "11111111", O/P tc <= '1'
    -- When count = "00000000", O/P zero <= '1'
    ----------------------------------
    up_dn_s <= '0';  --** up_dn
    wait for 30 * clk_period;



    --15. Testing the RAM

    -----------------------
    -- Reset The Counter
    -----------------------
    rst_s   <= '0';    --** rst_n
    cen_s   <= '0';    --** cen
    din_s   <= (OTHERS => '0'); --** din
    up_dn_s <= '0';    --** up_dn
    ld_n_s  <= '1';   --** ld_n

    ------------------------------
    -- Write Address 0
    -----------------------------
    A_0_s   <= "00000";
    DI_0_s  <= "11000001";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Write Address 1
    -----------------------------
    A_0_s   <= "00001";
    DI_0_s  <= "11111111";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Write Address 2
    -----------------------------
    A_0_s   <= "00010";
    DI_0_s  <= "00000001";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Write Address 4
    -----------------------------
    A_0_s   <= "00100";
    DI_0_s  <= "01111100";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Write Address 8
    -----------------------------
    A_0_s   <= "01000";
    DI_0_s  <= "01010100";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Write Address 16
    -----------------------------
    A_0_s   <= "10000";
    DI_0_s  <= "01011111";
    WEN_0_s <= '1';


    wait for clk_period/2;
    WEN_0_s <= '0';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Read Address 16
    -----------------------------
    A_0_s   <= "10000";
    DI_0_s  <= "00000000";
    WEN_0_s <= '1';

    wait for clk_period/2;
    WEN_0_s <= '1';
    wait for clk_period/2;
    WEN_0_s <= '1';
    
    ------------------------------
    -- Read Address 8
    -----------------------------
    A_0_s   <= "01000";
    DI_0_s  <= "00000000";
    WEN_0_s <= '1';


    end_of_test_s <= TRUE;
    
    wait for clk_period;    

    wait;
  end process;




  --sequence of tests
  Check_cnt_ram_response: process
   variable expected_count_val_v : UNSIGNED(7 DOWNTO 0); 
   variable cycle_count_v : integer;
   variable tests_failed : integer;
  begin
    --14. Testing the 8 bit Up/Down Counter
    -- <<< REQ_TP_3.14
    
	tests_failed := 0;
	
    --** add delay by 3/4 of a clock period to sample after +ve edge of clock
    wait for (3 * clk_period)/4;

    ------------------------------
    -- Counter Output Unknown here
    -----------------------------

    -----------------------
    -- Reset The Counter
    -----------------------
    --** rst_s   <= '0';    --** rst_n
    expected_count_val_v := (OTHERS => '0');
    Test_14_1_Assert : assert (UNSIGNED(dout_s) = expected_count_val_v) report "## Failed 14.1 Counter asynch reset test ###" SEVERITY ERROR;
    if(UNSIGNED(dout_s) /= expected_count_val_v) then
        tests_failed := tests_failed + 1;
    end if;
	
    wait for 2 * clk_period;
    --** rst_s <= '1'; --** rst_n
     
    ----------------------------------
    -- Counter should be held static
    -- zero O/P should be active
    ----------------------------------

    Test_14_2_Assert : assert (UNSIGNED(dout_s) = expected_count_val_v) report "## Failed 14.2 Counter hold test ###" SEVERITY ERROR;
    if(UNSIGNED(dout_s) /= expected_count_val_v) then
        tests_failed := tests_failed + 1;
    end if;
	
	
    wait for 2 * clk_period;
 

    ----------------------------------
    -- Counter count up
    -- & wrap around.
    -- When count = "11111111", O/P tc
    -- should be active
    ----------------------------------
    --** cen_s   <= '1';  --** cen
    cycle_count_v := 0;
    WHILE (cycle_count_v < 260) LOOP
       expected_count_val_v := expected_count_val_v + "01";
       Test_14_3_Assert : assert (UNSIGNED(dout_s) = expected_count_val_v) report "## Failed 14.3 Counter increment test ###" SEVERITY ERROR; 
       if(UNSIGNED(dout_s) /= expected_count_val_v) then
          tests_failed := tests_failed + 1;
       end if;
       wait for clk_period;
       cycle_count_v := cycle_count_v + 1;
    END LOOP;
    -- ** wait for 260 * clk_period;

    ----------------------------------
    -- Counter load 
    -- Load Counter with max value 
    -- When count = "11111111", O/P tc
    -- should be active
    ----------------------------------
    --** din_s <= "11111100"; --** din
    expected_count_val_v := "11111100";
    wait for clk_period;
    --** ld_n_s <= '0'; --** ld_n
    wait for clk_period;
    --** ld_n_s <= '1'; --** ld_n

    cycle_count_v := 0;
    WHILE (cycle_count_v < 20) LOOP
       expected_count_val_v := expected_count_val_v + "01";
       Test_14_4_Assert : assert (UNSIGNED(dout_s) = expected_count_val_v) report "## Failed 14.4 Counter load test ###" SEVERITY ERROR; 
       if(UNSIGNED(dout_s) /= expected_count_val_v) then
          tests_failed := tests_failed + 1;
       end if;
       wait for clk_period;
       cycle_count_v := cycle_count_v + 1;
    END LOOP;
    --** wait for 20 * clk_period;
    
    ----------------------------------
    -- Counter count down
    -- & wrap around.
    -- When count = "11111111", O/P tc <= '1'
    -- When count = "00000000", O/P zero <= '1'
    ----------------------------------
    --** up_dn_s <= '0';  --** up_dn
 
    cycle_count_v := 0;
    WHILE (cycle_count_v < 30) LOOP
       expected_count_val_v := expected_count_val_v - "01";
       Test_14_5_Assert : assert (UNSIGNED(dout_s) = expected_count_val_v) report "## Failed 14.5 Counter Decrement test ###" SEVERITY ERROR; 
       if(UNSIGNED(dout_s) /= expected_count_val_v) then
          tests_failed := tests_failed + 1;
       end if;
       wait for clk_period;
       cycle_count_v := cycle_count_v + 1;
    END LOOP;
 
 
    if (tests_failed = 0) then
        REPORT "<<< REQ_TP_3.14 PASS";
    else
        REPORT "<<< REQ_TP_3.14 FAIL";
    end if;


    --15. Testing the RAM
    -- <<< REQ_TP_3.15

	tests_failed := 0;
    -----------------------
    -- Reset The Counter
    -----------------------
    --** rst_s   <= '0';    --** rst_n
    --** cen_s   <= '0';    --** cen
    --** din_s   <= (OTHERS => '0'); --** din
    --** up_dn_s <= '0';    --** up_dn
    --** ld_n_s  <= '1';   --** ld_n

    ------------------------------
    -- Write Address 0
    -----------------------------
    --** A_0_s   <= "00000";
    --** DI_0_s  <= "11000001";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_1_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.1 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Write Address 1
    -----------------------------
    --** A_0_s   <= "00001";
    --** DI_0_s  <= "11111111";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_2_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.2 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Write Address 2
    -----------------------------
    --** A_0_s   <= "00010";
    --** DI_0_s  <= "00000001";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_3_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.3 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Write Address 4
    -----------------------------
    --** A_0_s   <= "00100";
    --** DI_0_s  <= "01111100";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_4_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.4 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Write Address 8
    -----------------------------
    --** A_0_s   <= "01000";
    --** DI_0_s  <= "01010100";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_5_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.5 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Write Address 16
    -----------------------------
    --** A_0_s   <= "10000";
    --** DI_0_s  <= "01011111";
    --** WEN_0_s <= '1';


    --wait for clk_period/2;
    --** WEN_0_s <= '0';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_6_Assert : assert (DO_0_s = DI_0_s) report "## Failed 15.6 Ram WR/Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= DI_0_s) then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Read Address 16
    -----------------------------
    --** A_0_s   <= "10000";
    --** DI_0_s  <= "00000000";
    --** WEN_0_s <= '1';

    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    --wait for clk_period/2;
    --** WEN_0_s <= '1';
    Test_15_7_Assert : assert (DO_0_s = "01011111") report "## Failed 15.7 Ram Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= "01011111") then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    ------------------------------
    -- Read Address 8
    -----------------------------
    --** A_0_s   <= "01000";
    --** DI_0_s  <= "00000000";
    --** WEN_0_s <= '1';


    --** end_of_test_s <= TRUE;
    
    wait for clk_period;    
    Test_15_8_Assert : assert (DO_0_s = "01010100") report "## Failed 15.8 Ram Rd test ###" SEVERITY ERROR; 
    if(DO_0_s /= "01010100") then
        tests_failed := tests_failed + 1;
    end if;
    wait for clk_period;
    
    if (tests_failed = 0) then
        REPORT "<<< REQ_TP_3.15 PASS";
    else
        REPORT "<<< REQ_TP_3.15 FAIL";
    end if;

    cnt_tst_end : ASSERT FALSE REPORT "*** Simulation Completed !!! ***" SEVERITY NOTE;
  	
    wait;
  end process;
  



  
end bsda_tb_arch3;

