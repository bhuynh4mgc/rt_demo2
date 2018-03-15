library ieee;
    use ieee.std_logic_1164.all;
    use ieee.std_logic_arith.all;

entity ip_core is 
  port (clk         :  in std_ulogic;
        rst_n       :  in std_ulogic;
	ld_n        :  in std_ulogic;
	cen         :  in std_ulogic;
	up_dn       :  in std_ulogic;
	din         :  in std_ulogic_vector(7 DOWNTO 0);
	tc          : out std_ulogic;
	zero        : out std_ulogic;
	dout        : out std_ulogic_vector(7 DOWNTO 0);
	mbist_clk   :  in std_ulogic;
	mbist_rst_n :  in std_ulogic;
	test_h      :  in std_ulogic;
	A_0         :  in std_ulogic_vector(4 DOWNTO 0);
	DI_0        :  in std_ulogic_vector(7 DOWNTO 0);
	WEN_0       :  in std_ulogic;
	DO_0        : out std_ulogic_vector(7 DOWNTO 0);
	tst_done    : out std_ulogic;
	fail_h      : out std_ulogic
	);
end ip_core;	 



architecture struct of ip_core is


-- <<< JTAG_138
component counter
  port (clk     :  in std_ulogic;
        rst_n   :  in std_ulogic;
	ld_n    :  in std_ulogic;
	cen     :  in std_ulogic;
	up_dn   :  in std_ulogic;
	din     :  in std_ulogic_vector(7 DOWNTO 0);
	tc      : out std_ulogic;
	zero    : out std_ulogic;
	dout    : out std_ulogic_vector(7 DOWNTO 0));
end component;

-- <<< JTAG_132
component ram32x8_bist_con
   port(
      tst_done : out STD_ULOGIC;
      fail_h   : out STD_ULOGIC;
      test_h   :  in STD_ULOGIC;
      bist_clk :  in STD_ULOGIC;
      rst_l    :  in STD_ULOGIC;
      DO_0     : out STD_ULOGIC_VECTOR(7 downto 0);
      A_0      :  in STD_ULOGIC_VECTOR(4 downto 0);
      DI_0     :  in STD_ULOGIC_VECTOR(7 downto 0);
      WEN_0    :  in STD_ULOGIC);
END component;


begin

inst_bist1 : ram32x8_bist_con
 port map(tst_done => tst_done,
          fail_h   => fail_h,
          test_h   => test_h,
          bist_clk => mbist_clk,
          rst_l    => mbist_rst_n,
          DO_0     => DO_0,
          A_0      => A_0,
          DI_0     => DI_0,
          WEN_0    => WEN_0);
		  
inst_cnt1 : counter
 port map( clk    => clk,
           rst_n  => rst_n,
           ld_n   => ld_n,
           cen    => cen,
           up_dn  => up_dn,
           din    => din,
           tc     => tc,
           zero   => zero,    
           dout   => dout);
		   		  
end struct;
