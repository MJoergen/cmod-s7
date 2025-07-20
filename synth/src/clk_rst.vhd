-- ----------------------------------------------------------------------------
-- Description: Clock and Reset
-- ----------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library unisim;
  use unisim.vcomponents.all;

library xpm;
  use xpm.vcomponents.all;

entity clk_rst is
  port (
    sys_clk_i : in    std_logic; -- Reference Clock, 12 MHz

    clk_o     : out   std_logic;
    rst_o     : out   std_logic
  );
end entity clk_rst;

architecture synthesis of clk_rst is

  signal mmcm_fb     : std_logic;
  signal mmcm_clk    : std_logic;
  signal mmcm_locked : std_logic;

begin


  mmcm_inst : component mmcme2_base
    generic map (
      BANDWIDTH          => "OPTIMIZED",
      CLKFBOUT_MULT_F    => 63.000,
      CLKFBOUT_PHASE     => 0.000,
      CLKIN1_PERIOD      => 83.3330, -- INPUT @ 12 MHz
      CLKOUT0_DIVIDE_F   => 13.125,  -- OUTPUT @ 57.6 MHz
      CLKOUT0_DUTY_CYCLE => 0.500,
      CLKOUT0_PHASE      => 0.000,
      DIVCLK_DIVIDE      => 1,
      REF_JITTER1        => 0.010,
      STARTUP_WAIT       => FALSE
    )
    port map (
      clkfbin  => mmcm_fb,
      clkfbout => mmcm_fb,
      clkin1   => sys_clk_i,
      clkout0  => mmcm_clk,
      locked   => mmcm_locked,
      pwrdwn   => '0',
      rst      => '0'
    ); -- mmcm_inst


  bufg_inst : component bufg
    port map (
      i => mmcm_clk,
      o => clk_o
    ); -- bufg_inst


  xpm_cdc_async_inst : component xpm_cdc_async_rst
    generic map (
      RST_ACTIVE_HIGH => 1
    )
    port map (
      src_arst  => not mmcm_locked,
      dest_clk  => clk_o,
      dest_arst => rst_o
    ); -- xpm_cdc_async_inst

end architecture synthesis;

