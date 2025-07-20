library ieee;
  use ieee.std_logic_1164.all;

library unisim;
  use unisim.vcomponents.all;

entity wbus_xadc is
  port (
    clk_i        : in    std_ulogic;
    rst_i        : in    std_ulogic;

    wbus_addr_i  : in    std_logic_vector(11 downto 0);
    wbus_wrdat_i : in    std_logic_vector(31 downto 0);
    wbus_we_i    : in    std_logic;
    wbus_cyc_i   : in    std_logic;
    wbus_stb_i   : in    std_logic;
    wbus_rddat_o : out   std_logic_vector(31 downto 0);
    wbus_ack_o   : out   std_logic
  );
end entity wbus_xadc;

architecture synthesis of wbus_xadc is

  signal   en   : std_ulogic;
  signal   we   : std_ulogic;
  signal   addr : std_ulogic_vector(8 downto 0);
  signal   din  : std_ulogic_vector(15 downto 0);
  signal   dout : std_ulogic_vector(15 downto 0);
  signal   rdy  : std_ulogic;
  signal   bsy  : std_ulogic;

  signal   adc_busy  : std_ulogic;
  signal   jtag_lock : std_ulogic;

  --------------------------------------------------------------------------------
  -- configuration register constants

  -- channel 0 = on chip temperature (n/a in default sequencer mode)
  -- no increased settling time
  -- continuous sampling
  -- bipolar operating mode
  -- internal mux
  -- average 256 samples
  -- enable coefficient averaging
  constant C_CFGREG0 : bit_vector := x"3400";

  -- disable OT
  -- disable alarms
  -- enable calibration coefficients
  -- default sequencer mode
  constant C_CFGREG1 : bit_vector := x"0FFF";

  -- enable both ADCs
  -- ADC freq = 100MHz / 100 = 1MHz
  constant C_CFGREG2 : bit_vector := x"6400";

--------------------------------------------------------------------------------

begin

  wbus_drp_inst : entity work.wbus_drp
    generic map (
      G_ADDR_SIZE => 12,
      G_DATA_SIZE => 32
    )
    port map (
      clk_i        => clk_i,
      rst_i        => rst_i,
      wbus_addr_i  => wbus_addr_i,
      wbus_wrdat_i => wbus_wrdat_i,
      wbus_we_i    => wbus_we_i,
      wbus_cyc_i   => wbus_cyc_i,
      wbus_stb_i   => wbus_stb_i,
      wbus_rddat_o => wbus_rddat_o,
      wbus_ack_o   => wbus_ack_o,
      drp_addr_o   => addr,
      drp_en_o     => en,
      drp_di_o     => din,
      drp_we_o     => we,
      drp_do_i     => dout,
      drp_rdy_i    => rdy
    ); -- wbus_drp_inst : entity work.wbus_drp

  bsy <= adc_busy or jtag_lock;

  xadc_inst : component xadc
    generic map (
      SIM_MONITOR_FILE => "xadc_sim.txt",
      INIT_40          => C_CFGREG0,
      INIT_41          => C_CFGREG1,
      INIT_42          => C_CFGREG2
    )
    port map (
      reset        => rst_i,
      dclk         => clk_i,
      den          => en,
      dwe          => we,
      daddr        => addr(6 downto 0),
      di           => din,
      do           => dout,
      drdy         => rdy,
      jtagbusy     => open,
      jtaglocked   => jtag_lock,
      jtagmodified => open,
      convst       => '0',
      convstclk    => '0',
      muxaddr      => open,     -- external mux address
      channel      => open,     -- channel select at EOC
      busy         => adc_busy, -- ADC conversion in progress
      eoc          => open,     -- end of conversion output
      eos          => open,     -- end of sequence output
      alm          => open,     -- alarm outputs
      ot           => open,     -- over-temperature
      vauxn        => (others => '0'),
      vauxp        => (others => '0'),
      vn           => '0',
      vp           => '0'
    ); -- xadc_inst : component xadc

end architecture synthesis;

