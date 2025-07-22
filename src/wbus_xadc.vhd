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

  --------------------------------------------------------------------------------
  -- configuration register constants

  -- C_INIT_40:
  -- * CH   = 00000
  -- * ACQ  = 0.      Default acquisition time.
  -- * EC   = 0.      Continuous sampling mode.
  -- * BU   = 0.      Unipolar mode.
  -- * MUX  = 0.      Disable external multiplexer mode.
  -- * AVG  = 00.     No averaging.
  -- * CAVG = 1.      Disable calibration averaging.
  constant C_INIT_40 : bit_vector := x"8000";

  -- C_INIT_41:
  -- * OT  = 1.       Disable over-temperature alarm.
  -- * ALM = 0001111. Disable alarms on Temperature, V_CCINT, V_CCAUX, and V_CCBRAM.
  -- * CAL = 0000.    Disable calibration.
  -- * SEQ = 0011.    Single channel mode (sequencer off).
  constant C_INIT_41 : bit_vector := x"310F";

  -- C_INIT_42:
  -- * PD = 00.       No power down
  -- * CD = 00001010. Division=10 => ADCCLK=5.76 MHz
  -- Note: Maximum ADC clock on Spartan 7 is 26 MHz.
  constant C_INIT_42 : bit_vector := x"0A00";

  -- C_INIT_48:
  -- Enable Temperature
  constant C_INIT_48 : bit_vector := x"0100";

  --------------------------------------------------------------------------------

  signal   drp_en   : std_ulogic;
  signal   drp_we   : std_ulogic;
  signal   drp_addr : std_ulogic_vector(8 downto 0);
  signal   drp_din  : std_ulogic_vector(15 downto 0);
  signal   drp_dout : std_ulogic_vector(15 downto 0);
  signal   drp_rdy  : std_ulogic;

  signal   adc_muxaddr   : std_ulogic_vector(4 downto 0);
  signal   adc_channel   : std_ulogic_vector(4 downto 0);
  signal   adc_busy      : std_ulogic;
  signal   adc_eoc       : std_ulogic;
  signal   adc_eos       : std_ulogic;
  signal   adc_jtag_lock : std_ulogic;

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
      drp_addr_o   => drp_addr,
      drp_en_o     => drp_en,
      drp_di_o     => drp_din,
      drp_we_o     => drp_we,
      drp_do_i     => drp_dout,
      drp_rdy_i    => drp_rdy
    ); -- wbus_drp_inst : entity work.wbus_drp

  xadc_inst : component xadc
    generic map (
      SIM_MONITOR_FILE => "xadc_sim.txt",
      SIM_DEVICE       => "7SERIES",
      INIT_40          => C_INIT_40,
      INIT_41          => C_INIT_41,
      INIT_42          => C_INIT_42,
      INIT_48          => C_INIT_48
    )
    port map (
      reset        => rst_i,
      dclk         => clk_i,
      den          => drp_en,
      dwe          => drp_we,
      daddr        => drp_addr(6 downto 0),
      di           => drp_din,
      do           => drp_dout,
      drdy         => drp_rdy,
      jtagbusy     => open,
      jtaglocked   => adc_jtag_lock,
      jtagmodified => open,
      convst       => '0',
      convstclk    => '0',
      muxaddr      => adc_muxaddr, -- external mux address
      channel      => adc_channel, -- channel select at EOC
      busy         => adc_busy,    -- ADC conversion in progress
      eoc          => adc_eoc,     -- end of conversion output
      eos          => adc_eos,     -- end of sequence output
      alm          => open,        -- alarm outputs
      ot           => open,        -- over-temperature
      vauxn        => (others => '0'),
      vauxp        => (others => '0'),
      vn           => '0',
      vp           => '0'
    ); -- xadc_inst : component xadc

end architecture synthesis;

