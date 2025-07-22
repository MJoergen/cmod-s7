library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.tb_parse_keywords_p.all;    -- Include package with command names and associated enums
  use work.tb_parse_p.all;             -- Package with the script parser functions

entity tb_cmod_s7 is
  generic (
    G_SCRIPT_DIR  : string := "scripts/"; -- Directory containing test script files
    G_SCRIPT_NAME : string                -- File name containing test script
  );
end entity tb_cmod_s7;

architecture simulation of tb_cmod_s7 is

  constant C_TIMESTAMP      : std_logic_vector(31 downto 0)  := X"23456789";
  constant C_COMMIT_ID      : std_logic_vector(31 downto 0)  := X"3456789A";
  constant C_MAIN_ADDR_SIZE : natural                        := 16;

  constant C_CONFIG_MAX : natural                            := 10;
  constant C_STATS_MAX  : natural                            := 10;

  signal   tb_config  : slv32_vector_type(0 to C_CONFIG_MAX) := (others => (others => '0'));
  signal   tb_stats   : slv32_vector_type(0 to C_STATS_MAX)  := (others => (others => '0'));
  signal   tb_running : std_logic                            := '1';

  -- Wishbone bus interface from Test Bench
  signal   wbus_clk : std_logic                              := '1';
  signal   wbus_rst : std_logic                              := '1';

  signal   wbus_addr  : std_logic_vector(C_MAIN_ADDR_SIZE - 1 downto 0);
  signal   wbus_wrdat : std_logic_vector(31 downto 0);
  signal   wbus_we    : std_logic;
  signal   wbus_cyc   : std_logic;
  signal   wbus_stb   : std_logic;
  signal   wbus_rddat : std_logic_vector(31 downto 0);
  signal   wbus_ack   : std_logic;

  signal   led0_r  : std_logic;
  signal   led0_g  : std_logic;
  signal   led0_b  : std_logic;
  signal   uart_rx : std_logic;
  signal   uart_tx : std_logic;

begin

  ------------------------------------------------
  -- Convert script files to WBUS transactions
  ------------------------------------------------

  mcu_sim_inst : entity work.mcu_sim
    generic map (
      G_PERIOD      => 17.361 ns,    -- 57.6 MHz
      G_ADDR_SIZE   => C_MAIN_ADDR_SIZE,
      G_CONFIG_MAX  => C_CONFIG_MAX,
      G_STATS_MAX   => C_STATS_MAX,
      G_SCRIPT_DIR  => G_SCRIPT_DIR,
      G_SCRIPT_NAME => G_SCRIPT_NAME
    )
    port map (
      wbus_clk_o   => wbus_clk,
      wbus_rst_o   => wbus_rst,
      wbus_addr_o  => wbus_addr,
      wbus_wrdat_o => wbus_wrdat,
      wbus_we_o    => wbus_we,
      wbus_stb_o   => wbus_stb,
      wbus_cyc_o   => wbus_cyc,
      wbus_rddat_i => wbus_rddat,
      wbus_ack_i   => wbus_ack,
      config_o     => tb_config,
      stats_i      => tb_stats
    ); -- mcu_sim_inst


  ---------------------------------------------------------
  -- Instantiate DUT
  ---------------------------------------------------------

  cmod_s7_inst : entity work.cmod_s7
    generic map (
      G_TIMESTAMP => C_TIMESTAMP,
      G_COMMIT_ID => C_COMMIT_ID
    )
    port map (
      clk_i        => wbus_clk,
      rst_i        => wbus_rst,
      wbus_addr_i  => wbus_addr,
      wbus_wrdat_i => wbus_wrdat,
      wbus_we_i    => wbus_we,
      wbus_stb_i   => wbus_stb,
      wbus_cyc_i   => wbus_cyc,
      wbus_rddat_o => wbus_rddat,
      wbus_ack_o   => wbus_ack,
      btn_i        => tb_config(0)(1 downto 0),
      led0_r_o     => led0_r,
      led0_g_o     => led0_g,
      led0_b_o     => led0_b,
      led_o        => tb_stats(0)(3 downto 0)
    ); -- cmod_s7_inst : entity work.cmod_s7

end architecture simulation;

