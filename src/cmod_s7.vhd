library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.wbus_pkg.slv32_array_type;

entity cmod_s7 is
  generic (
    G_TIMESTAMP : std_logic_vector(31 downto 0); -- Automatically filled out
    G_COMMIT_ID : std_logic_vector(31 downto 0)  -- Automatically filled out
  );
  port (
    -- Main input clock
    clk_i        : in    std_logic;
    rst_i        : in    std_logic;

    wbus_addr_i  : in    std_logic_vector(15 downto 0); -- lower address bits
    wbus_wrdat_i : in    std_logic_vector(31 downto 0); -- Write Databus
    wbus_we_i    : in    std_logic;                     -- Write enable
    wbus_cyc_i   : in    std_logic;                     -- Valid bus cycle
    wbus_stb_i   : in    std_logic;                     -- Strobe signals / core select signal
    wbus_rddat_o : out   std_logic_vector(31 downto 0); -- Read Databus
    wbus_ack_o   : out   std_logic;                     -- Bus cycle acknowledge

    btn_i        : in    std_logic_vector(1 downto 0);

    led0_r_o     : out   std_logic;
    led0_g_o     : out   std_logic;
    led0_b_o     : out   std_logic;

    led_o        : out   std_logic_vector(3 downto 0)
  );
end entity cmod_s7;

architecture synthesis of cmod_s7 is

  constant C_NUM_SLAVES : natural := 2;

  signal   wbus_map_addr  : std_logic_vector(11 downto 0);               -- lower address bits
  signal   wbus_map_wrdat : std_logic_vector(31 downto 0);               -- Write Databus
  signal   wbus_map_we    : std_logic;                                   -- Write enable
  signal   wbus_map_cyc   : std_logic;                                   -- Valid bus cycle
  signal   wbus_map_stb   : std_logic_vector(C_NUM_SLAVES - 1 downto 0); -- Strobe signals / core select signal
  signal   wbus_map_rddat : slv32_array_type(C_NUM_SLAVES - 1 downto 0); -- Read Databus
  signal   wbus_map_ack   : std_logic_vector(C_NUM_SLAVES - 1 downto 0); -- Bus cycle acknowledge

begin

  wbus_mapper_inst : entity work.wbus_mapper
    generic map (
      G_TIMEOUT          => 10_000,
      G_NUM_SLAVES       => 2,
      G_MASTER_ADDR_SIZE => 16,
      G_SLAVE_ADDR_SIZE  => 12
    )
    port map (
      clk_i          => clk_i,
      rst_i          => rst_i,
      s_wbus_addr_i  => wbus_addr_i,
      s_wbus_wrdat_i => wbus_wrdat_i,
      s_wbus_we_i    => wbus_we_i,
      s_wbus_cyc_i   => wbus_cyc_i,
      s_wbus_stb_i   => wbus_stb_i,
      s_wbus_rddat_o => wbus_rddat_o,
      s_wbus_ack_o   => wbus_ack_o,
      m_wbus_addr_o  => wbus_map_addr,
      m_wbus_wrdat_o => wbus_map_wrdat,
      m_wbus_we_o    => wbus_map_we,
      m_wbus_cyc_o   => wbus_map_cyc,
      m_wbus_stb_o   => wbus_map_stb,
      m_wbus_rddat_i => wbus_map_rddat,
      m_wbus_ack_i   => wbus_map_ack
    ); -- wbus_mapper_inst : entity work.wbus_mapper

  wbus_csr_inst : entity work.wbus_csr
    generic map (
      G_TIMESTAMP => G_TIMESTAMP,
      G_COMMIT_ID => G_COMMIT_ID
    )
    port map (
      clk_i          => clk_i,
      rst_i          => rst_i,
      s_wbus_addr_i  => wbus_map_addr,
      s_wbus_wrdat_i => wbus_map_wrdat,
      s_wbus_we_i    => wbus_map_we,
      s_wbus_cyc_i   => wbus_map_cyc,
      s_wbus_stb_i   => wbus_map_stb(0),
      s_wbus_rddat_o => wbus_map_rddat(0),
      s_wbus_ack_o   => wbus_map_ack(0)
    ); -- wbus_csr_inst : entity work.wbus_csr

  wbus_xadc_inst : entity work.wbus_xadc
    port map (
      clk_i        => clk_i,
      rst_i        => rst_i,
      wbus_addr_i  => wbus_map_addr,
      wbus_wrdat_i => wbus_map_wrdat,
      wbus_we_i    => wbus_map_we,
      wbus_cyc_i   => wbus_map_cyc,
      wbus_stb_i   => wbus_map_stb(1),
      wbus_rddat_o => wbus_map_rddat(1),
      wbus_ack_o   => wbus_map_ack(1)
    ); -- wbus_xadc_inst : entity work.wbus_xadc

  led0_r_o <= '0';
  led0_g_o <= '0';
  led0_b_o <= '0';
  led_o    <= (others => '0');

end architecture synthesis;

