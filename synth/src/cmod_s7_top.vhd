library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity cmod_s7_top is
  generic (
    G_TIMESTAMP : std_logic_vector(31 downto 0); -- Automatically filled out
    G_COMMIT_ID : std_logic_vector(31 downto 0)  -- Automatically filled out
  );
  port (
    -- Main input clock
    sys_clk_i : in    std_logic;

    btn_i     : in    std_logic_vector(1 downto 0);

    led0_r_o  : out   std_logic;
    led0_g_o  : out   std_logic;
    led0_b_o  : out   std_logic;

    led_o     : out   std_logic_vector(3 downto 0);

    -- UART
    uart_rx_i : in    std_logic;
    uart_tx_o : out   std_logic
  );
end entity cmod_s7_top;

architecture synthesis of cmod_s7_top is

  signal clk : std_logic;
  signal rst : std_logic;

  signal uart_rx : std_logic;
  signal uart_tx : std_logic;

  signal wbus_addr  : std_logic_vector(15 downto 0); -- lower address bits
  signal wbus_wrdat : std_logic_vector(31 downto 0); -- Write Databus
  signal wbus_we    : std_logic;                     -- Write enable
  signal wbus_cyc   : std_logic;                     -- Valid bus cycle
  signal wbus_stb   : std_logic;                     -- Strobe signals / core select signal
  signal wbus_rddat : std_logic_vector(31 downto 0); -- Read Databus
  signal wbus_ack   : std_logic;                     -- Bus cycle acknowledge

begin

  ---------------------------------------------------------
  -- Local Clock and Reset
  ---------------------------------------------------------

  clk_rst_inst : entity work.clk_rst
    port map (
      sys_clk_i => sys_clk_i,
      clk_o     => clk,
      rst_o     => rst
    ); -- clk_rst_inst


  ---------------------------------------------------------
  -- Register I/O signals
  ---------------------------------------------------------

  reg_proc : process (clk)
  begin
    if rising_edge(clk) then
      uart_rx   <= uart_rx_i;
      uart_tx_o <= uart_tx;
    end if;
  end process reg_proc;


  ---------------------------------------------------------
  -- Instantiate UART command decoder
  ---------------------------------------------------------

  uart_wbus_inst : entity work.uart_wbus
    generic map (
      G_CLOCK_KHZ     => 57_600,
      G_UART_BAUDRATE => 115_200,
      G_ADDR_SIZE     => 16
    )
    port map (
      clk_i        => clk,
      rst_i        => rst,
      uart_rxd_i   => uart_rx,
      uart_txd_o   => uart_tx,
      wbus_addr_o  => wbus_addr,
      wbus_wrdat_o => wbus_wrdat,
      wbus_we_o    => wbus_we,
      wbus_cyc_o   => wbus_cyc,
      wbus_stb_o   => wbus_stb,
      wbus_rddat_i => wbus_rddat,
      wbus_ack_i   => wbus_ack
    ); -- uart_wbus_inst : entity work.uart_wbus


  ---------------------------------------------------------
  -- Instantiate main part
  ---------------------------------------------------------

  cmod_s7_inst : entity work.cmod_s7
    generic map (
      G_TIMESTAMP => G_TIMESTAMP,
      G_COMMIT_ID => G_COMMIT_ID
    )
    port map (
      clk_i        => clk,
      rst_i        => rst,
      wbus_addr_i  => wbus_addr,
      wbus_wrdat_i => wbus_wrdat,
      wbus_we_i    => wbus_we,
      wbus_cyc_i   => wbus_cyc,
      wbus_stb_i   => wbus_stb,
      wbus_rddat_o => wbus_rddat,
      wbus_ack_o   => wbus_ack,
      btn_i        => btn_i,
      led0_r_o     => led0_r_o,
      led0_g_o     => led0_g_o,
      led0_b_o     => led0_b_o,
      led_o        => led_o
    ); -- cmod_s7_inst : entity work.cmod_s7

end architecture synthesis;

