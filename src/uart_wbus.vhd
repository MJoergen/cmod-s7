-- ----------------------------------------------------------------------------
-- Description: USB to Wishbone converter
-- ----------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity uart_wbus is
  generic (
    G_CLOCK_KHZ     : natural;
    G_UART_BAUDRATE : natural := 115_200;
    G_ADDR_SIZE     : natural := 10
  );
  port (
    clk_i        : in    std_logic;
    rst_i        : in    std_logic;

    -- UART Control
    uart_rxd_i   : in    std_logic;
    uart_txd_o   : out   std_logic;

    -- Wishbone bus Master interface
    wbus_addr_o  : out   std_logic_vector(G_ADDR_SIZE - 1 downto 0); -- lower address bits
    wbus_wrdat_o : out   std_logic_vector(31 downto 0);              -- Write Databus
    wbus_we_o    : out   std_logic;                                  -- Write enable
    wbus_cyc_o   : out   std_logic;                                  -- Valid bus cycle
    wbus_stb_o   : out   std_logic;                                  -- Strobe signals / core select signal
    wbus_rddat_i : in    std_logic_vector(31 downto 0);              -- Read Databus
    wbus_ack_i   : in    std_logic                                   -- Bus cycle acknowledge
  );
end entity uart_wbus;

architecture synthesis of uart_wbus is

  constant C_BUFFER_LEN : natural := 20;

  signal   rx_valid : std_logic;
  signal   rx_ready : std_logic;
  signal   rx_data  : std_logic_vector(7 downto 0);
  signal   tx_ready : std_logic;
  signal   tx_valid : std_logic;
  signal   tx_data  : std_logic_vector(7 downto 0);

  signal   rx_buffer_ready : std_logic;
  signal   rx_buffer_valid : std_logic;
  signal   rx_buffer_size  : natural range 0 to C_BUFFER_LEN;
  signal   rx_buffer_data  : std_logic_vector(8 * C_BUFFER_LEN - 1 downto 0);
  signal   tx_buffer_ready : std_logic;
  signal   tx_buffer_valid : std_logic;
  signal   tx_buffer_size  : natural range 0 to C_BUFFER_LEN;
  signal   tx_buffer_data  : std_logic_vector(8 * C_BUFFER_LEN - 1 downto 0);

begin

  uart_serdes_inst : entity work.uart_serdes
    generic map (
      G_DIVISOR => G_CLOCK_KHZ * 1000 / G_UART_BAUDRATE
    )
    port map (
      clk_i      => clk_i,
      rst_i      => rst_i,
      uart_tx_o  => uart_txd_o,
      uart_rx_i  => uart_rxd_i,
      rx_valid_o => rx_valid,
      rx_ready_i => rx_ready,
      rx_data_o  => rx_data,
      tx_valid_i => tx_valid,
      tx_ready_o => tx_ready,
      tx_data_i  => tx_data
    ); -- uart_serdes_inst : entity work.uart_serdes

  line_buffer_inst : entity work.line_buffer
    generic map (
      G_BUFFER_LEN => C_BUFFER_LEN
    )
    port map (
      clk_i             => clk_i,
      rst_i             => rst_i,
      rx_ready_o        => rx_ready,
      rx_valid_i        => rx_valid,
      rx_data_i         => rx_data,
      tx_ready_i        => tx_ready,
      tx_valid_o        => tx_valid,
      tx_data_o         => tx_data,
      rx_buffer_ready_i => rx_buffer_ready,
      rx_buffer_valid_o => rx_buffer_valid,
      rx_buffer_size_o  => rx_buffer_size,
      rx_buffer_data_o  => rx_buffer_data,
      tx_buffer_ready_o => tx_buffer_ready,
      tx_buffer_valid_i => tx_buffer_valid,
      tx_buffer_size_i  => tx_buffer_size,
      tx_buffer_data_i  => tx_buffer_data
    ); -- line_buffer_inst : entity work.line_buffer

  cmd_inst : entity work.cmd
    generic map (
      G_ADDR_SIZE  => G_ADDR_SIZE,
      G_BUFFER_LEN => C_BUFFER_LEN
    )
    port map (
      clk_i             => clk_i,
      rst_i             => rst_i,
      rx_buffer_ready_o => rx_buffer_ready,
      rx_buffer_valid_i => rx_buffer_valid,
      rx_buffer_size_i  => rx_buffer_size,
      rx_buffer_data_i  => rx_buffer_data,
      tx_buffer_ready_i => tx_buffer_ready,
      tx_buffer_valid_o => tx_buffer_valid,
      tx_buffer_size_o  => tx_buffer_size,
      tx_buffer_data_o  => tx_buffer_data,
      wbus_addr_o       => wbus_addr_o,
      wbus_wrdat_o      => wbus_wrdat_o,
      wbus_we_o         => wbus_we_o,
      wbus_cyc_o        => wbus_cyc_o,
      wbus_stb_o        => wbus_stb_o,
      wbus_rddat_i      => wbus_rddat_i,
      wbus_ack_i        => wbus_ack_i
    ); -- cmd_inst : entity work.cmd

end architecture synthesis;

