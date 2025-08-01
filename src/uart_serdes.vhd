library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

-- This provides a byte-level interface to UART serial interface.
-- Protocol is 8-N-1, i.e. one start bit, 8 data bits, no parity, and one stop bit.
-- Data is transferred LSB first.

entity uart_serdes is
  generic (
    G_DIVISOR : natural
  );
  port (
    clk_i      : in    std_logic;
    rst_i      : in    std_logic;
    -- Byte level interface
    tx_valid_i : in    std_logic;
    tx_ready_o : out   std_logic;
    tx_data_i  : in    std_logic_vector(7 downto 0);
    rx_valid_o : out   std_logic;
    rx_ready_i : in    std_logic;
    rx_data_o  : out   std_logic_vector(7 downto 0);
    -- UART serial interface
    uart_tx_o  : out   std_logic;
    uart_rx_i  : in    std_logic
  );
end entity uart_serdes;

architecture synthesis of uart_serdes is

  type   state_type is (
    IDLE_ST,
    BUSY_ST
  );

  signal tx_data    : std_logic_vector(9 downto 0) := (others => '1');
  signal tx_state   : state_type                   := IDLE_ST;
  signal tx_counter : natural range 0 to G_DIVISOR;

  signal rx_data    : std_logic_vector(9 downto 0);
  signal rx_state   : state_type                   := IDLE_ST;
  signal rx_counter : natural range 0 to G_DIVISOR;

begin

  --------------------------------------------
  -- Handler UART Transmission
  --------------------------------------------

  tx_ready_o <= '1' when tx_state = IDLE_ST else
                '0';

  uart_tx_o  <= tx_data(0);

  tx_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then

      case tx_state is

        when IDLE_ST =>
          if tx_valid_i = '1' then
            tx_data    <= "1" & tx_data_i & "0"; -- Stop + Data + Start
            tx_counter <= G_DIVISOR;
            tx_state   <= BUSY_ST;
          end if;

        when BUSY_ST =>
          if tx_counter > 0 then
            tx_counter <= tx_counter - 1;
          else
            if or (tx_data(9 downto 1)) = '1' then
              tx_counter <= G_DIVISOR;
              tx_data    <= "0" & tx_data(9 downto 1);
            else
              tx_data(0) <= '1';
              tx_state   <= IDLE_ST;
            end if;
          end if;

      end case;

      if rst_i = '1' then
        tx_data(0) <= '1';
        tx_state   <= IDLE_ST;
      end if;
    end if;
  end process tx_proc;


  --------------------------------------------
  -- Handler UART Reception
  --------------------------------------------

  rx_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rx_ready_i = '1' then
        rx_valid_o <= '0';
      end if;

      case rx_state is

        when IDLE_ST =>
          if uart_rx_i = '0' then
            rx_data    <= (others => '1');
            rx_data(9) <= uart_rx_i;
            rx_counter <= G_DIVISOR / 2;
            rx_state   <= BUSY_ST;
          end if;

        when BUSY_ST =>
          if rx_counter > 0 then
            rx_counter <= rx_counter - 1;
          else
            rx_counter <= G_DIVISOR;
            rx_data    <= uart_rx_i & rx_data(9 downto 1);
          end if;

          if rx_data(0) = '0' and rx_data(9) = '1' then
            -- Only forward if Stop bit is correct.
            rx_data_o  <= rx_data(8 downto 1);
            rx_valid_o <= '1';
            rx_state   <= IDLE_ST;
          end if;

      end case;

      if rst_i = '1' then
        rx_valid_o <= '0';
        rx_state   <= IDLE_ST;
      end if;
    end if;
  end process rx_proc;

end architecture synthesis;

