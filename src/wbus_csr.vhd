library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity wbus_csr is
  generic (
    G_TIMESTAMP : std_logic_vector(31 downto 0);
    G_COMMIT_ID : std_logic_vector(31 downto 0)
  );
  port (
    clk_i          : in    std_logic;
    rst_i          : in    std_logic;
    s_wbus_addr_i  : in    std_logic_vector(11 downto 0);
    s_wbus_wrdat_i : in    std_logic_vector(31 downto 0);
    s_wbus_we_i    : in    std_logic;
    s_wbus_cyc_i   : in    std_logic;
    s_wbus_stb_i   : in    std_logic;
    s_wbus_rddat_o : out   std_logic_vector(31 downto 0);
    s_wbus_ack_o   : out   std_logic;
    led_o          : out   std_logic_vector(3 downto 0)
  );
end entity wbus_csr;

architecture synthesis of wbus_csr is

  constant C_REG_MAIN_BUILD_TIME : std_logic_vector(11 downto 0) := X"000"; -- Seconds since 1970-01-01
  constant C_REG_MAIN_COMMIT_ID  : std_logic_vector(11 downto 0) := X"004";
  constant C_REG_MAIN_SCRATCH    : std_logic_vector(11 downto 0) := X"008";
  constant C_REG_MAIN_LED        : std_logic_vector(11 downto 0) := X"00C";

  signal   scratch : std_logic_vector(31 downto 0) := (others => '0');
  signal   led     : std_logic_vector(3 downto 0)  := (others => '0');

begin

  csr_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_wbus_ack_o   <= '0';
      s_wbus_rddat_o <= (others => '0');

      -- Handle read
      if s_wbus_cyc_i = '1' and s_wbus_stb_i = '1' and s_wbus_we_i = '0' then
        s_wbus_ack_o <= '1';                                                  -- Default accept access

        case s_wbus_addr_i(11 downto 0) is

          when C_REG_MAIN_BUILD_TIME =>
            s_wbus_rddat_o <= G_TIMESTAMP;

          when C_REG_MAIN_COMMIT_ID =>
            s_wbus_rddat_o <= G_COMMIT_ID;

          when C_REG_MAIN_SCRATCH =>
            s_wbus_rddat_o <= scratch;

          when C_REG_MAIN_LED =>
            s_wbus_rddat_o(3 downto 0) <= led;

          when others =>
            s_wbus_ack_o <= '0';                                              -- Force timeout

        end case;

      end if;

      -- Handle write
      if s_wbus_cyc_i = '1' and s_wbus_stb_i = '1' and s_wbus_we_i = '1' then
        s_wbus_ack_o <= '1';                                                  -- Default accept access

        case s_wbus_addr_i(11 downto 0) is

          when C_REG_MAIN_BUILD_TIME =>
            null;

          when C_REG_MAIN_COMMIT_ID =>
            null;

          when C_REG_MAIN_SCRATCH =>
            scratch <= s_wbus_wrdat_i;

          when C_REG_MAIN_LED =>
            led <= s_wbus_wrdat_i(3 downto 0);

          when others =>
            s_wbus_ack_o <= '0';                                              -- Force timeout

        end case;

      end if;
    end if;
  end process csr_proc;

  led_o <= led;

end architecture synthesis;

