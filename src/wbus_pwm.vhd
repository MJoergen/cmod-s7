library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std_unsigned.all;

entity wbus_pwm is
  port (
    clk_i        : in    std_logic;
    rst_i        : in    std_logic;

    wbus_addr_i  : in    std_logic_vector(11 downto 0);
    wbus_wrdat_i : in    std_logic_vector(31 downto 0);
    wbus_we_i    : in    std_logic;
    wbus_cyc_i   : in    std_logic;
    wbus_stb_i   : in    std_logic;
    wbus_rddat_o : out   std_logic_vector(31 downto 0);
    wbus_ack_o   : out   std_logic;

    pwm_o        : out   std_logic
  );
end entity wbus_pwm;

architecture synthesis of wbus_pwm is

  constant C_CNT_MAX : natural := 100;

  signal   pwm  : std_logic;
  signal   cnt  : natural range 0 to C_CNT_MAX - 1;
  signal   duty : natural range 0 to C_CNT_MAX;

begin

  duty_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      wbus_ack_o <= '0';
      if wbus_cyc_i = '1' and wbus_stb_i = '1' and wbus_addr_i = 0 then
        if wbus_we_i = '1' and to_integer(wbus_wrdat_i) <= C_CNT_MAX then
          duty <= to_integer(wbus_wrdat_i);
        end if;
        wbus_rddat_o <= to_stdlogicvector(duty, 32);
        wbus_ack_o   <= '1';
      end if;

      if wbus_cyc_i = '1' and wbus_stb_i = '1' and wbus_addr_i = 4 then
        wbus_rddat_o <= to_stdlogicvector(C_CNT_MAX, 32);
        wbus_ack_o   <= '1';
      end if;

      if rst_i = '1' then
        duty <= C_CNT_MAX / 10;
      end if;
    end if;
  end process duty_proc;

  pwm_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if cnt = C_CNT_MAX - 1 then
        cnt <= 0;
      else
        cnt <= cnt + 1;
      end if;

      if cnt < duty then
        pwm_o <= '0';
      else
        pwm_o <= '1';
      end if;
    end if;
  end process pwm_proc;

end architecture synthesis;

