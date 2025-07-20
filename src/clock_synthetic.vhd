library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity clock_synthetic is
  generic (
    G_INPUT_FREQ_HZ  : natural;
    G_OUTPUT_FREQ_HZ : natural
  );
  port (
    clk_i : in    std_logic;
    clk_o : out   std_logic
  );
end entity clock_synthetic;

architecture synthesis of clock_synthetic is

  signal counter : natural range 0 to G_INPUT_FREQ_HZ - 1 := 0;
  signal clk     : std_logic                              := '0';

  pure function gcd (
    a : natural;
    b : natural
  ) return natural is
  begin
    if b < a then
      return gcd(b, a);
    end if;

    if a = b then
      return a;
    end if;

    -- Now a < b
    if a = 0 then
      return b;
    end if;

    -- Now 0 < a < b
    return gcd(b mod a, a);
  end function gcd;

  constant C_FACTOR                 : natural               := gcd(G_OUTPUT_FREQ_HZ, G_INPUT_FREQ_HZ);
  constant C_INPUT_FREQ_HZ_REDUCED  : natural               := G_INPUT_FREQ_HZ / C_FACTOR;
  constant C_OUTPUT_FREQ_HZ_REDUCED : natural               := G_OUTPUT_FREQ_HZ / C_FACTOR;

begin

  assert 2 * G_OUTPUT_FREQ_HZ <= G_INPUT_FREQ_HZ;

  clk_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if counter + 2 * C_OUTPUT_FREQ_HZ_REDUCED < C_INPUT_FREQ_HZ_REDUCED then
        counter <= counter + 2 * C_OUTPUT_FREQ_HZ_REDUCED;
      else
        counter <= counter + 2 * C_OUTPUT_FREQ_HZ_REDUCED - C_INPUT_FREQ_HZ_REDUCED;
        clk     <= not clk;
      end if;
    end if;
  end process clk_proc;

  clk_o <= clk;

end architecture synthesis;

