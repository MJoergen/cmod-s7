-- ----------------------------------------------------------------------------
-- Description: Drives the MCU interface as specified in a script file
-- ----------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.numeric_std_unsigned.all;

library work;
  use work.tb_parse_keywords_p.all;    -- Include package with command names and associated enums
  use work.tb_parse_p.all;             -- Package with the script parser functions

entity mcu_sim is
  generic (
    G_PERIOD      : time;
    G_ADDR_SIZE   : integer;
    G_CONFIG_MAX  : natural;
    G_STATS_MAX   : natural;
    G_SCRIPT_DIR  : string := ""; -- Directory containing test script file
    G_SCRIPT_NAME : string        -- File name containing test script
  );
  port (
    wbus_clk_o   : out   std_logic;
    wbus_rst_o   : out   std_logic;
    wbus_addr_o  : out   std_logic_vector(G_ADDR_SIZE - 1 downto 0); -- lower address bits
    wbus_wrdat_o : out   std_logic_vector(31 downto 0);              -- Write Databus
    wbus_we_o    : out   std_logic;                                  -- Write enable
    wbus_stb_o   : out   std_logic;                                  -- Strobe signals / core select signal
    wbus_cyc_o   : out   std_logic;                                  -- Valid bus cycle
    wbus_rddat_i : in    std_logic_vector(31 downto 0);              -- Read Databus
    wbus_ack_i   : in    std_logic;                                  -- Bus cycle acknowledge
    config_o     : out   slv32_vector_type(0 to G_CONFIG_MAX);
    stats_i      : in    slv32_vector_type(0 to G_STATS_MAX)
  );
end entity mcu_sim;

architecture simulation of mcu_sim is

  signal running  : std_logic := '1';
  signal wbus_clk : std_logic := '1';

begin

  wbus_rst_o <= '1', '0' after G_PERIOD*10;
  wbus_clk   <= running and not wbus_clk after G_PERIOD/2;

  wbus_clk_o <= wbus_clk;

  -- Main loop
  doscript_proc : process
    variable cmd_v      : t_keyword;
    variable txtstate_v : TxtScanState_t;
    variable testok_v   : boolean := TRUE;
    variable p0_v       : integer;
    variable p1_v       : integer;
    variable p2_v       : integer;
    variable p3_v       : integer;
    variable tmp_v      : std_logic_vector(31 downto 0);

    procedure cpu_write (
      addr : std_logic_vector(G_ADDR_SIZE - 1 downto 0);
      data : std_logic_vector(31 downto 0)
    ) is
    begin
      wbus_cyc_o   <= '1';
      wbus_we_o    <= '1';
      wbus_addr_o  <= addr;
      wbus_wrdat_o <= data;
      wait until rising_edge(wbus_clk);
      wbus_stb_o   <= '1';
      wait until rising_edge(wbus_clk);
      wbus_stb_o   <= '0';

      while wbus_ack_i = '0' loop
        wait until rising_edge(wbus_clk);
      end loop;

      wbus_cyc_o <= '0';
    end procedure cpu_write;

    procedure cpu_read (
      addr : std_logic_vector(G_ADDR_SIZE - 1 downto 0);
      data : out std_logic_vector(31 downto 0)
    ) is
    begin
      wbus_cyc_o  <= '1';
      wbus_we_o   <= '0';
      wbus_addr_o <= addr;
      wait until rising_edge(wbus_clk);
      wbus_stb_o  <= '1';
      wait until rising_edge(wbus_clk);
      wbus_stb_o  <= '0';

      while wbus_ack_i = '0' loop
        wait until rising_edge(wbus_clk);
      end loop;

      wbus_cyc_o <= '0';

      data       := wbus_rddat_i;
    end procedure cpu_read;

    pure function to_slv (
      value : integer;
      size : integer
    ) return std_logic_vector is
    begin
      if value >= 0 then
        return to_stdlogicvector(value, size);
      else
        return not to_stdlogicvector((-value) - 1, size);
      end if;
    end function to_slv;

  --
  begin
    -- The init function copies the script to a buffer and initializes the state of the interpreter
    InitTxtScan(G_SCRIPT_DIR, G_SCRIPT_NAME, txtstate_v);
    -- Set default radix to hex
    txtstate_v.Radix := SCAN_HEXBYTES;

    wbus_stb_o       <= '0';
    wbus_cyc_o       <= '0';
    wbus_addr_o      <= (others => '0');
    wbus_wrdat_o     <= (others => '0');
    wbus_we_o        <= '0';

    config_o         <= (others => (others => '0'));

    -- Wait until out of reset before starting test
    wait until falling_edge(wbus_rst_o);

    -- Interpret script until end of file
    while txtstate_v.Sym /= SCAN_EOF loop
      GetNextCmd(txtstate_v, cmd_v);

      wait until rising_edge(wbus_clk);
      wbus_cyc_o <= '0';
      wbus_stb_o <= '0';

      case cmd_v is

        when CMD_TB_SET_CONFIG =>
          -- TbSetConfig <Addr> <Value>
          -- Get parameters
          Eval(txtstate_v, p0_v);
          Eval(txtstate_v, p1_v);

          assert p0_v >= 0 and p0_v <= 4
            report "TbSetConfig: Device " & to_string(p0_v) & " is out of range"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity error;

          config_o(p0_v) <= to_slv(p1_v, 32);

        when CMD_TB_VERIFY_STATS =>
          -- TbVerifyStats <Addr> <Value>
          -- Get parameters
          Eval(txtstate_v, p0_v);
          Eval(txtstate_v, p1_v);

          assert p0_v >= 0 and p0_v <= 4
            report "TbSetConfig: Device " & to_string(p0_v) & " is out of range"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity error;

          tmp_v := stats_i(p0_v);

          -- Verify the result
          if tmp_v /= to_slv(p1_v, 32) then
            testok_v := FALSE;
            report "TbVerifyStats error at address " & to_string(p0_v)
                   & ". Read " & to_hstring(tmp_v)
                   & ", expected " & to_hstring(to_slv(p1_v, 32))
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
              severity error;
          end if;

        when CMD_TB_WAIT =>
          -- Wait <Duration> (in microseconds)
          -- Get parameters
          Eval(txtstate_v, p0_v);
          -- Duration

          -- Execute the wait command
          wait for p0_v * 1 us;

        when CMD_WRITE =>
          -- Write <Addr> <Data>
          -- Get parameters
          Eval(txtstate_v, p1_v);
          Eval(txtstate_v, p2_v);

          -- sanity check
          assert (p1_v mod 4) = 0
            report "MCU: Write address " & to_hstring(to_slv(p1_v, G_ADDR_SIZE))
                   & " must be a multiple of 4"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity SYNTAXERR;

          -- execute the write command
          cpu_write(to_stdlogicvector(p1_v, G_ADDR_SIZE), to_slv(p2_v, 32));

        when CMD_VERIFY =>
          -- Verify <Addr> <Data>
          -- Get parameters
          Eval(txtstate_v, p1_v);
          Eval(txtstate_v, p2_v);

          -- sanity check
          assert (p1_v mod 4) = 0
            report "MCU: Read address " & to_hstring(to_slv(p1_v, G_ADDR_SIZE))
                   & " must be a multiple of 4"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity SYNTAXERR;

          -- Do the read
          cpu_read(to_slv(p1_v, G_ADDR_SIZE), tmp_v);

          -- Verify the result
          if tmp_v /= to_slv(p2_v, 32) then
            testok_v := FALSE;
            report "MCU: Read error at address " & to_hstring(to_stdlogicvector(p1_v, G_ADDR_SIZE))
                   & ". Read " & to_hstring(tmp_v)
                   & ", expected " & to_hstring(to_slv(p2_v, 32))
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
              severity error;
          end if;

        when CMD_WAIT_FOR_BIT_SET =>
          -- WaitForBitSet <Addr> <Mask> <Timeout (us)>
          -- Get parameters
          Eval(txtstate_v, p0_v);
          Eval(txtstate_v, p1_v);
          Eval(txtstate_v, p2_v);

          -- sanity check
          assert (p0_v mod 4) = 0
            report "MCU: Read address " & to_hstring(to_stdlogicvector(p0_v, G_ADDR_SIZE))
                   & " must be a multiple of 4"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity SYNTAXERR;

          -- Do the polling/waiting
          loop
            cpu_read(to_slv(p0_v, G_ADDR_SIZE), tmp_v);
            if (tmp_v and to_slv(p1_v, 32)) /= 0 then
              exit;
            end if;
            if p2_v = 0 then
              report "MCU: Timeout"
                     & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
                severity error;
              exit;
            end if;
            p2_v := p2_v - 1;
            wait for 1 us;
          end loop;

        when KW_EOF =>
          -- End of script file. Break out of loop
          exit;

        when others =>
          -- Unsupported commands caught here
          assert false
            report "MCU: Command not supported"
                   & ". Line# " & to_str(GetLineNum(txtstate_v), SCAN_DEC)
            severity SYNTAXERR;

      end case;

    --
    end loop;

    -- while txtstate_v.Sym /= SCAN_EOF loop

    assert not testok_v
      report "MCU: Test completed succesfully."
      severity note;

    assert testok_v
      report "MCU: *** ERROR(s) *** in test !"
      severity error;

    running <= '0';
    -- Stop clock to prevent more events
    wait;
  end process doscript_proc;

end architecture simulation;

