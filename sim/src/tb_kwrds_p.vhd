-- ##########################################################################
--
-- Description: Sample keyword file for use by tb_parse_p package.
--
-- ##########################################################################

library ieee;
  use ieee.std_logic_1164.all;

library std;
  use std.textio.all;

library work;
  use work.tb_parse_p.all;

package tb_parse_keywords_p is

  type     slv32_vector_type is array (natural range <>) of std_logic_vector(31 downto 0);

  constant C_KEYWORDNAMES : string :=
    -- Predefined keyword names. Do NOT change these.
                                      "while loop endloop if then else endif " &

    -- Additional command names for application go here
                                      "TbSetConfig TbVerifyStats " &
                                      "VerifyState "&
                                      "Write Verify TbWait WaitForBitSet Log";

  type     t_keyword is (
    -- Predefined keyword values. Do NOT change these.
    KW_EOF,
    KW_WHILE,
    KW_LOOP,
    KW_ENDLOOP,
    KW_IF,
    KW_THEN,
    KW_ELSE,
    KW_ENDIF,

    -- Additional command names for application go here. One per command name
    CMD_TB_SET_CONFIG,
    CMD_TB_VERIFY_STATS,
    CMD_VERIFY_STATE,
    CMD_WRITE,
    CMD_VERIFY,
    CMD_TB_WAIT,
    CMD_WAIT_FOR_BIT_SET,
    CMD_LOG
  );

  procedure inittxtscan (
    constant ScriptDir : in string;
    constant FName     : in    string;
    S                  : inout TxtScanState_t);

  procedure getnextcmd (
    S   : inout TxtScanState_t;
    Cmd : out t_keyword); -- Enumerated value of command output in 2nd argument

end package tb_parse_keywords_p;

library work;
  use std.textio.all;
  use work.tb_parse_p.all;

package body tb_parse_keywords_p is

  procedure inittxtscan (
    constant ScriptDir : in string;
    constant FName     : in string;
    S                  : inout TxtScanState_t) is
  begin
    inittxtscan(ScriptDir, FName, C_KEYWORDNAMES, S);
  end;

  procedure getnextcmd (S : inout TxtScanState_t; Cmd : out t_keyword) is
    variable i_v : integer;
  begin
    getnextcmd(S, i_v);
    Cmd := t_keyword'val(i_v);
  end;

end package body tb_parse_keywords_p;

