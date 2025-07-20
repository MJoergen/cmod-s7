-- ##########################################################################
--
-- Script parser package tb_parse_p.
--
-- Refer documentation in tb_parse.txt.
--
-- ##########################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use IEEE.std_logic_arith.all;
--use IEEE.std_logic_unsigned.all;
use std.textio.all;

package tb_parse_p is

  constant SYNTAXERR  : severity_level := failure;
  constant SYMSPACE  : integer := 10000;
  constant MAX_SYM  : integer := 1000;
  constant MAX_FILESZ  : integer := 100000;
  constant MAX_INC_FILES : integer := 20;
  constant MAX_NEST  : integer := 20;

  constant ErrHead: string := "TXTSCAN: ";

  type IntegerArray_t  is array(natural range <>) of integer;
  constant INTW: integer := 32;

  -- radix' supported
  type ScanRadix_t is (SCAN_BIN, SCAN_DEC, SCAN_HEX, SCAN_HEXBYTES, SCAN_HEXSHORT);

  -- symbols recognized by scanner
  type ScanSym_t is (

    -- operator precedence as in 'C':
    -- highest priority
    SCAN_LPAR,  -- (
    SCAN_RPAR,  -- )

    SCAN_NOT,   -- !
    SCAN_INV,   -- ~
--    SCAN_PLUS,  -- unary +
--    SCAN_MINUS,  -- unary -

    SCAN_MUL,   -- *
    SCAN_DIV,   -- /

    SCAN_PLUS,  -- +
    SCAN_MINUS, -- -

    SCAN_MOD,   -- %

    SCAN_SHL,   -- <<
    SCAN_SHR,   -- >>

    SCAN_LT,    -- <
    SCAN_LE,    -- <=
    SCAN_GT,    -- >
    SCAN_GE,    -- >=

    SCAN_EQ,    -- =
    SCAN_NE,    -- /=

    SCAN_AND,   -- &

    SCAN_XOR,   -- ^

    SCAN_OR,    -- |

    SCAN_LOGAND, -- &&

    SCAN_LOGOR, -- ||
    -- lowest priority


    SCAN_COL,   -- :  not supported
    SCAN_COLEQ, -- :=  not supported
    SCAN_PWR,   -- **  not supported

    SCAN_LF,    -- newline
    SCAN_EOF,   -- end of file
    SCAN_NUM,   -- number
    SCAN_ID,    -- identifier
    SCAN_STR,   -- string enclosed by single or double quotes
    SCAN_KEYW,  -- keyword
    SCAN_BAD    -- illegal input
    );

  subtype ScanStr_t is string(1 to 257);
  subtype ScanVect_t is std_logic_vector(0 to 255);

  subtype IncLineNums_t is IntegerArray_t(0 to 4*MAX_INC_FILES);

  type PrgState_t is (
    PRG_IDLE,
    PRG_LOOP,
    PRG_TRUE,
    PRG_FALSE
    );

  -- this record holds position in the char buffer, current linenumber,
  -- linenumber prior to last call of GetSym and the lookahead character
  -- and some program control check info
  type PState_t is
    record
      Pos      : integer;
      LNum     : integer;
      LNumPrev : integer;
      C        : character;
      PrgState : PrgState_t;
    end record;

  type PStateArray_t is array(natural range<>) of PState_t;

  subtype Char_t is integer range 0 to 255;
  type CharArray_t is array(natural range<>) of Char_t;

  type TxtScanState_t is
    record
      Radix     : ScanRadix_t;
      Sym       : ScanSym_t;
      Vect      : ScanVect_t;
      Str       : ScanStr_t;
      Attr      : integer;
      AttrIx    : integer;
      SymTab    : CharArray_t(1 to SYMSPACE);
      SymAttrTab  : IntegerArray_t(1 to MAX_SYM);
      SymNextFree: integer;
      AttrNextFree: integer;
      MaxKeywIx : integer;
      FBuf      : string(1 to MAX_FILESZ);
      FSize     : integer;
      LineNums  : IncLineNums_t;
      PState    : PState_t;
      PStack    : PStateArray_t(0 to MAX_NEST);
      PStateSP  : integer;
    end record;

  -- Random number generate from 0 to 999999999 based on current time (now).
  -- Use optional seed_mod to make different random numbers at the same time.
  impure function rnd(seed_mod : natural := 0) return natural;

  -- Random number generate for >= min and <= max
  -- Based on rnd, thus use optional seed_mod to make different random numbers at the same time.
  impure function rnd(minval : natural; maxval : natural; seed_mod : natural := 0) return natural;

  function ConvInteger(inp: std_logic_vector) return integer;
--  function ConvInteger(inp: unsigned) return integer;
  function IntegerMod(L,C: in integer) return integer;
  function IntegerShl(L,C: in integer) return integer;
  function IntegerShr(L,C: integer) return integer;
  function IntegerAnd(L,R: integer) return integer;
  function IntegerXor(L,R: integer) return integer;
  function IntegerOr(L,R: integer) return integer;

  function to_str(inp : std_logic_vector; radix: ScanRadix_t := SCAN_BIN) return string;
  function to_str(inp : std_logic) return string;
--  function to_str(inp : unsigned; radix: ScanRadix_t := SCAN_BIN) return string;
  function to_str(inp : integer; radix: ScanRadix_t := SCAN_DEC) return string;
  function to_str(inp : boolean) return string;
  function to_str(inp : string) return string;

  --------------------------------------------------------
  -- Taken from IO_UTILS
  --------------------------------------------------------
  procedure WriteString(
    L    : inout line;
    Str    : in string;
    Just  : in side := right;
    Field  : in width := 0
    );


  procedure GetNextSym(S: inout TxtScanState_t);
  procedure Eval(S: inout TxtScanState_t; Res: inout integer);
  procedure SetScriptVar(S: inout TxtScanState_t; SetValue: in integer);
  procedure InitTxtScan(constant ScriptDir: in string; constant FName: in string; constant KeywordNames: in string; S: inout TxtScanState_t);
  procedure GetNextCmd(S: inout TxtScanState_t; Cmd: out integer);
  function GetLineNum(S: TxtScanState_t) return integer;
  function GetLineNumPrev(S: TxtScanState_t) return integer;

  constant EOF : character := character'val(26);
  constant MAX_LINE : integer := 500;
  constant MAX_DIGITS : integer := 20;

end package tb_parse_p;



package body tb_parse_p is

  constant KW_EOF     : integer := 0;
  constant KW_WHILE   : integer := 1;
  constant KW_LOOP    : integer := 2;
  constant KW_ENDLOOP : integer := 3;
  constant KW_IF      : integer := 4;
  constant KW_THEN    : integer := 5;
  constant KW_ELSE    : integer := 6;
  constant KW_ENDIF   : integer := 7;

  constant UP_LO      : integer := character'pos('A') - character'pos('a');
  constant to_hex     : string := "0123456789ABCDEF";

  -- Random number generate from 0 to 999999999
  impure function rnd(seed_mod : natural := 0) return natural is
    variable val : integer;
  begin
    val := (now / 1 ps) + seed_mod;
    for i in 1 to 3 loop
      val := ((val mod 100003) * 12347 + 1009) mod 1000000000;
    end loop;
    return val;
  end function;


  -- Random number generate for >= min and <= max
  impure function rnd(minval : natural; maxval : natural; seed_mod : natural := 0) return natural is
  begin
    return (rnd(seed_mod) mod (maxval - minval + 1)) + minval;
  end function;

  -- under Synopsys, the conv_integer function (in the IEEE.std_logic_arith package) can't convert
  -- 32 bit vectors properly.
  -- This hack attempts to circumvents it
  function ConvInteger(inp: std_logic_vector) return integer is
    alias vec : std_logic_vector(inp'length-1 downto 0) is inp;
    variable msbs : std_logic_vector(1 downto 0);
    constant I40 : integer := 16#40000000#;
    constant I80 : integer := integer'low;
    constant IC0 : integer := I80 + I40;
  begin
    if inp'length < INTW then
      return to_integer(signed(inp));
    end if;
    msbs := vec(INTW-1 downto INTW-2);
    case msbs is
      when "01" =>
        return I40 + to_integer(signed('0' & vec(INTW-3 downto 0)));
      when "10" =>
        return I80 + to_integer(signed('0' & vec(INTW-3 downto 0)));
      when "11" =>
        return IC0 + to_integer(signed('0' & vec(INTW-3 downto 0)));
      when others =>
        return to_integer(signed('0' & vec(INTW-3 downto 0)));
    end case;
  end;

  -- same for unsigned
--  function ConvInteger(inp: unsigned) return integer is
--  begin
--    return ConvInteger(std_logic_vector(inp));
--  end;

  function IntegerMod(L,C: in integer) return integer is
    variable V: std_logic_vector((2*INTW)-1 downto 0);
  begin
    assert C > 0
      report "Non-positive modulus not allowed with '%'"
      severity SYNTAXERR;
    return L mod C;
  end;

  function IntegerShl(L,C: in integer) return integer is
    variable V: std_logic_vector((2*INTW)-1 downto 0);
  begin
    assert C >= 0
      report "Negative shift count not allowed with '<<'"
      severity SYNTAXERR;
    V((2*INTW)-1 downto INTW) := std_logic_vector(to_signed(L,INTW));
    V(INTW-1 downto  0) := (others => '0');
    if C < INTW then
      return ConvInteger(V((2*INTW)-1-C downto INTW-C));
    else
      return 0;
    end if;
  end;

  function IntegerShr(L,C: integer) return integer is
    variable V: std_logic_vector((2*INTW)-1 downto 0);
  begin
    assert C >= 0
      report "Negative shift count not allowed with '>>'"
      severity SYNTAXERR;
    V((2*INTW)-1 downto INTW) := (others => '0');
    V(INTW-1 downto  0) := std_logic_vector(to_signed(L,INTW));
    if C < INTW then
      return ConvInteger(V(INTW-1+C downto C));
    else
      return 0;
    end if;
  end;

  function IntegerAnd(L,R: integer) return integer is
  begin
    return ConvInteger(std_logic_vector(to_signed(L,INTW)) and std_logic_vector(to_signed(R,INTW)));
  end;

  function IntegerXor(L,R: integer) return integer is
  begin
    return ConvInteger(std_logic_vector(to_signed(L,INTW)) xor std_logic_vector(to_signed(R,INTW)));
  end;

  function IntegerOr(L,R: integer) return integer is
  begin
    return ConvInteger(std_logic_vector(to_signed(L,INTW)) or std_logic_vector(to_signed(R,INTW)));
  end;

  --
  -- convert a std_logic value to a character
  --
  type stdlogic_to_char_t is array(std_logic) of character;
  constant to_char : stdlogic_to_char_t := (
    'U' => 'U',
    'X' => 'X',
    '0' => '0',
    '1' => '1',
    'Z' => 'Z',
    'W' => 'W',
    'L' => 'L',
    'H' => 'H',
    '-' => '-');

  --
  -- convert a std_logic_vector to a string
  --
  function to_str(inp : std_logic_vector; radix: ScanRadix_t := SCAN_BIN) return string is
    alias vec : std_logic_vector(inp'length downto 1) is inp;
    variable res  : string(vec'range);
    variable res4 : string((inp'length+3)/4 downto 1);
    variable vec4 : std_logic_vector(4*((inp'length+3)/4) downto 1);
    variable tmp  : integer;
    variable pos  : integer;
    variable cnt  : integer;
    variable firstdigit :boolean;

  begin
    -- radix =  SCAN_HEXSHORT: avoid insertion of leading zeros
    if radix = SCAN_HEX or radix = SCAN_HEXSHORT then
      if radix = SCAN_HEXSHORT then
        firstdigit := false;
      else
        firstdigit := true;
      end if;
      vec4 := (others => '0');
      vec4(vec'range) := vec;
      pos := res4'left;
      cnt := 1;
      for i in res4'range loop
        if i = res4'right then
          firstdigit := true;           -- as a miniumum print the last zero nibble when inp=0
        end if;
        if Is_X(vec4(i*4 downto i*4-3)) then
          firstdigit := true;
          res4(pos) := '?';
          cnt := cnt + 1;
          pos := pos - 1;
        else
          if (firstdigit or (vec4(i*4 downto i*4-3) /= "0000")) then
            firstdigit := true;
            res4(pos) := to_hex(1+to_integer(signed(vec4(i*4 downto i*4-3))));
            cnt := cnt + 1;
            pos := pos - 1;
          end if;
        end if;
      end loop;
      return res4(res4'left downto pos+1);
    else
      for i in res'range loop
        res(i) := to_char(vec(i));
      end loop;
      return res;
    end if;
  end;

--  -- same for unsigned
--  function to_str(inp : unsigned; radix: ScanRadix_t := SCAN_BIN) return string is
--  begin
--    return to_str(std_logic_vector(inp),radix);
--  end;

  -- same for integer (paste from TEXTIO).
  function to_str(inp : integer; radix: ScanRadix_t := SCAN_DEC) return string is
    variable result  : string(MAX_DIGITS downto 1);
    variable pos  : integer := 1;
    variable tmp  : integer;
    variable digit  : integer;
  begin
    if radix = SCAN_DEC then
      tmp := abs(inp);
      loop
        digit := abs(tmp MOD 10);    -- MOD of integer'left returns neg number!
        tmp := tmp / 10;
        result(pos) := character'val(character'pos('0') + digit);
        pos := pos + 1;
        exit when tmp = 0;
      end loop;
      if inp < 0 then
        result(pos) := '-';
        pos := pos + 1;
      end if;
    elsif radix = SCAN_HEX then
      tmp := inp;
      loop
        digit := IntegerAnd(tmp,15);
        tmp := IntegerShr(tmp,4);
        if digit <= 9 then
          result(pos) := character'val(character'pos('0') + digit);
        else
          result(pos) := character'val(character'pos('A') + digit-10);
        end if;
        pos := pos + 1;
        exit when tmp = 0;
      end loop;
    end if;
    return result(pos-1 downto 1);
  end;

  -- convert a std_logic to a string
  function to_str(inp : std_logic) return string is
    variable vector : std_logic_vector(0 downto 0);
  begin
    vector(0) := inp;
    return to_str(vector);
  end function;

  function to_str(inp : boolean) return string is
  begin
    if (inp) then
      return "TRUE";
    else
      return "FALSE";
    end if;
  end function;

  function to_str(inp : string) return string is
    variable L: integer;
  begin
    L := character'pos(inp(inp'low));
    if L > 0 then
      return inp(inp'low+1 to inp'low+L);
    else
      return "";
    end if;
  end;


  procedure WriteString(
    L    : inout line;
    Str    : in string;
    Just  : in side := right;
    Field  : in width := 0
    ) is
  begin
    write(L, Str, Just, Field);
  end;


  function ToUpper(C : character) return character is
  begin
    if 'a' <= C and C <= 'z' then
      return character'val(character'pos(C) + UP_LO);
    else
      return C;
    end if;
  end;

  procedure ReadFile(constant ScriptDir: in string; constant FName: in string; FileBuffer: out string; Length: out integer; LineNums: inout IncLineNums_t) is
    constant INCLUDE_STR : string := "INCLUDE";
    variable i           : integer;
    variable Tmp         : line;
    variable has_include : boolean;
    variable inc_fnam_ix : integer;
    variable inc_len     : integer;
    variable LineNumRel  : integer;
    variable LineNumDiff : integer;
    variable LineNumIx   : integer;
    file     InFile      : text is FName;
  begin
    i := FileBuffer'low;
    LineNumRel  := 0;
    LineNumDiff := 0;
    while not EndFile(InFile) loop
      ReadLine(InFile, Tmp);
      has_include := FALSE;
      for j in Tmp'range loop
        -- Look for "include whitespace filename"
        has_include := INCLUDE_STR(j) = ToUpper(Tmp(j));
        exit when j = INCLUDE_STR'high or not has_include;
      end loop;
      if has_include and Tmp'high > INCLUDE_STR'high then
        for j in INCLUDE_STR'high + 1 to Tmp'high loop
          inc_fnam_ix := j;
          exit when Tmp(j) /= ' ' and Tmp(j) /= HT;
        end loop;
        if inc_fnam_ix > INCLUDE_STR'high + 1 and inc_fnam_ix < Tmp'high then
          -- Found "include whitespace"
          -- Read the file into our buffer instead of the line with the include directive
          LineNumIx := LineNums(LineNums'low) + 1;
          LineNums(LineNumIx) := LineNumRel;
          LineNums(LineNumIx + 1) := LineNumDiff;
          LineNums(LineNums'low) := LineNumIx + 1;
          ReadFile(ScriptDir, ScriptDir & Tmp(inc_fnam_ix to Tmp'high), FileBuffer(i to FileBuffer'high), inc_len, LineNums);
          LineNumDiff := 0;
          i := i + inc_len;
        end if;
      else
        -- Copy line into buffer
        for j in Tmp'range loop
          FileBuffer(i) := Tmp(j);
          i := i + 1;
        end loop;
        FileBuffer(i) := LF;
        i := i + 1;
        LineNumDiff := LineNumDiff + 1;
      end if;
      LineNumRel := LineNumRel + 1;
    end loop;
    Length := i - FileBuffer'low;
    LineNumIx := LineNums(LineNums'low) + 1;
    LineNums(LineNumIx) := LineNumRel;
    LineNums(LineNumIx + 1) := -LineNumDiff;
    LineNums(LineNums'low) := LineNumIx + 1;
  end;

  function GetLineNumInt(AbsLineNum : integer; LineNums: IncLineNums_t) return integer is
    variable CmpLineNum : integer;
    variable ix         : integer;
  begin
    -- Convert to relative linenumber in file
    CmpLineNum := 0;
    ix := 1;
    while ix < LineNums(0) loop
      CmpLineNum := CmpLineNum + abs(LineNums(ix+1));
      if CmpLineNum >= AbsLineNum then
        -- Found interval.
        -- Calc backwards from relative line at end of interval
        return LineNums(ix) - (CmpLineNum - AbsLineNum);
      end if;
      ix := ix + 2;
    end loop;
    return 12345678; -- panic ??
  end;

  function GetLineNum(S: TxtScanState_t) return integer is
  begin
    return GetLineNumInt(S.PState.LNum, S.LineNums); -- absolute linenumber to locate
  end;

  function GetLineNumPrev(S: TxtScanState_t) return integer is
  begin
    return GetLineNumInt(S.PState.LNumPrev, S.LineNums); -- absolute linenumber to locate
  end;

  procedure GetChar(
    FBuf    : in string;
    PState  : inout PState_t
    ) is
  begin
    if PState.C /= EOF then
      PState.C := FBuf(PState.Pos);
      PState.Pos := PState.Pos + 1;
    end if;
  end;

--  procedure CopyStr(signal Dest: out string;  Src: in string) is
--      variable Len: integer;
--    begin
--       if Dest'length > Src'length then
--          Len := Src'length;
--            Dest(Len + 1) <= nul;    -- zero terminate if room
--        else
--          Len := Dest'length;
--      end if;
--      Dest(1 to Len) <= Src(1 to Len);
--    end;

  procedure GetSym(
    FBuf  : in string;
    B    : inout PState_t;
    Radix  : ScanRadix_t;    -- default radix. User may override by prepending
    -- %(binary), #(decimal) or $(hexadecimal).
    -- When the default radix is 16, anything starting with
    -- 0..9,A..F,a..f will be interpreted as a number.
    Sym    : out ScanSym_t;
    Vect  : out ScanVect_t;
    Str    : out ScanStr_t;
    Attr  : out integer    -- hash value if identifier, # bits needed to represent
    -- number when number.
    ) is


    variable C0: character;
    variable MySym: ScanSym_t;

    procedure GetC is
    begin
      if B.C /= EOF then
        B.C := FBuf(B.Pos);
        B.Pos := B.Pos + 1;
      end if;
    end;

    procedure SkipWhite is
    begin
      while B.C = ' ' or B.C = HT or B.C = CR loop
        B.C := FBuf(B.Pos);
        B.Pos := B.Pos + 1;
      end loop;

      -- comments may begin with '--','//' or ';'
      case B.C is
        when '-' | '/' =>
          if FBuf(B.Pos) = B.C then
            -- skip comment
            while FBuf(B.Pos) /= LF and FBuf(B.Pos) /= EOF loop
              B.Pos := B.Pos + 1;
            end loop;
            B.C := FBuf(B.Pos);
            B.Pos := B.Pos + 1;
          end if;
          when ';' =>
            -- skip comment
          while FBuf(B.Pos) /= LF and FBuf(B.Pos) /= EOF loop
            B.Pos := B.Pos + 1;
          end loop;
          B.C := FBuf(B.Pos);
          B.Pos := B.Pos + 1;
        when others =>
          null;
      end case;
    end;

    --
    --  GetBin supports all std_logic values.
    --  The width returned is the number of characters
    --
    procedure GetBin is
      variable i: integer;
    begin
      i := 0;
      loop
        case B.C is
          when 'U'|'u' =>
            Vect(i) := 'U';
          when 'X'|'x' =>
            Vect(i) := 'X';
          when '0' =>
            Vect(i) := '0';
          when '1' =>
            Vect(i) := '1';
          when 'Z'|'z' =>
            Vect(i) := 'Z';
          when 'W'|'w' =>
            Vect(i) := 'W';
          when 'L'|'l' =>
            Vect(i) := 'L';
          when 'H'|'h' =>
            Vect(i) := 'H';
          when '-' =>
            Vect(i) := '-';
          when others =>
            exit;
        end case;
        i := i + 1;
        GetC;
      end loop;

      if i /= 0 then
        MySym := SCAN_NUM;
        Attr := i;
      else
        MySym := SCAN_BAD;
      end if;
    end;

    --  The width returned is the number of bits required to hold the number
    procedure GetDec is
      variable i,tmp: integer;
    begin
      tmp := 0;
      i := 0;
      while '0' <= B.C and B.C <= '9' loop
        i := i + 1;
        tmp := (tmp * 10) + character'pos(B.C) - character'pos('0');
        GetC;
      end loop;

      if i /= 0 then
        -- determine # bits needed to represent number and insert it in vector
        -- left justified
        for j in 1 to 31 loop
          if 2**(j-1) > tmp then -- Use (j-1) to make sure integer is treated as unsigned.
            Attr := j;
            Vect(0 to j-1) := std_logic_vector(to_signed(tmp,j));
            exit;
          end if;
        end loop;
        MySym := SCAN_NUM;
      else
        MySym := SCAN_BAD;
      end if;
    end;

    --  The width returned is the number of bits required to hold the number
    procedure GetHex is
      variable i,j,tmp: integer;
      variable V: ScanVect_t;
    begin
      i := 0;
      j := 0;
      loop
        case B.C is
          when '0' to '9' =>
            tmp := character'pos(B.C) - character'pos('0');

          when 'a' to 'f' =>
            tmp := character'pos(B.C) - character'pos('a') + 10;

          when 'A' to 'F' =>
            tmp := character'pos(B.C) - character'pos('A') + 10;

          when others =>
            if i = 0 and j > 0 then
              V(0) := '0';
              i := 1;
            end if;
            exit;

        end case;

        if i = 0 then
          -- first non-zero digit determines fractional width
          if tmp >= 8 then
            i := 4;
          elsif tmp >= 4 then
            i := 3;
          elsif tmp >= 2 then
            i := 2;
          elsif tmp >= 1 then
            i := 1;
          end if;
          if i > 0 then
            V(0 to i-1) := std_logic_vector(to_unsigned(tmp,i));
          end if;
        else
          V(i to i+3) := std_logic_vector(to_unsigned(tmp,4));
          i := i + 4;
        end if;
        GetC;
        j := j + 1;
      end loop;

      if i /= 0 then
        if Radix = SCAN_HEXBYTES then
          -- return full bytes: 0FFFF => 16bits, 123 => 16 bits, 000003 => 24 bits
          j := j/2 * 8;  -- j # bits in full bytes specified
          if j < i then
            j := j+8;
          end if;
          -- j has #bits to return
          if j > i then
            V(j-i to j-1) := V(0 to i-1);    -- right justify bits
            V(0 to j-i-1) := (others => '0');  -- pad on left with zero
            i := j;
          end if;
        end if;
        MySym := SCAN_NUM;
        Attr := i;
        Vect(0 to i-1) := V(0 to i-1);
      else
        MySym := SCAN_BAD;
      end if;
    end;

    procedure GetStr is
      variable C0_v: character;
      variable i: integer;
    begin
      C0_v := B.C;
      GetC;
      i := 0;
      while B.C /= C0_v loop      -- self defining delimiter
        if B.C = LF then
          MySym := SCAN_BAD;
          return;
        end if;
        Str(i+2) := B.C;
        i := i + 1;
        GetC;
      end loop;
      Str(1) := character'val(i);
      Attr := i;          -- # chars here also
      MySym := SCAN_STR;
    end;

    procedure GetId is
      variable i,hash: integer;
    begin
      i := 1;
      B.C := ToUpper(B.C);
      Str(2) := B.C;
      hash := character'pos(B.C);
      loop
        GetC;
        case B.C is
          when 'a' to 'z' =>
            B.C := character'val(character'pos(B.C) + UP_LO);
          when '0' to '9' | 'A' to 'Z' | '_' =>
            null;
          when others =>
            exit;
        end case;
        Str(i+2) := B.C;
        i := i + 1;
        hash := (((hash * 8 + hash / 32) mod 256) + character'pos(B.C)) mod 256; -- rotate left 3
      end loop;
      Str(1) := character'val(i);
      Attr := hash;
      MySym := SCAN_ID;
    end;

  begin
    -- C contains the first char following the previous symbol
    B.LNumPrev := B.LNum;
    SkipWhite;
    while B.C = LF loop
      B.LNum := B.LNum + 1;
      GetC;
      SkipWhite;
    end loop;

    case B.C is
      when '+' =>
        MySym := SCAN_PLUS;

      when '-' =>
        MySym := SCAN_MINUS;

      when '=' =>
        MySym := SCAN_EQ;

      when '^' =>
        MySym := SCAN_XOR;

      when '!' =>
        MySym := SCAN_NOT;

      when '~' =>
        MySym := SCAN_INV;

      when '(' =>
        MySym := SCAN_LPAR;

      when ')' =>
        MySym := SCAN_RPAR;

      when '%' =>
        MySym := SCAN_MOD;

  --  when LF =>      -- can't happen
  --    MySym := SCAN_LF;

      when EOF =>
        MySym := SCAN_EOF;

      when '<' | '>' | '*' | '/' | ':' | '&' | '|' =>
        -- symbol known from second char
        C0 := B.C;
        GetC;
        case C0 is

          when '<' =>
            if B.C = '<' then
              MySym := SCAN_SHL;
            elsif B.C = '=' then
              MySym := SCAN_LE;
            else
              MySym := SCAN_LT;
            end if;

          when '>' =>
            if B.C = '>' then
              MySym := SCAN_SHR;
            elsif B.C = '=' then
              MySym := SCAN_GE;
            else
              MySym := SCAN_GT;
            end if;

          when '*' =>
            if B.C = '*' then
              MySym := SCAN_PWR;
            else
              MySym := SCAN_MUL;
            end if;

          when '/' =>
            if B.C = '=' then
              MySym := SCAN_NE;
            else
              MySym := SCAN_DIV;
            end if;

          when ':' =>
            if B.C = '=' then
              MySym := SCAN_COLEQ;
            else
              MySym := SCAN_COL;
            end if;

          when '&' =>
            if B.C = '&' then
              MySym := SCAN_LOGAND;
            else
              MySym := SCAN_AND;
            end if;

          when '|' =>
            if B.C = '|' then
              MySym := SCAN_LOGOR;
            else
              MySym := SCAN_OR;
            end if;

          when others =>
            null;

        end case;

      when '#' =>
        -- decimal number
        GetC;
        GetDec;

      when '$' =>
        -- hex number
        GetC;
        GetHex;

      when 'g' to 'z' | 'G' to 'Z' | '_' =>
        -- identifier
        GetId;

      when 'a' to 'f' | 'A' to 'F' =>
        GetId;    -- always Id
--        -- number if default radix is hex else identifier
--        if Radix = SCAN_HEX then
--          GetHex;
--        else
--          GetId;
--        end if;

      when '0' to '9' =>
        -- number
        if Radix = SCAN_BIN then
          GetBin;
        elsif Radix = SCAN_DEC then
          GetDec;
        else
          GetHex;
        end if;

      when ''' | '"' =>
        -- string
        GetStr;

      when others =>
        MySym := SCAN_BAD;

    end case;

    -- read first character following symbol when not already done
    case MySym is
      when SCAN_LT  | SCAN_GT  | SCAN_MUL | SCAN_DIV | SCAN_COL |
         SCAN_EOF | SCAN_NUM | SCAN_ID  | SCAN_AND | SCAN_OR =>
        null;
      when others =>
        GetC;
    end case;

    Sym := MySym;
  end GetSym;


  --------------------------------------------------------
  --  Simple symbol table support routines.
  --
  --  The symbol table consists of variable length entries inserted into a (large)
  --  character array.
  --  The identifer 'NAM' with some integer attribute value is stored as:
  --
  --  Hash,Length(=3),'N','A','M',AttrIxLo,AttrIxHi
  --
  --  where AttrIx selects en entry in an integer array with the attribute value.
  --------------------------------------------------------
  function GetAttrIx(
        SymTab  : CharArray_t;
        Name  : string;
        Hash  : integer;
        SymNextFree: integer
        ) return integer is
    variable i,j,l: integer;
  begin
    -- Sequential search through array
    i := SymTab'low;
    while i < SymNextFree loop
      l := SymTab(i+1);
      if Hash = SymTab(i) then
        j := 0;
        while j <= l loop
          exit when Name(Name'low+j) /= character'val(SymTab(i+1+j));
          j := j + 1;
        end loop;
        if j > l then
          -- found
          return SymTab(i+2+l) + (256 * SymTab(i+3+l));
        end if;
      end if;
            -- not found here, move on to next entry if any
      i := i + l + 4;
    end loop;
    return 0;
  end;

  procedure AddNewName(
        SymTab      : inout CharArray_t;
        Name        : in string;
        Hash        : in integer;
        SymNextFree : in integer;
        AttrNextFree: in integer;
        SymNextFreeNew: out integer
        ) is
    variable i,l: integer;
  begin
    i := SymNextFree;
    SymTab(i) := Hash;
    l := character'pos(Name(Name'low));
    for j in 0 to l loop
      SymTab(i+1+j) := character'pos(Name(Name'low + j));
    end loop;
    SymTab(i+2+l) := AttrNextFree mod 256;
    SymTab(i+3+l) := AttrNextFree / 256;
    SymNextFreeNew := i+4+l;
  end;

  procedure GetExtSym (
      FBuf    : in string;
      B       : inout PState_t;
      Radix   : ScanRadix_t;
      Sym     : inout ScanSym_t;
      Vect    : out ScanVect_t;
      Str     : inout ScanStr_t;
      Attr    : inout integer;
      AttrIx  : inout integer;
      SymTab  : in CharArray_t;
      MaxKeywIx  : in integer;
      SymNextFree  : in integer
      ) is
    variable i: integer;
  begin
    GetSym(FBuf,B,Radix,Sym,Vect,Str,Attr);
    if Sym = SCAN_ID then
      -- see if keyword
      AttrIx := GetAttrIx(SymTab,Str,Attr,SymNextFree);
      if AttrIx > 0 and AttrIx <= MaxKeywIx then
        Sym := SCAN_KEYW;
      end if;
    end if;
  end GetExtSym;

  procedure EvalExpr(
      FBuf    : in string;
      LineNums : in IncLineNums_t;
      PState  : inout PState_t;
      Radix    : ScanRadix_t;
      Sym      : inout ScanSym_t;
      Vect    : inout ScanVect_t;
      Str      : inout ScanStr_t;
      Attr    : inout integer;
      AttrIx  : inout integer;
      SymTab  : in CharArray_t;
      MaxKeywIx  : in integer;
      SymNextFree  : in integer;
      SymAttrTab  : in IntegerArray_t;
      Result  : inout integer
      ) is

    -- Functions and procedures private to EvalExpr

    procedure EvalExp(Res: inout integer);  -- forward declaration

    procedure GetS is
    begin
      GetExtSym(FBuf,PState,Radix,Sym,Vect,Str,Attr,AttrIx,SymTab,MaxKeywIx,SymNextFree);
    end;

    procedure EvalFactor(Res: inout integer) is
      variable Sym0: ScanSym_t;
    begin
      case Sym is
        when SCAN_NOT | SCAN_INV | SCAN_PLUS | SCAN_MINUS =>
          Sym0 := Sym;
          GetS;
          EvalFactor(Res);
          case Sym0 is
            when SCAN_NOT =>
              if Res = 0 then
                 Res := 1;
              else
                 Res := 0;
              end if;
            when SCAN_INV =>
              Res := -1-Res;
            when SCAN_PLUS =>
              null;
            when SCAN_MINUS =>
              Res := -Res;
            when others =>
              null;
          end case;

        when SCAN_LPAR =>
          GetS;
          EvalExp(Res);
          if Sym = SCAN_RPAR then
            GetS;
          else
            assert false report
              ErrHead & "Missing ')' at line " & to_str(GetLineNumInt(PState.LNum, LineNums))
              severity SYNTAXERR;
          end if;

        when SCAN_NUM =>
          if Attr <= INTW then
            Res := ConvInteger(Vect(0 to Attr-1));
          else
            Res := ConvInteger(Vect(0 to INTW-1));    -- truncate
          end if;
          GetS;

        when SCAN_ID =>
          -- name must be known
          if AttrIx /= 0 then
            Res := SymAttrTab(AttrIx);
          else
            assert false report
              ErrHead & "Identifier '" & to_str(Str) & "' unknown at line " & to_str(GetLineNumInt(PState.LNum, LineNums))
              severity SYNTAXERR;
          end if;
          GetS;

        when others =>
          assert false report
            ErrHead & "Syntax error at line " & to_str(GetLineNumInt(PState.LNum, LineNums))
            severity SYNTAXERR;
      end case;
    end;

    procedure EvalTerm(Res: inout integer) is
      variable Tmp: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalFactor(Res);
      while Sym = SCAN_MUL or Sym = SCAN_DIV loop
        Sym0 := Sym;
        GetS;
        EvalFactor(Tmp);
        if Sym0 = SCAN_MUL then
          Res := Res * Tmp;
        else
          Res := Res / Tmp;
        end if;
      end loop;
    end;

    procedure EvalShiftOp(Res: inout integer) is
      variable Tmp,High,HighBit: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalTerm(Res);
      while Sym = SCAN_PLUS or Sym = SCAN_MINUS loop
        Sym0 := Sym;
        GetS;
        EvalTerm(Tmp);
        HighBit := IntegerShl(1,31);
        if Sym0 = SCAN_PLUS then
          High := IntegerXor(IntegerAnd(IntegerXor(Res,Tmp),HighBit),HighBit);
          Res := IntegerXor(Res + IntegerXor(Tmp,High),High);
        else
          High := IntegerAnd(IntegerXor(Res,Tmp),HighBit);
          Res := IntegerXor(Res - IntegerXor(Tmp,High),High);
        end if;
      end loop;
    end;

    procedure EvalModOp(Res: inout integer) is
      variable Tmp: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalShiftOp(Res);
      while Sym = SCAN_MOD loop
        Sym0 := Sym;
        GetS;
        EvalShiftOp(Tmp);
        Res := IntegerMod(Res,Tmp);
      end loop;
    end;

    procedure EvalCmpOp(Res: inout integer) is
      variable Tmp: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalModOp(Res);
      while Sym = SCAN_SHL or Sym = SCAN_SHR loop
        Sym0 := Sym;
        GetS;
        EvalModOp(Tmp);
        if Sym0 = SCAN_SHL then
          Res := IntegerShl(Res,Tmp);
        else
          Res := IntegerShr(Res,Tmp);
        end if;
      end loop;
    end;

    procedure EvalEqOp(Res: inout integer) is
      variable Tmp: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalCmpOp(Res);
      while Sym = SCAN_LT or Sym = SCAN_LE or Sym = SCAN_GT or Sym = SCAN_GE loop
        Sym0 := Sym;
        GetS;
        EvalCmpOp(Tmp);
        if (Sym0 = SCAN_LT and Res < Tmp) or
          (Sym0 = SCAN_LE and Res <= Tmp) or
          (Sym0 = SCAN_GT and Res > Tmp) or
          (Sym0 = SCAN_GE and Res >= Tmp) then
          Res := 1;
        else
          Res := 0;
        end if;
      end loop;
    end;

    procedure EvalAndOp(Res: inout integer) is
      variable Tmp: integer;
      variable Sym0: ScanSym_t;
    begin
      EvalEqOp(Res);
          while Sym = SCAN_EQ or Sym = SCAN_NE loop
              Sym0 := Sym;
        GetS;
        EvalEqOp(Tmp);
              if (Sym0 = SCAN_EQ and Res = Tmp) or
           (Sym0 = SCAN_NE and Res /= Tmp) then
          Res := 1;
              else
          Res := 0;
              end if;
          end loop;
    end;

    procedure EvalXorOp(Res: inout integer) is
      variable Tmp: integer;
    begin
      EvalAndOp(Res);
      while Sym = SCAN_AND loop
        GetS;
        EvalAndOp(Tmp);
        Res := IntegerAnd(Res,Tmp);
      end loop;
    end;

    procedure EvalOrOp(Res: inout integer) is
      variable Tmp: integer;
    begin
      EvalXorOp(Res);
      while Sym = SCAN_XOR loop
        GetS;
        EvalXorOp(Tmp);
        Res := IntegerXor(Res,Tmp);
      end loop;
    end;

    procedure EvalLogAndOp(Res: inout integer) is
      variable Tmp: integer;
    begin
      EvalOrOp(Res);
      while Sym = SCAN_OR loop
        GetS;
        EvalOrOp(Tmp);
        Res := IntegerOr(Res,Tmp);
      end loop;
    end;

    procedure EvalLogOrOp(Res: inout integer) is
      variable Tmp: integer;
    begin
      EvalLogAndOp(Res);
      while Sym = SCAN_LOGAND loop
        GetS;
        EvalLogAndOp(Tmp);
        if Res /= 0 and Tmp /= 0 then
          Res := 1;
        else
          Res := 0;
        end if;
      end loop;
    end;

    procedure EvalExp(Res: inout integer) is
      variable Tmp: integer;
    begin
      EvalLogOrOp(Res);
      while Sym = SCAN_LOGOR loop
        GetS;
        EvalLogOrOp(Tmp);
        if Res /= 0 or Tmp /= 0 then
          Res := 1;
        else
          Res := 0;
        end if;
      end loop;
    end;

  begin  -- body of EvalExpr
    EvalExp(Result);
  end EvalExpr;


  procedure GetNextSym(S: inout TxtScanState_t) is
  begin
    GetExtSym(S.FBuf,S.PState,S.Radix,S.Sym,S.Vect,S.Str,S.Attr,S.AttrIx,S.SymTab,S.MaxKeywIx,S.SymNextFree);
  end;


  procedure Eval(S: inout TxtScanState_t; Res: inout integer) is
  begin
    EvalExpr(S.FBuf,S.LineNums,S.PState,S.Radix,S.Sym,S.Vect,S.Str,S.Attr,S.AttrIx,S.SymTab,S.MaxKeywIx,S.SymNextFree,S.SymAttrTab,Res);
  end;

  -- Return value back to script interpreter. The name of the variable is expected
  procedure SetScriptVar(S: inout TxtScanState_t; SetValue: in integer) is
  begin
    if S.Sym = SCAN_ID then
      -- Got identifier. It must be known
      if S.AttrIx /= 0 then
        S.SymAttrTab(S.AttrIx) := SetValue;
        GetNextSym(S);
      else
        assert false report
          ErrHead & "Identifier for SetScriptVar: '" & to_str(S.Str) & "' unknown at line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
          severity SYNTAXERR;
      end if;
    else
      assert false report
        -- Must have a name
        ErrHead & "Identifier expected by SetScriptVar at line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
        severity SYNTAXERR;
    end if;
  end;

  procedure InitTxtScan(constant ScriptDir: in string; constant FName: in string; constant KeywordNames: in string; S: inout TxtScanState_t) is
  begin
    S.SymNextFree := S.SymTab'low;
    S.AttrNextFree := S.SymAttrTab'low;
    S.PStateSP := S.PStack'low;
    S.LineNums := (others => 0);

    -- Read in keywords
    S.FBuf(1 to KeywordNames'length) := KeywordNames;
    S.FBuf(KeywordNames'length + 1) := EOF;

    S.PState.Pos := S.FBuf'low;
    S.PState.LNum := 1;
    S.PState.C := ' ';  -- Whitespace in lookahead char to make the scanner start up properly

    GetChar(S.FBuf,S.PState);
    loop
      GetSym(S.FBuf,S.PState,S.Radix,S.Sym,S.Vect,S.Str,S.Attr);
      case S.Sym is
        when SCAN_ID =>
          if (GetAttrIx(S.SymTab,S.Str,S.Attr,S.SymNextFree) /= 0) then
            assert false report
              ErrHead & "Found duplicated keyword while initializing scanner."
              severity SYNTAXERR;
          end if;
          AddNewName(S.SymTab,S.Str,S.Attr,S.SymNextFree,S.AttrNextFree,S.SymNextFree);
          S.AttrNextFree := S.AttrNextFree + 1;

      when SCAN_EOF =>
        exit;

      when others =>
        assert false report
          ErrHead & "Found symbol other than identifier while initializing scanner."
          severity SYNTAXERR;
      end case;
    end loop;

    S.MaxKeywIx := S.AttrNextFree - 1;

    -- read and interpret file
    ReadFile(ScriptDir, ScriptDir & FName, S.FBuf, S.FSize, S.LineNums);
    S.FBuf(S.FSize) := EOF;
    S.FSize := S.FSize + 1;
    S.PState.Pos := S.FBuf'low;
    S.PState.LNum := 1;
    S.PState.C := ' ';  -- whitespace in lookahead char to make the scanner start up properly
    S.PState.PrgState := PRG_IDLE;

    GetChar(S.FBuf,S.PState);
    GetNextSym(S);
  end InitTxtScan;


  procedure GetNextCmd(S: inout TxtScanState_t; Cmd: out integer) is

    variable KWord: integer;
    variable Tmp: integer;
    variable i: integer;
    variable SymNextFreeNew: integer;
    variable PStateTmp: PState_t;

    -- variable OutLine: line;

    procedure GetExtS is
    begin
      GetExtSym(S.FBuf,S.PState,S.Radix,S.Sym,S.Vect,S.Str,S.Attr,S.AttrIx,S.SymTab,S.MaxKeywIx,S.SymNextFree);
    end;

    procedure Eval(Res: inout integer) is
    begin
      EvalExpr(S.FBuf,S.LineNums,S.PState,S.Radix,S.Sym,S.Vect,S.Str,S.Attr,S.AttrIx,S.SymTab,S.MaxKeywIx,S.SymNextFree,S.SymAttrTab,Res);
    end;

  begin
    loop
      case S.Sym is
        when SCAN_EOF =>
          if S.PState.PrgState = PRG_LOOP then
            assert false report
              ErrHead & "End of file encountered with unterminated 'WHILE' starting at line " &
              to_str(GetLineNumInt(S.PStack(S.PStateSP-1).LNum,S.LineNums))
              severity SYNTAXERR;
          end if;
          if S.PState.PrgState = PRG_TRUE then
            assert false report
              ErrHead & "End of file encountered with unterminated 'IF' starting at line " &
              to_str(GetLineNumInt(S.PStack(S.PStateSP-1).LNum,S.LineNums))
              severity SYNTAXERR;
          end if;
          if S.PState.PrgState = PRG_FALSE then
            assert false report
              ErrHead & "End of file encountered with unterminated 'ELSE' starting at line " &
              to_str(GetLineNumInt(S.PStack(S.PStateSP-1).LNum,S.LineNums))
              severity SYNTAXERR;
          end if;
          Cmd := KW_EOF;
          exit;

        when SCAN_KEYW =>
          KWord := S.AttrIx;

          case KWord is
            when KW_WHILE =>
              -- save buffer state
              S.PStack(S.PStateSP) := S.PState;
              GetExtS;
              Eval(Tmp);
              if S.Sym /= SCAN_KEYW or S.AttrIx /= KW_LOOP then
                assert false report
                  ErrHead & "Missing 'LOOP'."
                  severity SYNTAXERR;
              else
                -- excute loop, so push saved state
                S.PStateSP := S.PStateSP + 1;
                S.PState.PrgState := PRG_LOOP;
                if Tmp = 0 then
                  -- skip loop
                  GetExtS;
                  i := 0;
                  while S.Sym /= SCAN_EOF loop
                    if S.Sym = SCAN_KEYW then
                      if S.AttrIx = KW_ENDLOOP then
                        if i = 0 then
                          S.PStateSP := S.PStateSP - 1;
                          S.PState.PrgState := S.PStack(S.PStateSP).PrgState;
                             exit;
                        else
                          i := i - 1;
                        end if;
                      elsif S.AttrIx = KW_WHILE then
                        i := i + 1;
                      end if;
                    end if;
                    GetExtS;
                  end loop;
                end if;
              end if;

            when KW_LOOP | KW_THEN =>
              assert false report
                ErrHead & "Unmatched 'LOOP' or 'THEN' at line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                severity SYNTAXERR;

            when KW_ENDLOOP =>
              if S.PState.PrgState = PRG_LOOP then
                -- jump back to loop start and reevaluate expression
                PStateTmp := S.PState;
                S.PState := S.PStack(S.PStateSP-1);
                GetExtS;
                Eval(Tmp);
                if Tmp = 0 then
                  -- terminate loop by restoring PStateTmp
                  S.PState := PStateTmp;              -- point after ENDLOOP
                  S.PStateSP := S.PStateSP - 1;
                  S.PState.PrgState := S.PStack(S.PStateSP).PrgState;
                else
                  -- keep state while looping
                  S.PState.PrgState := PStateTmp.PrgState;
                end if;
              else
                assert false report
                  ErrHead & "Unmatched 'ENDLOOP' near line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                  severity SYNTAXERR;
              end if;

            when KW_IF =>
              GetExtS;
              Eval(Tmp);
              if S.Sym = SCAN_KEYW and S.AttrIx = KW_THEN then
                S.PStack(S.PStateSP) := S.PState;
                S.PStateSP := S.PStateSP + 1;
                S.PState.PrgState := PRG_TRUE;
                if Tmp = 0 then
                  -- false, so skip up to matching ELSE or ENDIF
                  GetExtS;
                  i := 0;
                  while S.Sym /= SCAN_EOF loop
                    if S.Sym = SCAN_KEYW then
                      if S.AttrIx = KW_IF then
                        i := i + 1;
                      elsif S.AttrIx = KW_ELSE then
                        if i = 0 then
                          S.PState.PrgState := PRG_FALSE;
                             exit;
                        end if;
                      elsif S.AttrIx = KW_ENDIF then
                        if i = 0 then
                          S.PStateSP := S.PStateSP - 1;
                          S.PState.PrgState := S.PStack(S.PStateSP).PrgState;
                             exit;
                        else
                          i := i - 1;
                        end if;
                      end if;
                    end if;
                    GetExtS;
                  end loop;
                end if;
              else
                assert false report
                  ErrHead & "Missing 'THEN' at line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                  severity SYNTAXERR;
              end if;

            when KW_ELSE =>
              -- should be executing IF true part
              if S.PState.PrgState = PRG_TRUE then
                GetExtS;
                i := 0;
                while S.Sym /= SCAN_EOF loop
                  if S.Sym = SCAN_KEYW then
                    if S.AttrIx = KW_IF then
                      i := i + 1;
                    elsif i = 0 and S.AttrIx = KW_ELSE then
                      assert false report
                        ErrHead & "Unexpected 'ELSE' near line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                        severity SYNTAXERR;
                    elsif S.AttrIx = KW_ENDIF then
                      if i = 0 then
                        S.PStateSP := S.PStateSP - 1;
                        S.PState.PrgState := S.PStack(S.PStateSP).PrgState;
                        exit;
                      else
                        i := i - 1;
                      end if;
                    end if;
                  end if;
                  GetExtS;
                end loop;
              else
                assert false report
                  ErrHead & "Unmatched 'ELSE' near line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                  severity SYNTAXERR;
              end if;

            when KW_ENDIF =>
              if S.PState.PrgState = PRG_TRUE or S.PState.PrgState = PRG_FALSE then
                S.PStateSP := S.PStateSP - 1;
                S.PState.PrgState := S.PStack(S.PStateSP).PrgState;
              else
                assert false report
                  ErrHead & "Unmatched 'ENDIF' near line " & to_str(GetLineNumInt(S.PState.LNumPrev, S.LineNums))
                  severity SYNTAXERR;
              end if;

            when others =>
              Cmd := KWord;
              GetExtS;
              return;

          end case;
          GetExtS;

        when SCAN_ID =>
          -- WriteString(OutLine,to_str(S.Str));
          i := S.AttrIx;
          if i = 0 then
            AddNewName(S.SymTab,S.Str,S.Attr,S.SymNextFree,S.AttrNextFree,SymNextFreeNew);
            i := S.AttrNextFree;
            S.AttrNextFree  := S.AttrNextFree + 1;
            S.SymNextFree   := SymNextFreeNew;
            S.SymAttrTab(i) := 0;
          --  else
          --    SymNextFreeNew := S.SymNextFree;
          end if;
          GetExtS;
          if S.Sym /= SCAN_EQ then
            assert false report
              ErrHead & "Missing '=' near line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
              severity SYNTAXERR;
          else
            GetExtS;
            Eval(Tmp);
            S.SymAttrTab(i) := Tmp;
            -- WriteString(OutLine," = ");
            -- Write(OutLine,Tmp);
            -- WriteString(OutLine," line = ");
            -- Write(OutLine,S.GetLineNumInt(S.PState.LNumPrev, S.LineNums));
            -- writeline(output,OutLine);
          end if;
          --  S.SymNextFree := SymNextFreeNew;

        when SCAN_STR =>
          assert false report
            ErrHead & "Got unexpected string '" & to_str(S.Str) &
            "' at line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
            severity SYNTAXERR;
          GetExtS;

        when SCAN_BAD =>
          assert false report
            ErrHead & "Unknown token read near line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
            severity SYNTAXERR;
          GetExtS;

        when others =>
          assert false report
            ErrHead & "Got unexpected symbol (internal value = " &
            to_str(ScanSym_t'pos(S.Sym)) & ") at line " & to_str(GetLineNumInt(S.PState.LNum, S.LineNums))
            severity SYNTAXERR;
          GetExtS;

      end case;
    end loop;
  end GetNextCmd;

end package body tb_parse_p;

