library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity HD44780U is
    port(	
        clk, en, rw, rs: in std_logic;
        db_in: in std_logic_vector( 7 downto 0 );
        db_out: out std_logic_vector( 7 downto 0 );
        cgrom_addr: out unsigned(10 downto 0);
        cgrom_data: in std_logic_vector(4 downto 0);
        pix_addr: out unsigned(13 downto 0);
        pix_val, pix_clk, busy: out std_logic;
        CharRow: out unsigned( 1 downto 0 );
        CharCol: out unsigned( 4 downto 0 );
        PixRow: out unsigned( 3 downto 0 );
        PixCol: out unsigned( 3 downto 0 );
        AC: out unsigned( 6 downto 0 );
        StatePeek: out std_logic_vector( 2 downto 0 );
        PixStatePeek: out std_logic_vector( 1 downto 0 );
        BitModePeek: out std_logic_vector( 1 downto 0 );
        CmdReadStatePeek: out std_logic;
        Cmd: out std_logic_vector( 7 downto 0 )
    );
end HD44780U;

architecture rtl of HD44780U is
    constant CHAR_PIX_ROWS : integer := 10;
    constant CGROM_PIX_ROWS : integer := 8;
    constant CHAR_PIX_COLS : integer := 6;
    constant DISPLAY_CHAR_ROWS : integer := 2;
    constant DISPLAY_CHAR_COLS : integer := 20;
    constant DISPLAY_PIX_ROWS : integer := DISPLAY_CHAR_ROWS * CHAR_PIX_ROWS;
    constant DISPLAY_PIX_COLS : integer := DISPLAY_CHAR_COLS * CHAR_PIX_COLS;
    constant ROW_0_ADDR : integer := 0;
    constant ROW_1_ADDR : integer := 64;
    constant ROW_2_ADDR : integer := 20;
    constant ROW_3_ADDR : integer := 84;

    type ddram_type is array (0 to 255) of unsigned(7 downto 0);

    signal clearing: std_logic := '0';
    signal DDRAM: ddram_type;

    type bitmode_states is (UNKNOWN, BITMODE_4, BITMODE_8);
    signal bitmode: bitmode_states := UNKNOWN;

    type states is (init0, init1, init2, idle, run_cmd, write_char);
    signal state: states := init0;

    type cmd_read_states is (zero_nibble_read, first_nibble_read);
    signal cmd_read_state: cmd_read_states := zero_nibble_read;

    type pix_states is (start, read_cgrom, clk_on, clk_off);
    signal pix_state: pix_states := start;
begin
    process(clk, state, pix_state, en, PixRow, PixCol, pix_clk, AC, DDRAM, CharRow, CharCol, busy, clearing, cgrom_data, db_in, rs, rw)
    begin
        db_out <= B"ZZZZZZZZ";
        if (en = '1' and rs = '0' and rw = '1') then
            db_out(6 downto 0) <= std_logic_vector(AC);
            db_out(7) <= busy;
        elsif (en'event and en = '1' and bitmode = BITMODE_4 and state = idle and cmd_read_state = zero_nibble_read) then
            Cmd <= db_in(7 downto 4) & X"0";
            cmd_read_state <= first_nibble_read;
        elsif (en'event and en = '1' and state = init0 and rs = '0' and rw = '0' and db_in(7 downto 4) = B"0011") then
            state <= init1;
            busy <= '0';
        elsif (en'event and en = '1' and state = init1 and cmd_read_state = zero_nibble_read and rs = '0' and rw = '0' and db_in(7 downto 5) = B"001") then
            if db_in(4) = '1' then
                assert db_in(3) = '1' report "1 row not implemented" severity error;
                assert db_in(2) = '0' report "5 x 11 not implemented" severity error;
                bitmode <= BITMODE_8;
                state <= idle;
                busy <= '0';
            else
                bitmode <= BITMODE_4;
                cmd_read_state <=  first_nibble_read;
                Cmd <= X"20";
            end if;
        elsif (en'event and en = '1' and state = init1 and cmd_read_state = first_nibble_read and rs = '0' and rw = '0') then
            assert bitmode = BITMODE_4 report "should be in 4 bit mode" severity error;
            Cmd <= Cmd or (X"0" & db_in(7 downto 4));
            assert Cmd(3) = '1' report "1 row not implemented" severity error;
            assert Cmd(2) = '0' report "5 x 11 not implemented" severity error;
            cmd_read_state <= zero_nibble_read;
            state <= init2;
            busy <= '0';
        elsif (en'event and en = '1' and state = init2 and cmd_read_state = zero_nibble_read and rs = '0' and rw = '0' and db_in(7 downto 4) = B"0010") then
            assert bitmode = BITMODE_4 report "should be in 4 bit mode" severity error;
            cmd_read_state <=  first_nibble_read;
            Cmd <= X"20";
        elsif (en'event and en = '1' and state = init2 and cmd_read_state = first_nibble_read and rs = '0' and rw = '0') then
            assert bitmode = BITMODE_4 report "should be in 4 bit mode" severity error;
            cmd_read_state <= zero_nibble_read;
            state <= idle;
            busy <= '0';
        elsif (en'event and en = '1' and state = idle and (bitmode = BITMODE_8 or (bitmode = BITMODE_4 and cmd_read_state = first_nibble_read))) then
            if bitmode = BITMODE_4 then
                cmd_read_state <= zero_nibble_read;
                Cmd <= Cmd or (X"0" & db_in(7 downto 4));
            else
                Cmd <= db_in;
            end if;
            state <= run_cmd;
            busy <= '1';
        elsif (clk'event and clk = '1' and state = run_cmd) then
            state <= idle;
            busy <= '0';
            cmd_read_state <= zero_nibble_read;
            if rs = '0' and rw = '0' then
                if Cmd(7) = '1' then
                    AC <= unsigned(Cmd(6 downto 0));
                elsif Cmd(6) = '1' then
                    -- set CGRAM
                    assert Cmd(6) = '0' report "set CGRAM not implemented" severity error;
                elsif Cmd(5) = '1' then
                    if Cmd(4) = '1' then
                        bitmode <= BITMODE_8;
                    else
                        bitmode <= BITMODE_4;
                    end if;
                    assert Cmd(3) = '1' report "1 row not implemented" severity error;
                    assert Cmd(2) = '0' report "5 x 11 not implemented" severity error;
                elsif Cmd(4) = '1' then
                    assert Cmd(3) = '0' and Cmd(2) = '1' report "only `shift cursor to the right, AC is increased by 1` implemented" severity error;
                elsif Cmd(3) = '1' then
                    assert Cmd(2) = '1' report "'display off' not implemented" severity error;
                    assert Cmd(1) = '0' report "cursor not implemented" severity error;
                    assert Cmd(0) = '0' report "blink not implemented" severity error;
                elsif Cmd(2) = '1' then
                    assert Cmd(1) = '1' report "only LTR implemented" severity error;
                    assert Cmd(0) = '0' report "shift not implemented" severity error;
                elsif Cmd(1) = '1' then
                    AC <= to_unsigned(0, AC'length);
                elsif Cmd(0) = '1' then
                    for i in 0 to DDRAM'length - 1 loop
                        DDRAM(i) <= X"20";
                    end loop;
                    AC <= to_unsigned(0, AC'length);
                    PixCol <= X"0";
                    PixRow <= X"0";
                    pix_state <= start;
                    state <= write_char;
                    busy <= '1';
                    clearing <= '1';
                end if;
            elsif rs = '0' and rw = '1' then 
                db_out(6 downto 0) <= std_logic_vector(AC);
                db_out(7) <= busy;
            elsif rs = '1' and rw = '0' then
                DDRAM(to_integer(unsigned(AC))) <= unsigned(Cmd);
                PixCol <= X"0";
                PixRow <= X"0";
                pix_state <= start;
                state <= write_char;
                busy <= '1';
                clearing <= '0';
            else
                db_out <= std_logic_vector(DDRAM(to_integer(AC)));
            end if;
            Cmd <= X"00";
        elsif (clk'event and clk = '1' and state = write_char) then
            CharRow <=  B"00" when ROW_0_ADDR <= AC and AC < ROW_0_ADDR + DISPLAY_CHAR_COLS  else
                        B"01" when ROW_1_ADDR <= AC and AC < ROW_1_ADDR + DISPLAY_CHAR_COLS  else
                        B"10" when ROW_2_ADDR <= AC and AC < ROW_2_ADDR + DISPLAY_CHAR_COLS  else
                        B"11";
            CharCol <=  to_unsigned(to_integer(AC) - ROW_0_ADDR,5) when ROW_0_ADDR <= AC and AC < ROW_0_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_1_ADDR,5) when ROW_1_ADDR <= AC and AC < ROW_1_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_2_ADDR,5) when ROW_2_ADDR <= AC and AC < ROW_2_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_3_ADDR,5) when ROW_3_ADDR <= AC and AC < ROW_3_ADDR + DISPLAY_CHAR_COLS  else
                        B"11111";
            case pix_state is
                when start =>               cgrom_addr <= (resize(DDRAM(to_integer(unsigned(AC))) * CGROM_PIX_ROWS + PixRow, 11));
                                            pix_addr <= to_unsigned(
                                                (to_integer(CharRow) * CHAR_PIX_ROWS + to_integer(PixRow)) * DISPLAY_PIX_COLS
                                                + to_integer(CharCol) * CHAR_PIX_COLS + to_integer(PixCol),
                                                pix_addr'length);
                                            pix_state <= read_cgrom;
                when read_cgrom =>          pix_val <= shift_right(unsigned(cgrom_data), to_integer(CHAR_PIX_COLS - 1 - PixCol))(0);
                                            pix_state <= clk_on;
                                            pix_clk <= '0';
                when clk_on =>              pix_clk <= '1';
                                            pix_state <= clk_off;
                when clk_off =>             pix_clk <= '0';
                                            if PixCol = CHAR_PIX_COLS - 1 then
                                                PixCol <= X"0";
                                                if PixRow = CGROM_PIX_ROWS - 1 then

                                                    if AC /= ROW_3_ADDR + DISPLAY_CHAR_COLS - 1 and clearing = '1' then
                                                        pix_state <= start;
                                                    else
                                                        state <= idle;
                                                        busy <= '0';
                                                        clearing <= '0';
                                                        Cmd <= X"00";
                                                        cmd_read_state <= zero_nibble_read;
                                                    end if;

                                                    -- done writing char, now "increment" AC
                                                    if AC = ROW_0_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                        AC <= to_unsigned(ROW_1_ADDR,7);
                                                    elsif AC = ROW_1_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                        AC <= to_unsigned(ROW_2_ADDR,7);
                                                    elsif AC = ROW_2_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                        AC <= to_unsigned(ROW_3_ADDR,7);
                                                    elsif AC = ROW_3_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                        AC <= to_unsigned(ROW_0_ADDR,7);
                                                    else
                                                        -- move to next character in the row
                                                        AC <= AC + 1;
                                                    end if;

                                                    PixCol <= X"0";
                                                    PixRow <= X"0";
                                                else
                                                    PixRow <= PixRow + 1;
                                                end if;
                                            else
                                                PixCol <= PixCol + 1;
                                            end if;
                                            pix_state <= start;
            end case;
        end if;
        StatePeek <=    B"000" when state = init0 else
                        B"001" when state = init1 else
                        B"010" when state = init2 else
                        B"011" when state = idle else
                        B"100" when state = run_cmd else
                        B"101";
        PixStatePeek <= B"00" when pix_state = start else
                        B"01" when pix_state = read_cgrom else
                        B"10" when pix_state = clk_on else
                        B"11";
        CmdReadStatePeek <= '0' when cmd_read_state = zero_nibble_read else
                            '1';
        BitModePeek <=  B"00" when bitmode = UNKNOWN else
                        B"01" when bitmode = BITMODE_4 else
                        B"10";
    end process;
end architecture rtl;