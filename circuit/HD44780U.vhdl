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
        StatePeek: out std_logic_vector( 1 downto 0 );
        PixStatePeek: out std_logic_vector( 1 downto 0 )
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

    type states is (idle, write_char);
    signal state: states := idle;

    type pix_states is (start, read_cgrom, clk_on, clk_off);
    signal pix_state: pix_states := start;
begin
    process(clk, state, pix_state, en, PixRow, PixCol, pix_clk, AC, DDRAM, CharRow, CharCol, busy, clearing)
    begin
        if (en'event and en = '1' and rs = '0' and rw = '1') then
            db_out(6 downto 0) <= std_logic_vector(AC);
            db_out(7) <= busy;
        elsif (en'event and en = '1' and state = idle) then
            if rs = '0' and rw = '0' then
                if db_in(7) = '1' then
                    AC <= unsigned(db_in(6 downto 0));
                elsif db_in(6) = '1' then
                    -- set CGRAM
                    assert db_in(6) = '0' report "set CGRAM not implemented" severity error;
                elsif db_in(5) = '1' then
                    assert db_in(4) = '1' report "4-bit mode not implemented" severity error;
                    assert db_in(3) = '1' report "1 row not implemented" severity error;
                    assert db_in(2) = '0' report "5 x 11 not implemented" severity error;
                elsif db_in(4) = '1' then
                    assert db_in(3) = '0' and db_in(2) = '1' report "only `shift cursor to the right, AC is increased by 1` implemented" severity error;
                elsif db_in(3) = '1' then
                    assert db_in(2) = '1' report "'display off' not implemented" severity error;
                    assert db_in(1) = '0' report "cursor not implemented" severity error;
                    assert db_in(0) = '0' report "blink not implemented" severity error;
                elsif db_in(2) = '1' then
                    assert db_in(1) = '1' report "only LTR implemented" severity error;
                    assert db_in(0) = '0' report "shift not implemented" severity error;
                elsif db_in(1) = '1' then
                    AC <= to_unsigned(0, AC'length);
                elsif db_in(0) = '1' then
                    for i in 0 to DDRAM'length - 1 loop
                        DDRAM(i) <= X"20";
                    end loop;
                    AC <= to_unsigned(0, AC'length);
                    PixCol <= X"0";
                    PixRow <= X"0";
                    pix_state <= start;
                    busy <= '1';
                    state <= write_char;
                    clearing <= '1';
                end if;
            elsif rs = '0' and rw = '1' then 
                db_out(6 downto 0) <= std_logic_vector(AC);
                db_out(7) <= busy;
            elsif rs = '1' and rw = '0' then
                DDRAM(to_integer(unsigned(AC))) <= unsigned(db_in);
                PixCol <= X"0";
                PixRow <= X"0";
                pix_state <= start;
                busy <= '1';
                state <= write_char;
                clearing <= '0';
            else
                db_out <= std_logic_vector(DDRAM(to_integer(AC)));
            end if;
        elsif (clk'event and clk = '1' and state = write_char) then
            CharRow <=  B"00" when ROW_0_ADDR <= AC and AC < ROW_0_ADDR + DISPLAY_CHAR_COLS  else
                        B"01" when ROW_1_ADDR <= AC and AC < ROW_1_ADDR + DISPLAY_CHAR_COLS  else
                        B"10" when ROW_2_ADDR <= AC and AC < ROW_2_ADDR + DISPLAY_CHAR_COLS  else
                        B"11";
            CharCol <=  to_unsigned(to_integer(AC) - ROW_0_ADDR,5) when ROW_0_ADDR <= AC and AC < ROW_0_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_1_ADDR,5) when ROW_1_ADDR <= AC and AC < ROW_1_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_2_ADDR,5) when ROW_2_ADDR <= AC and AC < ROW_2_ADDR + DISPLAY_CHAR_COLS  else
                        to_unsigned(to_integer(AC) - ROW_3_ADDR,5);
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
        StatePeek <=    B"00" when state = idle else 
                        B"01";
        PixStatePeek <= B"00" when pix_state = start else
                        B"01" when pix_state = read_cgrom else
                        B"10" when pix_state = clk_on else
                        B"11";
    end process;
end architecture rtl;