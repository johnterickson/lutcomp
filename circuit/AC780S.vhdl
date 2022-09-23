library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity AC780S is
    port(	
        clk, csb, rs, sclk, sd: in std_logic;
        cgrom_data: in std_logic_vector(4 downto 0);
		cgrom_addr: out std_logic_vector(10 downto 0);
		pix_addr: out std_logic_vector(12 downto 0);
		pix_val, pix_clk: out std_logic;
        probe_CharRow: out std_logic_vector( 1 downto 0 );
        probe_CharCol: out std_logic_vector( 4 downto 0 );
        probe_PixRow: out std_logic_vector( 3 downto 0 );
        probe_PixCol: out std_logic_vector( 3 downto 0 );
        probe_AC: out std_logic_vector( 6 downto 0 );
        probe_state: out std_logic_vector( 1 downto 0 );
        probe_pix_state: out std_logic_vector( 1 downto 0 );
        probe_SerialData: out std_logic_vector( 7 downto 0 );
        probe_ValidSerialBits: out std_logic_vector( 3 downto 0 )
    );
end AC780S;

architecture rtl of AC780S is
    constant CHAR_PIX_ROWS : integer := 10;
    constant CGROM_PIX_ROWS : integer := 8;
    constant CHAR_PIX_COLS : integer := 6;
    constant DISPLAY_CHAR_ROWS : integer := 4;
    constant DISPLAY_CHAR_COLS : integer := 20;
    constant DISPLAY_PIX_ROWS : integer := DISPLAY_CHAR_ROWS * CHAR_PIX_ROWS;
    constant DISPLAY_PIX_COLS : integer := DISPLAY_CHAR_COLS * CHAR_PIX_COLS;
    constant ROW_0_ADDR : integer := 0;
    constant ROW_1_ADDR : integer := 64;
    constant ROW_2_ADDR : integer := 20;
    constant ROW_3_ADDR : integer := 84;

    type ddram_type is array (0 to 255) of unsigned(7 downto 0);

    signal CharRow : unsigned(1 downto 0) := B"00";
    signal CharCol : unsigned(4 downto 0) := B"00000";
    signal PixRow : unsigned(3 downto 0) := B"0000";
    signal PixCol : unsigned(3 downto 0) := B"0000";
    signal AC: unsigned(6 downto 0) := B"0000000";
    signal SerialData: unsigned(7 downto 0) := X"00";
    signal ValidSerialBits: unsigned(3 downto 0) := X"0";
    signal DDRAM: ddram_type;

    type states is (idle, read_serial, write_char);
    signal state: states := idle;

    type pix_states is (start, read_cgrom, clk_on, clk_off);
    signal pix_state: pix_states := start;
begin
    process(clk, csb, rs, sclk, sd, CharRow, CharCol, PixRow, PixCol, AC, SerialData, ValidSerialBits, DDRAM, state, pix_state)
    begin
        if (csb'event) then
            if (csb = '1') then
                state <= idle;
            else
                state <= read_serial;
                ValidSerialBits <= X"0";
                SerialData <= X"00";
            end if;
        elsif (state = read_serial and sclk'event and sclk = '1') then
            if (state = read_serial) then
                if (ValidSerialBits = X"7") then
                    if rs = '1' then
                        DDRAM(to_integer(unsigned(AC))) <= unsigned(SerialData(6 downto 0) & sd);
                        PixCol <= X"0";
                        PixRow <= X"0";
                        pix_state <= start;
                        state <= write_char;
                    else -- rs = '0'
                        if SerialData(6) = '1' then
                            AC <= unsigned(SerialData(6 downto 0) & sd);
                        end if;
                        state <= idle;
                    end if;
                else
                    SerialData <= SerialData(6 downto 0) & sd;
                    ValidSerialBits <= ValidSerialBits + X"1";
                end if;
            end if;
        elsif (clk'event and clk = '1' and state = write_char) then
            case pix_state is
                when start =>               cgrom_addr <= std_logic_vector(resize(DDRAM(to_integer(unsigned(AC))) * CGROM_PIX_ROWS + PixRow, 11));
                                            pix_addr <= std_logic_vector(
                                                    resize((resize(CharRow,13) * CHAR_PIX_ROWS + resize(PixRow,13))*DISPLAY_PIX_COLS, 13)
                                                + resize(resize(CharCol,13) * CHAR_PIX_COLS + resize(PixCol,13), 13)
                                                );
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
                                                    -- done writing char, now "increment" AC
                                                    if AC mod 64 = DISPLAY_CHAR_COLS - 1 then
                                                        -- we're at the end of a characater row
                                                        if AC = ROW_0_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                            AC <= to_unsigned(ROW_1_ADDR,7);
                                                        elsif AC = ROW_1_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                            AC <= to_unsigned(ROW_2_ADDR,7);
                                                        elsif AC = ROW_2_ADDR + DISPLAY_CHAR_COLS - 1 then
                                                            AC <= to_unsigned(ROW_3_ADDR,7);
                                                        else
                                                            AC <= to_unsigned(ROW_0_ADDR,7);
                                                        end if;
                                                    else
                                                        -- move to next character in the row
                                                        AC <= AC + 1;
                                                    end if;
                                                    PixRow <= X"0";
                                                    state <= idle;
                                                else
                                                    PixRow <= PixRow + 1;
                                                end if;
                                            else
                                                PixCol <= PixCol + 1;
                                            end if;
                                            pix_state <= start;
            end case;
        end if;
        CharRow <=  B"00" when AC < ROW_2_ADDR else
                    B"10" when AC < ROW_1_ADDR else
                    B"01" when AC < ROW_3_ADDR else
                    B"11";
        CharCol <= resize(AC - CharRow * DISPLAY_CHAR_COLS,5);
        probe_state <=  B"00" when state = idle else 
                        B"01" when state = read_serial else
                        B"10";
        probe_pix_state <= B"00" when pix_state = start else
                           B"01" when pix_state = read_cgrom else
                           B"10" when pix_state = clk_on else
                           B"11";
        probe_PixCol <= std_logic_vector(PixCol);
        probe_PixRow <= std_logic_vector(PixRow);
        probe_CharRow <= std_logic_vector(CharRow);
        probe_CharCol <= std_logic_vector(CharCol);
        probe_AC <= std_logic_vector(AC);
        probe_SerialData <= std_logic_vector(SerialData);
        probe_ValidSerialBits <= std_logic_vector(ValidSerialBits);
    end process;
end architecture rtl;