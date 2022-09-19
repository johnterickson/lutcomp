library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity lcd_driver is
    port(	
        clk, en, rw, rs: in std_logic;
		db_in: in std_logic_vector( 7 downto 0 );
		db_out: out std_logic_vector( 7 downto 0 );
		cgrom_addr: out std_logic_vector(10 downto 0);
        cgrom_data: in std_logic_vector(4 downto 0);
		pix_addr: out std_logic_vector(12 downto 0);
		pix_val, pix_clk: out std_logic;
        probe_CharRow: out std_logic_vector( 1 downto 0 );
        probe_CharCol: out std_logic_vector( 4 downto 0 );
        probe_PixRow: out std_logic_vector( 3 downto 0 );
        probe_PixCol: out std_logic_vector( 3 downto 0 );
        probe_AC: out std_logic_vector( 6 downto 0 );
        probe_state: out std_logic_vector( 1 downto 0 );
        probe_pix_state: out std_logic_vector( 1 downto 0 )
    );
end lcd_driver;

architecture rtl of lcd_driver is
    constant CHAR_PIX_ROWS : integer := 10;
    constant CGROM_PIX_ROWS : integer := 8;
    constant CHAR_PIX_COLS : integer := 6;
    constant DISPLAY_CHAR_ROWS : integer := 4;
    constant DISPLAY_CHAR_COLS : integer := 20;
    constant DISPLAY_PIX_ROWS : integer := DISPLAY_CHAR_ROWS * CHAR_PIX_ROWS;
    constant DISPLAY_PIX_COLS : integer := DISPLAY_CHAR_COLS * CHAR_PIX_COLS;
    constant LCD_WIDTH: integer := 120;

    type ddram_type is array (0 to 255) of unsigned(7 downto 0);

    signal CharRow : unsigned(1 downto 0) := B"00";
    signal CharCol : unsigned(4 downto 0) := B"00000";
    signal PixRow : unsigned(3 downto 0) := B"0000";
    signal PixCol : unsigned(3 downto 0) := B"0000";
    signal AC: unsigned(6 downto 0) := B"0000000";
    signal busy: std_logic := '0';
    signal DDRAM: ddram_type;

    type states is (idle, write_char);
    signal state: states := idle;

    type pix_states is (start, read_cgrom, clk_on, clk_off);
    signal pix_state: pix_states := start;
begin
    process(clk, state, pix_state, en, PixRow, PixCol, pix_clk, AC, DDRAM, CharRow, CharCol)
    begin
        probe_PixCol <= std_logic_vector(PixCol);
        probe_PixRow <= std_logic_vector(PixRow);
        probe_CharRow <= std_logic_vector(CharRow);
        probe_CharCol <= std_logic_vector(CharCol);
        probe_AC <= std_logic_vector(AC);
        if (clk'event or en'event) then
            case state is
                when idle =>        probe_state <= "00"; 
                when write_char =>  probe_state <= "01";
            end case;
            case pix_state is
                when start =>       probe_pix_state <= "00";
                when read_cgrom =>  probe_pix_state <= "01";
                when clk_on =>      probe_pix_state <= "10";
                when clk_off =>     probe_pix_state <= "11";
            end case;
            case state is
                when idle =>
                    if (en'event and en = '1') then
                        if rs = '1' then
                            if rw = '0' then
                                CharCol <=  unsigned(AC(4 downto 0));
                                CharRow <= unsigned(AC(6 downto 5));
                                DDRAM(to_integer(unsigned(AC))) <= unsigned(db_in);
                                PixCol <= X"0";
                                PixRow <= X"0";
                                pix_state <= start;
                                busy <= '1';
                                state <= write_char;
                            else -- rw = '1'
                                db_out <= std_logic_vector(DDRAM(to_integer(unsigned(AC))));
                            end if;
                        else -- rs = '0'
                            if rw = '0' then
                                if db_in(7) = '1' then
                                    AC <= unsigned(db_in(6 downto 0));
                                end if;
                            else -- rw = '1'
                                db_out(6 downto 0) <= std_logic_vector(AC);
                                db_out(7) <= busy;
                            end if;
                        end if;
                    end if;
                when write_char =>
                    case pix_state is
                        when start =>               cgrom_addr <= std_logic_vector(resize(DDRAM(to_integer(unsigned(AC))) * CGROM_PIX_ROWS + PixRow, 11));
                                                    pix_addr <= std_logic_vector(
                                                          resize((resize(CharRow,13) * CHAR_PIX_ROWS + resize(PixRow,13))*DISPLAY_PIX_COLS, 13)
                                                        + resize(resize(CharCol,13) * CHAR_PIX_COLS + resize(PixCol, 13), 13)
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
                                                            AC <= AC + 1;
                                                            PixRow <= X"0";
                                                            state <= idle;
                                                            busy <= '0';
                                                        else
                                                            PixRow <= PixRow + 1;
                                                        end if;
                                                    else
                                                        PixCol <= PixCol + 1;
                                                    end if;
                                                    pix_state <= start;
                    end case;
            end case;
        end if;
    end process;
end architecture rtl;