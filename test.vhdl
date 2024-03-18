library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity crossing is
port( clk, rst : in std_logic;
      S1, S2 : in std_logic;
      L1, L2, L3, L4 : out std_logic );
end crossing;

architecture a1p of crossing is
signal state : std_logic_vector(1 downto 0);
signal count : std_logic_vector(3 downto 0);
begin

puniq : process(clk, rst) begin
if rst='1' then state <= "00"; count <= x"0"; L1 <= '0'; L2 <= '0'; L3 <= '0'; L4 <= '0';
elsif rising_edge(clk) then
case state is
    when "00" =>
        state <= "01"; count <= x"0"; L1 <= '1'; L3 <= '1';
    when "01" =>
        if count = x"9" and S1 = '1' then state <= "10"; count <= x"0"; L3 <= '0'; L2 <= '1';
        elsif count = x"9" and S2 = '1' then state <= "11"; count <= x"0"; L1 <= '0'; L3 <= '0'; L4 <= '1';
        else if count /= x"9" then count <= count + 1; end if;
        end if;
    when "10" =>
        if (count = x"4" or S1 = '0') and S2 = '1' then state <= "11"; count <= x"0"; L1 <= '0'; L2 <= '0'; L4 <= '1';
        elsif (count = x"4" or S1 = '0') then state <= "01"; count <= x"0"; L2 <= '0'; L3 <= '1';
        else count <= count + 1;
        end if;
    when "11" =>
        if (count = x"3" or S2 = '0') then state <= "01"; count <= x"0"; L4 <= '0'; L1 <= '1'; L3 <= '1';
        else count <= count + 1;
        end if;
    when others => null;
end case;
end if; end process;

end a1p;
