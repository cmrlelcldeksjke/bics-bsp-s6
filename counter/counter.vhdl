library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
    port (
        clock: in std_logic;
        reset: in std_logic;
        count_out: out std_logic_vector(3 downto 0)
    );
end;

architecture rtl of counter is
	signal count: unsigned(3 downto 0);
begin
	count_out <= std_logic_vector(count);

	main: process (clock, reset)
	begin
		if reset then
			count <= x"0"; -- try to comment this line out, and observe the uninitialized value
		elsif rising_edge(clock) then
			count <= count + 1 when count < 9 else x"0";
		end if;
	end process;
end;
