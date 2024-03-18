library ieee;
use ieee.std_logic_1164.all;

entity counter is
    port (
		clock: in std_ulogic;
		reset: in std_ulogic;
        count_out: out natural range 0 to 9
    );
end;

architecture rtl of counter is
begin
	main: process (clock, reset)
		variable count: natural range 0 to 9 := 0;
	begin
		if reset then
			count := 0;
		elsif rising_edge(clock) then
			count := count + 1 when count < 9 else 0;
			count_out <= count;
		end if;
	end process;
end;
