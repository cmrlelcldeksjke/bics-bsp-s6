library ieee;
use ieee.std_logic_1164.all;

entity counter_tb is
end;

architecture testbench of counter_tb is
	signal clock: std_logic := '0';
	signal reset: std_logic := '0';
	signal count_out: std_logic_vector(3 downto 0);
	constant PROPAGATION_DELAY: time := 1 ms;
begin
	counter: entity work.counter
		port map (clock => clock, reset => reset, count_out => count_out);

	clock <= not clock after 250 ms;
	reset <= '1', '0' after 20 ns;

	main: process
		variable num_iterations: natural range 0 to 20 := 0;
	begin
		wait until not reset;

		report "starting with count = " & to_string(count_out);

		while num_iterations < 20 loop
			wait until rising_edge(clock);
			wait for PROPAGATION_DELAY;

			num_iterations := num_iterations + 1;
			report "iteration = " & to_string(num_iterations) & ", count = " & to_string(count_out);
		end loop;

		std.env.finish;
	end process;
end;
