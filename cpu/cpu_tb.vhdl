library ieee;
use ieee.std_logic_1164.all;

entity cpu_tb is
end;

architecture testbench of cpu_tb is
	signal clock: std_logic := '0';
	signal reset: std_logic := '0';
	signal switches: std_logic_vector(15 downto 0);
	signal leds: std_logic_vector(15 downto 0);
	constant PROPAGATION_DELAY: time := 1 ms;
begin
	cpu: entity work.cpu
		port map (clock => clock, reset => reset, switches => switches, leds => leds);

	clock <= not clock after 250 ms;
	reset <= '1', '0' after 20 ns;

	main: process
		variable num_iterations: natural range 0 to 20 := 0;
	begin
		wait until not reset;

		report "starting with leds = " & to_string(leds);

		while num_iterations < 20 loop
			wait until rising_edge(clock);
			wait for PROPAGATION_DELAY;

			num_iterations := num_iterations + 1;
			report "iteration = " & to_string(num_iterations) & ", leds = " & to_string(leds);
		end loop;

		std.env.finish;
	end process;
end;
