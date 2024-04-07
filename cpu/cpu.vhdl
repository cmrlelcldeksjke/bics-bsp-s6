library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is
	port (
		clock: in std_logic;
		reset: in std_logic
	);
end;

architecture rtl of cpu is
	constant ADDRESS_SIZE: natural := 64;
	constant WORD_SIZE: natural := 32;
	type memory_type is array (natural range <>) of std_ulogic_vector;
	-- 2**ADDRESS_SIZE - 1 overflows because natural is a 32 bit type
	-- but I have no clue how to fix it, using unsigned doesn't work
	signal memory: memory_type(2**ADDRESS_SIZE - 1 downto 0)(WORD_SIZE-1 downto 0);

	constant REGISTER_SIZE: natural := ADDRESS_SIZE;
	constant NUM_REGISTERS: natural := 32;
	-- TODO I'm unsure about whether register_type should be u_unsigned, u_signed
	-- or std_ulogic_vector
	subtype register_type is u_unsigned(REGISTER_SIZE-1 downto 0);
	type register_array_type is array (natural range <>) of register_type;
	signal registers: register_array_type(NUM_REGISTERS-1 downto 0);

	signal program_counter: register_type;
	signal flags: register_type;


	function to_register(number: natural) return register_type is
	begin
		return to_unsigned(number, REGISTER_SIZE);
	end;
begin
	registers(0) <= to_register(0);

	process (clock, reset)
		variable instruction: std_ulogic_vector(WORD_SIZE-1 downto 0);
		variable opcode: std_ulogic_vector(5 downto 0);
		-- for J-type format
		variable jump_target: std_ulogic_vector(25 downto 0);
		-- for I-type format
		variable immediate: std_ulogic_vector(15 downto 0);
		-- for R-type format
		-- I have to research what shamt is for
		variable shamt: std_ulogic_vector(4 downto 0);
		variable funct_code: std_ulogic_vector(5 downto 0);
		variable register_d_index: register_type;
		-- for R- and I- type format
		variable register_s_index: register_type;
		variable register_t_index: register_type;
	begin
		if reset then
			program_counter <= to_register(0);
			-- "downto 1" because registers(0) is set elsewhere,
			-- we don't want to also set it here
			for i in NUM_REGISTERS-1 downto 1 loop
				registers(i) <= to_register(0);
			end loop;
		elsif rising_edge(clock) then
			instruction := memory(to_integer(program_counter));
			opcode := instruction(WORD_SIZE-1 downto WORD_SIZE-6);
			program_counter <= program_counter + 1;

			case opcode is
				when "000000" => -- R-type, also depends on funct_code
					shamt := instruction(10 downto 6);
					funct_code := instruction(5 downto 0);

					case funct_code is
						when "100000" => -- add
						when "100010" => -- sub
						when "100100" => -- and
						-- TODO or, xor
						when "001000" => -- jump register
						when "001001" => -- jump and link register
						when "000000" => -- explicit no-op
						when others => -- invalid funct_code, we ignore it
					end case;
				when "000010" => -- jump
				when "000011" => -- jump and link
				when "000100" => -- beq
				when "000101" => -- bne
				-- TODO what's bless, shift left/right ?
				when "001000" => -- addi
				when "001100" => -- andi
				-- TODO ori, xori
				when "100011" => -- load
				when "101011" => -- store
				when others => -- invalid opcode, we ignore it
			end case;
		end if;
	end process;
end;
