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
	constant BYTE_SIZE: natural := 8;
	constant WORD_SIZE: natural := 4 * BYTE_SIZE;
	subtype address_type is natural;
	type memory_type is array (address_type range <>) of std_ulogic_vector;
	-- 2**ADDRESS_SIZE - 1 overflows because natural is a 32 bit type
	-- but I have no clue how to fix it, using unsigned doesn't work
	signal memory: memory_type(0 to 2**ADDRESS_SIZE - 1)(BYTE_SIZE-1 downto 0);

	constant REGISTER_SIZE: natural := ADDRESS_SIZE;
	constant NUM_REGISTERS: natural := 32;
	type register_array_type is array (natural range <>) of u_signed;
	signal registers: register_array_type(NUM_REGISTERS-1 downto 0)(REGISTER_SIZE-1 downto 0);

	signal program_counter: u_unsigned(ADDRESS_SIZE-1 downto 0);
	-- negative, zero, carry, overflow
	signal flags: std_ulogic_vector(3 downto 0);

	function fetch_word(mem: memory_type; address: address_type) return std_ulogic_vector is
	begin
		return mem(address) & mem(address + 1) & mem(address + 2) & mem(address + 3);
	end fetch_word;
begin
	registers(0) <= to_signed(0, REGISTER_SIZE);

	process (clock, reset)
		variable instruction: std_ulogic_vector(WORD_SIZE-1 downto 0);
		variable opcode: std_ulogic_vector(5 downto 0);
		-- for J-type format
		variable jump_target: u_unsigned(25 downto 0);
		-- for I-type format
		variable immediate: u_signed(15 downto 0);
		-- for R-type format
		variable shift_amount: std_ulogic_vector(4 downto 0);
		variable funct_code: std_ulogic_vector(5 downto 0);
		variable register_d_index: natural range 0 to NUM_REGISTERS-1;
		variable register_d: u_signed(REGISTER_SIZE-1 downto 0);
		-- for R- and I- type format
		variable register_s_index: natural range 0 to NUM_REGISTERS-1;
		variable register_s: u_signed(REGISTER_SIZE-1 downto 0);
		variable register_t_index: natural range 0 to NUM_REGISTERS-1;
		variable register_t: u_signed(REGISTER_SIZE-1 downto 0);
	begin
		if reset then
			program_counter <= to_unsigned(0, REGISTER_SIZE);
			-- "downto 1" because registers(0) is set elsewhere,
			-- we don't want to also set it here
			for i in NUM_REGISTERS-1 downto 1 loop
				registers(i) <= to_signed(0, REGISTER_SIZE);
			end loop;
		elsif rising_edge(clock) then
			instruction := fetch_word(memory, to_integer(program_counter));
			opcode := instruction(31 downto 26);
			program_counter <= program_counter + 4;

			case opcode is
				when "000000" => -- R-type, also depends on funct_code
					register_s_index := to_integer(unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(unsigned(instruction(20 downto 16)));
					register_d_index := to_integer(unsigned(instruction(15 downto 11)));
					shift_amount := instruction(10 downto 6);
					funct_code := instruction(5 downto 0);

					case funct_code is
						when "000000" => -- explicit no-op
						when "001000" => -- jump register
							program_counter <= unsigned(registers(register_s_index));
						when "001001" => -- jump and link register
							registers(register_d_index) <= u_signed(program_counter + 4);
							program_counter <= unsigned(registers(register_s_index));
						when "100000" => -- add
							registers(register_d_index) <= registers(register_s_index) + registers(register_t_index);
						when "100010" => -- sub
							registers(register_d_index) <= registers(register_s_index) - registers(register_t_index);
						when "100100" => -- and
							registers(register_d_index) <= registers(register_s_index) and registers(register_t_index);
						when "100101" => -- or
							registers(register_d_index) <= registers(register_s_index) or registers(register_t_index);
						when "101000" => -- xor
							registers(register_d_index) <= registers(register_s_index) xor registers(register_t_index);
						-- TODO what's bless, shift left/right ?
						when others => -- invalid funct_code, we ignore it
					end case;
				when "000010" | "000011" => -- J-type
					jump_target := unsigned(instruction(25 downto 0));

					case opcode is
						when "000010" => -- jump
							program_counter <= program_counter(31 downto 28) & jump_target & "00";
						when "000011" => -- jump and link
							registers(31) <= u_signed(program_counter + 4);
							program_counter <= program_counter(31 downto 28) & jump_target & "00";
						when others => -- cannot happen
					end case;
				when others => -- I-type
					register_s_index := to_integer(unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(unsigned(instruction(20 downto 16)));
					immediate := signed(instruction(15 downto 0));

					case opcode is
						when "000100" => -- beq
						when "000101" => -- bne
						when "001000" => -- addi
							registers(register_t_index) <= registers(register_s_index) + (31 downto 16 => immediate(15), 15 downto 0 => immediate);
						when "001100" => -- andi
						-- TODO ori, xori
						when "100011" => -- load
						when "101011" => -- store
						when others => -- invalid opcode, we ignore it
					end case;
			end case;
		end if;
	end process;
end;
