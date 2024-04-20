library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is
	port (
		clock: in std_logic;
		reset: in std_logic;
		switches: in std_logic_vector(15 downto 0);
		leds: out std_logic_vector(15 downto 0)
	);
end entity;

architecture rtl of cpu is
	constant ADDRESS_SIZE: natural := 64; -- valid address in instructions
	constant ADDRESS_BUS_SIZE: natural := 16; -- determines the actual memory size
	constant BYTE_SIZE: natural := 8;
	constant WORD_SIZE: natural := 4 * BYTE_SIZE;
	type memory_type is array (natural range <>) of std_logic_vector;
	signal memory: memory_type(0 to 2**ADDRESS_BUS_SIZE - 1)(BYTE_SIZE-1 downto 0);

	constant REGISTER_SIZE: natural := ADDRESS_SIZE;
	constant NUM_REGISTERS: natural := 32;
	type register_array_type is array (natural range <>) of signed;
	signal registers: register_array_type(NUM_REGISTERS-1 downto 0)(REGISTER_SIZE-1 downto 0);

	signal program_counter: unsigned(ADDRESS_SIZE-1 downto 0);
	-- negative, zero, carry, overflow
	signal flags: std_logic_vector(3 downto 0);

	type boot_sector_type is array (natural range <>) of std_logic_vector(WORD_SIZE-1 downto 0);
	constant BOOT_SECTOR: boot_sector_type := (
		B"001001_00000_00001_0000000000000001", -- subi R1,R0,1 (set R1 to full 1)
		B"101011_00001_00001_1111111111111001"  -- store R1,-7(R1) (switch on the leds)
	);

	function fetch_word(mem: memory_type; address_in: unsigned) return std_logic_vector is
		variable address: natural;
	begin
		address := to_integer(address_in mod 2**ADDRESS_BUS_SIZE);
		return mem(address mod 2**ADDRESS_BUS_SIZE) & mem((address+1) mod 2**ADDRESS_BUS_SIZE) & mem((address+2) mod 2**ADDRESS_BUS_SIZE) & mem((address+3) mod 2**ADDRESS_BUS_SIZE);
	end fetch_word;

	procedure store_word(signal mem: inout memory_type; address_in: in unsigned; value: in std_logic_vector(WORD_SIZE-1 downto 0)) is
		variable address: natural;
	begin
		address := to_integer(address_in mod 2**ADDRESS_BUS_SIZE);
		mem(address mod 2**ADDRESS_BUS_SIZE) <= value(31 downto 24);
		mem((address+1) mod 2**ADDRESS_BUS_SIZE) <= value(23 downto 16);
		mem((address+2) mod 2**ADDRESS_BUS_SIZE) <= value(15 downto 8);
		mem((address+3) mod 2**ADDRESS_BUS_SIZE) <= value(7 downto 0);
	end store_word;

begin
	leds(15 downto 8) <= memory(2**ADDRESS_BUS_SIZE - 8);
	leds(7 downto 0) <= memory(2**ADDRESS_BUS_SIZE - 7);
	memory(2**ADDRESS_BUS_SIZE - 4) <= switches(15 downto 8);
	memory(2**ADDRESS_BUS_SIZE - 3) <= switches(7 downto 0);

	process (clock, reset)
		variable instruction: std_logic_vector(WORD_SIZE-1 downto 0);
		variable opcode: std_logic_vector(5 downto 0);
		-- for J-type format
		variable jump_target: unsigned(25 downto 0);
		-- for I-type format
		variable immediate: signed(15 downto 0);
		-- for R-type format
		variable shift_amount: natural range 31 downto 0;
		variable funct_code: std_logic_vector(5 downto 0);
		variable register_d_index: natural range 0 to NUM_REGISTERS-1;
		variable register_d: signed(REGISTER_SIZE-1 downto 0);
		-- for R- and I- type format
		variable register_s_index: natural range 0 to NUM_REGISTERS-1;
		variable register_s: signed(REGISTER_SIZE-1 downto 0);
		variable register_t_index: natural range 0 to NUM_REGISTERS-1;
		variable register_t: signed(REGISTER_SIZE-1 downto 0);
	begin
		if reset then
			program_counter <= to_unsigned(0, ADDRESS_SIZE);
			flags <= x"0";
			registers(NUM_REGISTERS-1 downto 0) <= (NUM_REGISTERS-1 downto 0 => to_signed(0, REGISTER_SIZE));
			-- TODO is this a good idea ? program counter needs 0 for a no-op, otherwise you get uninitialized as instruction
			memory(0 to 2**ADDRESS_BUS_SIZE - 5) <= (0 to 2**ADDRESS_BUS_SIZE - 5 => std_logic_vector(to_unsigned(0, BYTE_SIZE)));

			-- copy the boot sector from its ROM to start of memory
			-- FIXME "for signal: .cpu_tb(testbench).cpu@cpu(rtl).memory(65528)(7)
			-- ghdl:error: several sources for unresolved signal"
			for i in BOOT_SECTOR'range loop
				store_word(memory, to_unsigned(i, ADDRESS_BUS_SIZE)*4, BOOT_SECTOR(i));
			end loop;
		elsif rising_edge(clock) then
			instruction := fetch_word(memory, program_counter);
			opcode := instruction(31 downto 26);
			program_counter <= program_counter + 4;

			case opcode is
				when "000000" => -- R-type, operation also depends on funct_code
					register_s_index := to_integer(unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(unsigned(instruction(20 downto 16)));
					register_d_index := to_integer(unsigned(instruction(15 downto 11)));
					shift_amount := to_integer(unsigned(instruction(10 downto 6)));
					funct_code := instruction(5 downto 0);

					case funct_code is
						when "000000" => -- shift left immediate
							registers(register_d_index) <= registers(register_t_index) sll shift_amount;
						when "000010" => -- shift right immediate
							registers(register_d_index) <= registers(register_t_index) srl shift_amount;
						when "000100" => -- shift left
							registers(register_d_index) <= registers(register_t_index) sll to_integer(registers(register_s_index));
						when "000110" => -- shift right
							registers(register_d_index) <= registers(register_t_index) srl to_integer(registers(register_s_index));
						when "001000" => -- jump register
							program_counter <= unsigned(registers(register_s_index));
						when "001001" => -- jump and link register
							registers(register_d_index) <= signed(program_counter + 4);
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
						when others => -- invalid funct_code, we ignore it
					end case;
				when "000010" | "000011" => -- J-type
					jump_target := unsigned(instruction(25 downto 0));

					case opcode is
						when "000010" => -- jump
							program_counter <= program_counter(63 downto 28) & jump_target & "00";
						when "000011" => -- jump and link
							registers(31) <= signed(program_counter + 4);
							program_counter <= program_counter(63 downto 28) & jump_target & "00";
						when others => -- cannot happen
					end case;
				when others => -- I-type
					register_s_index := to_integer(unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(unsigned(instruction(20 downto 16)));
					immediate := signed(instruction(15 downto 0));

					case opcode is
						when "000100" => -- beq
							if registers(register_s_index) = registers(register_t_index) then
								program_counter <= unsigned(signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "000101" => -- bneq
							if registers(register_s_index) /= registers(register_t_index) then
								program_counter <= unsigned(signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "001000" => -- addi
							registers(register_t_index) <= registers(register_s_index) + (63 downto 16 => immediate(15), 15 downto 0 => immediate);
						when "001001" => -- subi
							-- Note: normally this opcode is addiu in MIPS
							registers(register_t_index) <= registers(register_s_index) - (63 downto 16 => immediate(15), 15 downto 0 => immediate);
						when "001010" => -- bless
							-- Note: normally this opcode is slti in MIPS
							if registers(register_s_index) < registers(register_t_index) then
								program_counter <= unsigned(signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "001100" => -- andi
							registers(register_t_index) <= registers(register_s_index) and (63 downto 16 => '0', 15 downto 0 => immediate);
						when "001101" => -- ori
							registers(register_t_index) <= registers(register_s_index) or (63 downto 16 => '0', 15 downto 0 => immediate);
						when "001110" => -- xori
							registers(register_t_index) <= registers(register_s_index) xor (63 downto 16 => '0', 15 downto 0 => immediate);
						when "100011" => -- load
							registers(register_t_index) <= signed(fetch_word(memory, unsigned(registers(register_s_index) + (63 downto 16 => immediate(15), 15 downto 0 => immediate))));
						when "101011" => -- store
							store_word(memory, unsigned(registers(register_s_index) + (63 downto 16 => immediate(15), 15 downto 0 => immediate)), std_logic_vector(registers(register_t_index)(31 downto 0)));
						when others => -- invalid opcode, we ignore it
					end case;
			end case;
		end if;
	end process;
end architecture;
