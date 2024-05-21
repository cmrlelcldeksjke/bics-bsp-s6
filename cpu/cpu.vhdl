library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu is
	port (
		clock: in std_ulogic;
		reset: in std_ulogic; -- middle button
		switches: in std_ulogic_vector(15 downto 0);

		-- buttons around the middle button
		-- from 3 to 0, the buttons are: north, west, south, east
		buttons: in std_ulogic_vector(3 downto 0);

		leds: out std_ulogic_vector(15 downto 0)
	);
end entity;

architecture rtl of cpu is
	constant ADDRESS_SIZE: natural := 64; -- valid address in instructions
	constant ADDRESS_BUS_SIZE: natural := 16; -- determines the actual memory size
	constant BYTE_SIZE: natural := 8;
	constant INSTRUCTION_SIZE: natural := 4 * BYTE_SIZE;
	type memory_type is array (natural range <>) of std_ulogic_vector;
	signal memory: memory_type(0 to 2**ADDRESS_BUS_SIZE - 1)(BYTE_SIZE-1 downto 0);

	constant REGISTER_SIZE: natural := 64;
	constant NUM_REGISTERS: natural := 32;
	type register_array_type is array (natural range <>) of u_signed;
	signal registers: register_array_type(NUM_REGISTERS-1 downto 0)(REGISTER_SIZE-1 downto 0);

	signal program_counter: u_unsigned(ADDRESS_SIZE-1 downto 0);
	-- negative, zero, carry, overflow
	signal flags: std_ulogic_vector(3 downto 0);

	constant IO_SECTOR_START: natural := 256; -- at which address do I/O mapped addresses begin

	type boot_sector_type is array (natural range <>) of std_ulogic_vector(INSTRUCTION_SIZE-1 downto 0);
	-- the boot sector assumes all registers are initially 0
	constant BOOT_SECTOR: boot_sector_type := (
		B"001001_00000_00001_0000000000000001", -- subi R1,R0,1 (set R1 to full 1)
		B"101011_00000_00001_0000000100000100", -- store R1,260(R0) (switch on the leds)
		-- begin loop, while west button not pressed
		B"100011_00000_00010_0000000100001000", -- load R2,264(R0) (load status of buttons and switches into R2)
		B"001100_00010_00011_0000_0100_00000000", -- andi R3,R2,0x0400 (get status of west button in R3)
		B"000101_00011_00000_0000000000111111", -- bneq R3,R0,63 (if west button is pressed, jump out of bootloader. This is relative addressing, we're at address 4, jumping to 63 gives us (4+1+63)*4 = 68*4 = 272)
		B"001100_00010_00011_0000_0001_00000000", -- andi R3,R2,0x0100 (get status of east button in R3)
		B"000100_00011_00000_0000000000001001", -- beq R3,R0,9 (if east button is pressed)
		-- begin if
		B"000101_00100_00000_0000000000000011", -- bneq R4,R0,3 (if R4 = 0 / if we're editing the high halfword)
		-- begin if 2
		B"000000_00000_00010_00101_10000_000010", -- shift right R5,R2,16 (store status of switches into R5)
		B"000000_00000_00101_00101_10000_000000", -- shift left R5,R5,16 (put them in the high halfword of R5)
		B"000100_00000_00000_0000000000000100", -- beq R0,R0,4 (end if, skip else part)
		-- else 2 (we're editing the low halfword)
		B"000000_00000_00010_00110_10000_000010", -- shift right R6,R2,16 (store status of switches into R6)
		B"000000_00101_00110_00101_00000_100100", -- and R5,R5,R6 (put them in the low halfword of R5)
		B"101011_00111_00101_0000000100010000", -- store R5,272(R7) (store this newly formed instruction in memory)
		B"001000_00111_00111_0000000000000001", -- addi R7,R7,1 (R7++)
		-- end if 2
		B"001110_00100_00100_0000000000000001", -- xori R4,R4,1 (negate R4)
		-- end if
		B"000010_00000000000000000000000010" -- jump 2 (repeat loop)
		-- end loop
	);

	impure function fetch_byte(address_in: u_unsigned) return std_ulogic_vector is
		variable address: natural;
	begin
		address := to_integer(address_in mod 2**ADDRESS_BUS_SIZE);
		case address is
			when 0 to 255 => return x"00"; -- boot sector, should never happen

			when 256 => return "0000" & flags;
			when 257 to 259 => return x"00"; -- reserved

			when 260 => return leds(15 downto 8);
			when 261 => return leds(7 downto 0);
			when 262 to 263 => return x"00"; -- reserved

			when 264 => return switches(15 downto 8);
			when 265 => return switches(7 downto 0);
			when 266 => return "0000" & buttons;
			when 267 => return x"00"; -- reserved

			when 268 to 271 => return x"00"; -- reserved

			when others => return memory(address - 272);
		end case;
	end fetch_byte;

	impure function fetch_word(address_in: u_unsigned) return std_ulogic_vector is
		variable address: natural;
	begin
		address := to_integer(address_in);
		case address is
			when 0 to 255 => return BOOT_SECTOR(address / 4);
			when others => return fetch_byte(address_in) & fetch_byte(address_in + 1) & fetch_byte(address_in + 2) & fetch_byte(address_in + 3);
		end case;
	end fetch_word;

	procedure store_byte(address_in: in u_unsigned; value: in std_ulogic_vector(BYTE_SIZE-1 downto 0); signal mem: inout memory_type; signal leds_in: inout std_ulogic_vector) is
		variable address: natural;
	begin
		address := to_integer(address_in mod 2**ADDRESS_BUS_SIZE);
		case address is
			when 0 to 255 => -- boot sector

			when 256 to 259 => -- flags

			when 260 => leds_in(15 downto 8) <= value;
			when 261 => leds_in(7 downto 0) <= value;
			when 262 to 263 => -- reserved

			when 264 to 267 => -- switches and buttons

			when 268 to 271 => -- reserved

			when others => mem(address - 272) <= value;
		end case;
	end store_byte;

	procedure store_word(address: in u_unsigned; value: in std_ulogic_vector(INSTRUCTION_SIZE-1 downto 0); signal mem: inout memory_type; signal leds_in: inout std_ulogic_vector) is
	begin
		store_byte(address, value(31 downto 24), mem, leds_in);
		store_byte(address + 1, value(23 downto 16), mem, leds_in);
		store_byte(address + 2, value(15 downto 8), mem, leds_in);
		store_byte(address + 3, value(7 downto 0), mem, leds_in);
	end store_word;

	procedure register_add(result: inout u_signed; op1: in u_signed; op2: in u_signed; signal my_flags: inout std_ulogic_vector) is
		variable tmp: u_signed(REGISTER_SIZE+1 downto 0);
	begin
		tmp := ('0' & op1(REGISTER_SIZE-1) & op1) + ('0' & op2(REGISTER_SIZE-1) & op2);
		result := tmp(REGISTER_SIZE-1 downto 0);
		my_flags(3) <= tmp(REGISTER_SIZE-1); -- negative
		my_flags(2) <= '1' when tmp(REGISTER_SIZE-1 downto 0) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
		my_flags(1) <= tmp(REGISTER_SIZE+1); -- carry
		my_flags(0) <= '1' when tmp(REGISTER_SIZE) = tmp(REGISTER_SIZE-1) else '0'; -- overflow
	end register_add;

begin
	process (clock, reset)
		variable instruction: std_ulogic_vector(INSTRUCTION_SIZE-1 downto 0);
		variable opcode: std_ulogic_vector(5 downto 0);
		-- for J-type format
		variable jump_target: u_unsigned(25 downto 0);
		-- for I-type format
		variable immediate: u_signed(15 downto 0);
		-- for R-type format
		variable shift_amount: natural range 31 downto 0;
		variable funct_code: std_ulogic_vector(5 downto 0);
		variable register_d_index: natural range 0 to NUM_REGISTERS-1;
		-- for R- and I- type format
		variable register_s_index: natural range 0 to NUM_REGISTERS-1;
		variable register_t_index: natural range 0 to NUM_REGISTERS-1;

		-- stupid hack
		variable tmp_register: u_signed(REGISTER_SIZE-1 downto 0);
	begin
		if reset then
			program_counter <= to_unsigned(0, ADDRESS_SIZE);
			flags <= "0000";
			registers <= (NUM_REGISTERS-1 downto 0 => to_signed(0, REGISTER_SIZE));
		elsif rising_edge(clock) then
			instruction := fetch_word(program_counter);
			opcode := instruction(31 downto 26);
			program_counter <= program_counter + 4;

			case opcode is
				when "000000" => -- R-type, operation also depends on funct_code
					register_s_index := to_integer(u_unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(u_unsigned(instruction(20 downto 16)));
					register_d_index := to_integer(u_unsigned(instruction(15 downto 11)));
					shift_amount := to_integer(u_unsigned(instruction(10 downto 6)));
					funct_code := instruction(5 downto 0);

					case funct_code is
						when "000000" => -- shift left immediate
							registers(register_d_index) <= registers(register_t_index) sll shift_amount;

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "000010" => -- shift right immediate
							registers(register_d_index) <= registers(register_t_index) srl shift_amount;

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "000100" => -- shift left
							registers(register_d_index) <= registers(register_t_index) sll to_integer(registers(register_s_index));

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "000110" => -- shift right
							registers(register_d_index) <= registers(register_t_index) srl to_integer(registers(register_s_index));

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "001000" => -- jump register
							program_counter <= u_unsigned(registers(register_s_index));
						when "001001" => -- jump and link register
							registers(register_d_index) <= u_signed(program_counter + 4);
							program_counter <= u_unsigned(registers(register_s_index));
						when "100000" => -- add
							-- amazing hack with tmp variable
							tmp_register := registers(register_d_index);
							register_add(tmp_register, registers(register_s_index), registers(register_t_index), flags);
							registers(register_d_index) <= tmp_register;
						when "100010" => -- sub
							-- amazing hack with tmp variable
							tmp_register := registers(register_d_index);
							register_add(tmp_register, registers(register_s_index), - registers(register_t_index), flags);
							registers(register_d_index) <= tmp_register;
						when "100100" => -- and
							registers(register_d_index) <= registers(register_s_index) and registers(register_t_index);

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "100101" => -- or
							registers(register_d_index) <= registers(register_s_index) or registers(register_t_index);

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "101000" => -- xor
							registers(register_d_index) <= registers(register_s_index) xor registers(register_t_index);

							flags(3) <= registers(register_d_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_d_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when others => -- invalid funct_code, we ignore it
					end case;
				when "000010" | "000011" => -- J-type
					jump_target := u_unsigned(instruction(25 downto 0));

					case opcode is
						when "000010" => -- jump
							program_counter <= program_counter(63 downto 28) & jump_target & "00";
						when "000011" => -- jump and link
							registers(31) <= u_signed(program_counter + 4);
							program_counter <= program_counter(63 downto 28) & jump_target & "00";
						when others => -- cannot happen
					end case;
				when others => -- I-type
					register_s_index := to_integer(u_unsigned(instruction(25 downto 21)));
					register_t_index := to_integer(u_unsigned(instruction(20 downto 16)));
					immediate := u_signed(instruction(15 downto 0));

					case opcode is
						when "000100" => -- beq
							if registers(register_s_index) = registers(register_t_index) then
								program_counter <= u_unsigned(u_signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "000101" => -- bneq
							if registers(register_s_index) /= registers(register_t_index) then
								program_counter <= u_unsigned(u_signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "001000" => -- addi
							-- amazing hack with tmp variable
							tmp_register := registers(register_t_index);
							register_add(tmp_register, registers(register_s_index), (63 downto 16 => immediate(15), 15 downto 0 => immediate), flags);
							registers(register_t_index) <= tmp_register;
						when "001001" => -- subi
							-- Note: normally this opcode is addiu in MIPS
							-- amazing hack with tmp variable
							tmp_register := registers(register_t_index);
							register_add(tmp_register, registers(register_s_index), -(63 downto 16 => immediate(15), 15 downto 0 => immediate), flags);
							registers(register_t_index) <= tmp_register;
						when "001010" => -- bless
							-- Note: normally this opcode is slti in MIPS
							if registers(register_s_index) < registers(register_t_index) then
								program_counter <= u_unsigned(u_signed(program_counter) + 4 + (63 downto 18 => immediate(15), 17 downto 2 => immediate, 1 downto 0 => '0'));
							end if;
						when "001100" => -- andi
							registers(register_t_index) <= registers(register_s_index) and (63 downto 16 => '0', 15 downto 0 => immediate);

							flags(3) <= registers(register_t_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_t_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "001101" => -- ori
							registers(register_t_index) <= registers(register_s_index) or (63 downto 16 => '0', 15 downto 0 => immediate);

							flags(3) <= registers(register_t_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_t_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "001110" => -- xori
							registers(register_t_index) <= registers(register_s_index) xor (63 downto 16 => '0', 15 downto 0 => immediate);

							flags(3) <= registers(register_t_index)(REGISTER_SIZE-1); -- negative
							flags(2) <= '1' when registers(register_t_index) = to_signed(0, REGISTER_SIZE) else '0'; -- zero
						when "100011" => -- load
							registers(register_t_index)(31 downto 0) <= u_signed(fetch_word(u_unsigned(registers(register_s_index) + (63 downto 16 => immediate(15), 15 downto 0 => immediate))));
						when "101011" => -- store
							store_word(u_unsigned(registers(register_s_index) + (63 downto 16 => immediate(15), 15 downto 0 => immediate)), std_ulogic_vector(registers(register_t_index)(31 downto 0)), memory, leds);
						when others => -- invalid opcode, we ignore it
					end case;
			end case;
		end if;
	end process;
end architecture;
