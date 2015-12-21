library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use ieee.numeric_std.all;

library UNISIM;  
use UNISIM.Vcomponents.all;

entity DIY_Flash_ADC is
   Port ( LED     : out STD_LOGIC_VECTOR(3 downto 0);
          clk      : in STD_LOGIC;
			 p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15 : in STD_LOGIC;
			 n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15 : in STD_LOGIC;

				HSYNC : out std_logic;
				VSYNC : out std_logic;
				Red : out  STD_LOGIC_VECTOR(2 downto 0);
				Green : out  STD_LOGIC_VECTOR(2 downto 0);
				Blue : out  STD_LOGIC_VECTOR(1 downto 0)
        );
end DIY_Flash_ADC;

architecture Behavioral of DIY_Flash_ADC is
	signal thermom : STD_LOGIC_VECTOR(15 downto 0);
	signal code : STD_LOGIC_VECTOR(3 downto 0);
	signal sample : STD_LOGIC_VECTOR(3 downto 0);
	
	signal pcOut : STD_LOGIC;
	signal hcount : std_logic_vector(9 downto 0);
	signal vcount : std_logic_vector(9 downto 0);
	
   signal wea : STD_LOGIC_VECTOR(0 DOWNTO 0);
   signal addra : STD_LOGIC_VECTOR(16 DOWNTO 0);
   signal dina : STD_LOGIC_VECTOR(3 DOWNTO 0);
   signal addrb : STD_LOGIC_VECTOR(16 DOWNTO 0) := (others => '0');
   signal doutb : STD_LOGIC_VECTOR(3 DOWNTO 0);
	
	subtype STATES is STD_LOGIC_VECTOR(3 downto 0);
	constant WAITING_VSYNC : STATES := "0000";
	constant WAITING_HSYNC : STATES := "0001";
	constant WAITING_VHSYNC : STATES := "0010";
	constant WAITING_BACKPORCH : STATES := "0011";
	constant SKIPPING : STATES := "0100";
	constant SAMPLING : STATES := "0101";
	
	signal state : STATES := WAITING_VHSYNC;
	
component clk25MHz
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic
 );
end component;	

COMPONENT mem
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
END COMPONENT;
	
begin

	pixelOutClk : clk25MHz
  port map
   (-- Clock in ports
    CLK_IN1 => clk,
    -- Clock out ports
    CLK_OUT1 => pcOut);

displayMem : mem
  PORT MAP (
    clka => pcOut,
    ena => '1',
    wea => wea,
    addra => addra,
    dina => dina,
    clkb => pcOut,
    addrb => addrb,
    doutb => doutb
  );

	--decode NTSC, a lot of fudging here, but works most of the time
	read_cam: process(pcOut)
		variable count : integer := 0;
		variable vsyncCount : integer := 0;
		variable lineCount : integer := 0;
	begin
		if rising_edge(pcOut) then
			case state is
				
				when WAITING_VHSYNC =>
					
					if (sample /= "0000" AND sample /= "0001") then
						count := count + 1;
					elsif count > 0 then
						count := count - 1;
					end if;
					
					if (count = 100) then
						state <= WAITING_VSYNC;
						count := 0;
					end if;

				when WAITING_VSYNC =>
					
					if (sample = "0000" OR sample = "0001") then
						count := count + 1;
					elsif count > 0 then
						count := count - 1;
					end if;
					
					if (count = 1000) then
						vsyncCount := vsyncCount + 1;
						--got enough VSYNC, read for image
						if (vsyncCount = 20) then
							lineCount := 239;
							count := 0;
							state <= WAITING_HSYNC;
							
						else
							count := 0;
							state <= WAITING_VHSYNC;
						end if;
					end if;

				when WAITING_HSYNC =>
					
					if (sample = "0000" OR sample = "0001") then
						count := count + 1;
					elsif count > 0 then
						count := count - 1;
					end if;
					
					if (count = 100) then
						state <= WAITING_BACKPORCH;
						count := 0;
					end if;
		
				when WAITING_BACKPORCH =>
					
					if (sample /= "0000") then
						count := count + 1;
					elsif count > 0 then
						count := count - 1;
					end if;
					
					if (count = 100) then
						state <= SKIPPING;
						count := 0;
					end if;
		
				when SKIPPING =>
					count := count + 1;
					if (count = 300) then
						addra <= std_logic_vector(to_unsigned(linecount * 320, addra'length));
						wea <= "1";
						count := 0;
						state <= SAMPLING;
					end if;
					
				when SAMPLING =>
					count := count + 1;
					if (count mod 2) = 1 then
						addra <= addra + 1;
					end if;
					dina <= sample;
					if (count = 640) then
						count := 0;
						wea <= "0";
						lineCount := lineCount - 1;
						if (lineCount = -1) then
							state <= WAITING_VHSYNC;
							vsyncCount := 0;
						else
							state <= WAITING_HSYNC;
						end if;
					end if;
				
				when others =>
					state <= WAITING_VHSYNC;
				
			end case;
		end if;
	end process;

	--clock out the vga signal for the display output
	clock_vga: process(pcOut)
	begin
		if rising_edge(pcOut) then

			if hcount = 799 then
				 hcount <= (others => '0');
				 
				 if vcount = 524 then
					vcount <= (others => '0');
					addrb <= (others => '0');
				 else
					if (vcount(0) = '0') then
						addrb <= addrb - 320;
					end if;
					vcount <= vcount + 1;
				 end if;
			  else
				 hcount <= hcount + 1;
			end if;

			if vcount >= 490 and vcount < 492 then
			  vsync <= '0';
			else
			  vsync <= '1';
			end if;	

			if hcount >= 656 and hcount < 752 then
			  hsync <= '0';
			else
				hsync <= '1';
			end if;

			--from cam
			if hcount < 320 and vcount < 240 then
				Red <= doutb(2 downto 0);
				Green <= doutb(2 downto 0);
				Blue <= doutb(2 downto 1);
				addrb <= addrb + 1;
			--test pattern
			elsif hcount < 640 and vcount < 480 then
				Red <= vcount(7 downto 5);
				Green <= hcount(7 downto 5);
				Blue <= hcount(7 downto 6);
			--non-visible
			else
				Red <= (others => '0');
				Green <= (others => '0');
				Blue <= (others => '0');
			end if;	
		
		end if;
	end process;

	--wire up differential pins for ADC comparators
	ib0 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(0), I => p0, IB => n0);

	ib1 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(1), I => p1, IB => n1);

	ib2 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(2), I => p2, IB => n2);

	ib3 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(3), I => p3, IB => n3);

	ib4 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(4), I => p4, IB => n4);

	ib5 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(5), I => p5, IB => n5);

	ib6 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(6), I => p6, IB => n6);

	ib7 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(7), I => p7, IB => n7);

	ib8 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(8), I => p8, IB => n8);

	ib9 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(9), I => p9, IB => n9);

	ib10 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(10), I => p10, IB => n10);

	ib11 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(11), I => p11, IB => n11);

	ib12 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(12), I => p12, IB => n12);

	ib13 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(13), I => p13, IB => n13);

	ib14 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(14), I => p14, IB => n14);

	ib15 : IBUFDS
		generic map (IOSTANDARD => "LVDS_25")
		port map (O => thermom(15), I => p15, IB => n15);

	--LED <= c(24 downto 21);
	--LED <= sample;
	LED <= state;
	
	--transform thermometer reading to a binary value
	thermometer: process (thermom)
		begin
			case (thermom) is
				when "0000000000000000" => code <= "0000";
				when "0000000000000001" => code <= "0001";
				when "0000000000000011" => code <= "0010";
				when "0000000000000111" => code <= "0011";
				when "0000000000001111" => code <= "0100";
				when "0000000000011111" => code <= "0101";
				when "0000000000111111" => code <= "0110";
				when "0000000001111111" => code <= "0111";
				when "0000000011111111" => code <= "1000";
				when "0000000111111111" => code <= "1001";
				when "0000001111111111" => code <= "1010";
				when "0000011111111111" => code <= "1011";
				when "0000111111111111" => code <= "1100";
				when "0001111111111111" => code <= "1101";
				when "0011111111111111" => code <= "1110";
				when "0111111111111111" => code <= "1111";
				when "1111111111111111" => code <= "1111";
				when others => code <= "0000";
			end case;
		end process;
	
	register_sample: process(clk)
	  begin
		 if rising_edge(clk) then
			 sample <= not code;
		 end if;
	  end process;

end Behavioral;
