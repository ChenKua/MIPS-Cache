-- Code your testbench here
-- or browse Examples
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache_tb is
end cache_tb;

architecture behavior of cache_tb is

component cache is
generic(
    ram_size : INTEGER := 32768
);
port(
    clock : in std_logic;
    reset : in std_logic;

    -- Avalon interface --
    s_addr : in std_logic_vector (31 downto 0);
    s_read : in std_logic;
    s_readdata : out std_logic_vector (31 downto 0);
    s_write : in std_logic;
    s_writedata : in std_logic_vector (31 downto 0);
    s_waitrequest : out std_logic; 

    m_addr : out integer range 0 to ram_size-1;
    m_read : out std_logic;
    m_readdata : in std_logic_vector (7 downto 0);
    m_write : out std_logic;
    m_writedata : out std_logic_vector (7 downto 0);
    m_waitrequest : in std_logic
);
end component;

component memory is 
GENERIC(
    ram_size : INTEGER := 32768;
    mem_delay : time := 10 ns;
    clock_period : time := 1 ns
);
PORT (
    clock: IN STD_LOGIC;
    writedata: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    address: IN INTEGER RANGE 0 TO ram_size-1;
    memwrite: IN STD_LOGIC;
    memread: IN STD_LOGIC;
    readdata: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    waitrequest: OUT STD_LOGIC
);
end component;
	
-- test signals 
signal reset : std_logic := '0';
signal clk : std_logic := '0';
constant clk_period : time := 1 ns;

signal s_addr : std_logic_vector (31 downto 0);
signal s_read : std_logic;
signal s_readdata : std_logic_vector (31 downto 0);
signal s_write : std_logic;
signal s_writedata : std_logic_vector (31 downto 0);
signal s_waitrequest : std_logic;

signal m_addr : integer range 0 to 2147483647;
signal m_read : std_logic;
signal m_readdata : std_logic_vector (7 downto 0);
signal m_write : std_logic;
signal m_writedata : std_logic_vector (7 downto 0);
signal m_waitrequest : std_logic; 

begin

-- Connect the components which we instantiated above to their
-- respective signals.
dut: cache 
port map(
    clock => clk,
    reset => reset,

    s_addr => s_addr,
    s_read => s_read,
    s_readdata => s_readdata,
    s_write => s_write,
    s_writedata => s_writedata,
    s_waitrequest => s_waitrequest,

    m_addr => m_addr,
    m_read => m_read,
    m_readdata => m_readdata,
    m_write => m_write,
    m_writedata => m_writedata,
    m_waitrequest => m_waitrequest
);

MEM : memory
port map (
    clock => clk,
    writedata => m_writedata,
    address => m_addr,
    memwrite => m_write,
    memread => m_read,
    readdata => m_readdata,
    waitrequest => m_waitrequest
);
				

clk_process : process
begin
  clk <= '0';
  wait for clk_period/2;
  clk <= '1';
  wait for clk_period/2;
end process;

test_process : process
begin

--Test 1 Invalid Dirty Read Hit IMPOSSIBLE cannot be invalid and dirty
--Test 2 Invalid Dirty Write Hit IMPOSSIBLE cannot be invalid and dirty
--Test 3 Invalid Dirty Read Miss IMPOSSIBLE cannot be invalid and dirty
--Test 4 Invalid Dirty Write Miss IMPOSSIBLE cannot be invalid and dirty
--Test 5 Invalid Clean Read Hit IMPOSSIBLE cannot hit if invalid
--Test 6 Invalid Clean Write Hit IMPOSSIBLE cannot hit if invalid

--Test 7 Invalid Clean Read Miss

	s_addr <= "00000000000000000000000000111111"; 
	s_read <= '1';--attempts to read from cache, but nothing stored there yet, becomes valid clean
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;  
	
--Test 8 Invalid clean write miss 

	s_addr <= "00000000000000000000000000001111"; 
    s_writedata <= x"BBBBBBBB";
    s_read <= '0';
	s_write <= '1';--write miss becomes valid dirty 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns; 
	
--Test 9-->Valid Dirty Read Hit based on test8	
    s_addr <= "00000000000000000000000000001111"; 
	s_read <= '1';--attempts to read from cache right tag so hit
	s_write <= '0';
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns; 
	
--Test 10 -->Valid Dirty Read Miss based on test9

	s_addr <= "00000000000000000000000100001111"; 	
	s_read <= '1';--attempts to read from cache but wrong tag so miss, stores previous data to main memory and then becomes clean
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns; 
	
--Test 11 ->Valid Clean Read miss based on test10


	s_addr <= "00000000000000000000000000001111"; 
	s_read <= '1';--attempts to read from cache but wrong tag so miss, but now stored in cache
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns; 

 --Test 12 ->Valid Clean Read Hit based on 11
     
	s_addr <= "00000000000000000000000000001111"; 
	s_read <= '1';--attempts to read from cache and retrives data
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;
	
	--Test 13 ->Valid Clean Write Hit based on 12
     
	s_addr <= "00000000000000000000000000001111"; 
	s_writedata <= x"BCBCBCBC";
	s_read <= '0';
	s_write <= '1'; --writes to cache and becomes dirty
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;
	
	--Test 14 ->Valid Dirty Write Miss based on 13
     
	s_addr <= "00000000000000000000000100001111"; 
	s_writedata <= x"BDBDBDBD";
	s_read <= '0';
	s_write <= '1'; --attempts to writeto dirty, but miss, goes to main memory to store old data, still dirty though since new data in cache, but not main mem
	wait until rising_edge(s_waitrequest);
	s_addr <= "00000000000000000000000000001111"; 	
	s_read <= '1';--attempts to read from cache but wrong tag so miss, stores previous data to main memory and then becomes clean making it clean for the next test
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;
	
	--Test 15 ->Valid Clean Write Miss based on 14
	s_addr <= "00000000000000000000000100001111"; 
	s_writedata <= x"CDCDCDCD";
	s_read <= '0';
	s_write <= '1'; --attempts to write to clean, but miss, becomes dirty
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;
	
	--Test 16 ->Valid Dirty Write Hit based on 15
	s_addr <= "00000000000000000000000100001111"; 
	s_writedata <= x"ACBDABCD";
	s_read <= '0';
	s_write <= '1'; --attempts to write to dirty, hit, still dirty
	wait until rising_edge(s_waitrequest);
	s_addr <= "00000000000000000000000100001111"; 	
	s_read <= '1';--not necessary just making sure read reads properly here which it does since it reads ABCDABCD
	s_write <= '0'; 
	wait until rising_edge(s_waitrequest); 
    --goes back to intial
	s_read <= '0';                                                       
	s_write <= '0';
    wait for 25 ns;	
end process;
	
end;