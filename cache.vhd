library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache is
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
end cache;

architecture arch of cache is

-- declare signals here

-- 128-bit blocks, 4096-bit data storage
-- 4096/128 = 32 blocks in the cache
-- 2^5 = 32 => 5 bits for Block index
-- 2 bits for Block offsets (4 words in 1 block)
-- 2 bits for byte offsets (4 bytes in 1 word) not need in our MIPS design
-- 128 bit for storage (bit 0-127)

-- To be determined here
-- 32 - 2 - 2 - 5 = tag = 23
-- We will only use first 15 bits for address 
-- Therefore, 15 - 2 - 2 - 5 = tag = 6 bits for tag
-- Address 14-9 bit in the address
-- 1 bit for dirty 
-- 1 bit for valid 

-- 1 + 1 + 6 + 128 = 136 bits

type my_cache is array (0 to 31) of std_logic_vector (135 downto 0);
SIGNAL cache_block: my_cache;

type state_type is (initial, read, write, read_load, write_back, write_load);
SIGNAL current_state : state_type;
SIGNAL next_state : state_type;

begin

-- make circuits here

my_process : process(clock, reset)
begin
	if reset = '1' then
		current_state <= initial;
	elsif rising_edge(clock) then
		current_state <= next_state;
	end if;
		
end process;


state_machine : process(s_read, s_write, m_waitrequest, current_state)

	variable word_offset : INTEGER :=0;   -- address bit 2 and 3 from the s_addr
	variable block_index : INTEGER :=0;	  -- bit 4-8 from 32bit s_addr
	variable tag : std_logic_vector (5 downto 0);	-- hold the 6 useful bits for tag
	
    -- intermedia vector used to store the address when writing cache content back to the memory
	variable address_temp: std_logic_vector (10 downto 0);	

	-- a boolean flag to trigger load from memory, indicates whether the prev state is writeback 
    -- because for write operation triggering writeback, another load is requried after writeback
    variable from_writeback : INTEGER :=0;		
    
	variable read_in_process : INTEGER :=0;	 -- a flag regulating the load flow.
	variable counter : INTEGER := 0;   -- 4 words in one block, 16 bytes in one block => every load need load 16 bytes, need counter to count 16

begin
	-- convert vector to integer
	word_offset := to_integer(unsigned (s_addr(3 downto 2)));
	block_index := to_integer(unsigned(s_addr(8 downto 4)));
	tag := s_addr(14 downto 9);
	

case current_state is

	when initial =>
    	-- cache is ready
    	s_waitrequest <= '1';
        -- check request
		if s_write = '1' then
			next_state <= write;
		elsif s_read = '1' then
			next_state <= read;
		else
			next_state <= initial;
		end if;
    
	when write =>
    	-- in case of write hit (valid == 1  + tag hit)
   	 	-- the dirty is not important for write hit
		-- then this is a direct cache write without memory interaction since we are write back policy
    	-- 135 is valid bit, 134 is dirty bit
    	-- 133 - 128 for tag
    	if cache_block(block_index)(135) = '1' and cache_block(block_index)(133 downto 128) = s_addr (14 downto 9) then
    		-- set content
			cache_block(block_index)( (word_offset+1)*32 -1 downto word_offset*32 ) <= s_writedata;
			
            -- set tag. maybe dont need this. But no harm to have it
			cache_block(block_index)(133 downto 128) <= tag;

			--set bits
			cache_block(block_index)(135) <= '1'; -- valid
			cache_block(block_index)(134) <= '1'; -- dirty
			s_waitrequest <= '0';

			next_state <= initial;
		
        -- in case of miss or non valid 
        -- dirty bit must be 0 in order to avoid write back
        -- load the content from memory to write
        -- Load must load the whole block i.e 16 bytes
        -- Use counter to count how many bytes is loaded
        elsif (cache_block(block_index)(135) = '0' or cache_block(block_index)(133 downto 128) /= s_addr (14 downto 9)) 
				and cache_block(block_index)(134) = '0' then
        		if m_waitrequest = '1' then 
					m_read <= '1';
					m_addr <= to_integer(unsigned(s_addr(14 downto 4)))*16 + counter;
					next_state <= write_load;
				else
					next_state <= write;
				end if;	
                
        -- in case of last state is writeback, then a similar process of loading is required
		-- here in cache_write, we issue the read request to memory
		-- when the state jumps to write_load, we will get the memory output data
		elsif from_writeback = 1 then
			if m_waitrequest = '1' then 
				m_read <= '1';
				m_addr <= to_integer(unsigned(s_addr(14 downto 4)))*16 + counter; -- byte address
				next_state <= write_load;
			else
				next_state <= write;
			end if;
        
        
        -- other cases (valid and miss + dirty)
        -- need to write back
		else 
			next_state <= write_back;
		end if;
        
	when read =>
		-- if read valid + tag hit, we can directly read from cache and get the output
		if cache_block(block_index)(135) = '1' and cache_block(block_index)(133 downto 128) = s_addr (14 downto 9) then
			s_readdata <= cache_block(block_index)((word_offset+1)*32 -1 downto word_offset*32);
			s_waitrequest <= '0';
			next_state <= initial;
				
		-- tag miss and dirty, need write back
		elsif cache_block(block_index)(134) = '1' and 
        		cache_block(block_index)(133 downto 128) /= s_addr (14 downto 9)then
			next_state <= write_back;
			
		-- tag miss and clean, directly replace the memory, no wirteback
		else 
			next_state <= read_load;
		end if;
     
    when write_back =>
		-- for write back, ensure that the entire block is loaded back. 16 byte for a block and why there is a counter 
		if counter < 16 and m_waitrequest = '1' then
			address_temp := cache_block(block_index)(133 downto 128)& s_addr(8 downto 4);
			
            -- use the tag and index to find the address of the old content in cache
            m_addr <= to_integer(unsigned( address_temp))*16 + counter;	
			m_write <= '1';
			m_writedata <= cache_block(block_index)( (counter+1)*8 -1 downto counter*8);
			counter := counter  + 1;
			next_state <= write_back;
			
		elsif counter = 16 then		-- finished writing 16 byte 
			if s_write = '1' then
				-- if request is write, trigger the process of load for write
				counter := 0;
				from_writeback := 1;	-- need another load from writeback
				m_write <= '0';
				next_state <= write;
			else			
				-- if request is read, trigger the provess of read_load
				counter := 0;
				m_write <= '0';
				next_state <= read_load;
			end if;
            
        -- has not done 16 byte transfer, stay in this state, until the memory is ready for another writ
		else
			m_write <= '0';
			next_state <= write_back;
		end if;
            
	when write_load =>
		-- if no need to wait memory, it has output (m_waitrequest = '0')
		-- then keep getting byte until loading all 16 bytes
		if m_waitrequest = '0' and counter < 15 then
			cache_block(block_index)( (counter+1)*8 -1 downto counter*8) <= m_readdata;
			counter := counter + 1;
			m_read <= '0'; -- this reading is done
			next_state <= write;	-- back to state write because the memory request is issued in write
		elsif m_waitrequest = '0' and counter = 15 then
			cache_block(block_index)( (counter+1)*8 -1 downto counter*8) <= m_readdata;
			m_read <= '0';
			counter := counter + 1;
			next_state <= write_load;
            
		-- all 16 bytes are loaded from memory
		-- the final step is to set valid and dirty bit, and also set the new tag
		elsif counter = 16 then
			counter := 0;	-- reset
			cache_block(block_index)(135) <= '1'; -- valid
			cache_block(block_index)(134) <= '0'; -- dirty bit is clean
			cache_block(block_index)(133 downto 128) <= tag;
			from_writeback := 0;
			next_state <= write;
		end if;

	when read_load =>
		-- when memory not idle 
        -- and also we are not waiting for the previous read to respond (read_issued = 0)
		-- we can issue a read but do data transfer
		if m_waitrequest = '1' and read_in_process = 0 and counter < 16 then
			m_read <= '1';	-- issue a read
			m_addr <= to_integer(unsigned(s_addr(14 downto 4)))*16 + counter;
			read_in_process := 1;	-- indicating a read is in progress
			next_state <= read_load;
		-- if the memory is not idle, but a read is issued
		-- then we keep waiting for that read
		elsif m_waitrequest = '1' and read_in_process = 1 then
			next_state <= read_load;
		-- when memory responds
		-- we can get the memory output byte into cache
        -- We need load 16 bytes into the cache block
		elsif m_waitrequest = '0' and counter < 16 then
			cache_block(block_index)( (counter+1)*8 -1 downto counter*8) <= m_readdata;
			counter := counter + 1;
			m_read <= '0';
			read_in_process := 0;	-- this read is done, need to reset the flag to trigger the next read
			next_state <= read_load;
			
		-- when all 16 read finished, set flag to valid and clean, and set the new tag in cache
		elsif counter = 16 then
			counter := 0;
			cache_block(block_index)(133 downto 128) <= tag;
            cache_block(block_index)(135) <= '1'; -- valid
			cache_block(block_index)(134) <= '0'; -- dirty bit is clean	
			next_state <= read;
		end if;


	end case;
        

end process;

end arch;