library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bfm;
use bfm.avalon_source_bfm_pkg.all;

library types;
use types.bus_data_types_pkg.all;

library test;
package test_ctrl_pkg is
  component test_ctrl is
    port (
      -- Basic clocking
      clk   : in std_ulogic;
      reset : in std_ulogic;

      -- Signals that request data streams
      new_stream_request : out bfm.avalon_source_bfm_pkg.SOURCE_REQUEST_t;
      new_stream_ack     : in  types.bus_data_types_pkg.ACK_t;

      -- Forked signals we monitor for correctness
      check_forked_packets : in types.bus_data_types_pkg.AVALON_PACKET_ARRAY_t;
      check_forked_acks : in types.bus_data_types_pkg.ACK_ARRAY_t;     

      -- Pause information
      sink_pause_pct : out natural range 0 to 99
      );
  end component;

end package;

library types;
use types.bus_data_types_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

-- Create a specialisation of the scoreboard type.
package data_stream_scoreboard_pkg is new
  osvvm.ScoreBoardGenericPkg
    generic map (
      ExpectedType => types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t,
      ActualType => types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t,
      match => types.bus_data_types_pkg.packet_equality,
      expected_to_string => types.bus_data_types_pkg.packet_to_text,
      actual_to_string => types.bus_data_types_pkg.packet_to_text
      );

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bfm;
use bfm.avalon_source_bfm_pkg.all;

library types;
use types.bus_data_types_pkg.all;

library osvvm;
context osvvm.OsvvmContext;
                                      
use work.data_stream_scoreboard_pkg.all;                                      

entity test_ctrl is
  port (
    -- Basic clocking
    clk   : in std_ulogic;
    reset : in std_ulogic;

    -- Signals that request data streams
    new_stream_request : out bfm.avalon_source_bfm_pkg.SOURCE_REQUEST_t;
    new_stream_ack     : in  types.bus_data_types_pkg.ACK_t;

    -- Forked signals we monitor for correctness
    check_forked_packets : in types.bus_data_types_pkg.AVALON_PACKET_ARRAY_t;
    check_forked_acks : in types.bus_data_types_pkg.ACK_ARRAY_t;
    
    -- Pause information
    sink_pause_pct : out natural range 0 to 99    
    );
end test_ctrl;

-- We really want a number of architectures here, each performing its own
-- set of tests. For example, one might test back to back tiny packets.
-- A configuration can switch between them at runtime.

-- This architecture spits out a number of totally random new stream requests.
-- Once we've checked it's doing the right thing by eye we can add
-- scoreboarding to check it automatically and then coverage to decide what
-- range of tests we are particularly interested in covering.
architecture Random_Stream_Test of test_ctrl is

  shared variable SB : work.data_stream_scoreboard_pkg.ScoreBoardPType;
  
  procedure set_pause_rate( desired_pause : in natural range 0 to 99;
                            signal pause : out natural range 0 to 99) is
  begin
    report "Changing pause rate to " & to_string(desired_pause);
    pause <= desired_pause;
  end procedure;  

  procedure random_request( signal req : out bfm.avalon_source_bfm_pkg.SOURCE_REQUEST_t) is
    variable RV : RandomPType;
  begin
    req.transaction_id <= RV.RandSlv(0, 4096, req.transaction_id'length);
    req.packet_count <= RV.RandInt(1, 50);
    req.pause_pct <= RV.RandInt(0,99);
  end procedure;

  procedure init_transaction( signal valid : out std_ulogic;
                              signal ack : in std_ulogic) is
  begin
    valid <= '1';
    WaitForLevel(ack, '1');
    WaitForClock(clk, 1);
    valid <= '0';
  end procedure;

  procedure scoreboard_request( signal req : in bfm.avalon_source_bfm_pkg.SOURCE_REQUEST_t) is
    variable pkt : types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t(data(check_forked_packets(0).data'range), transaction_id(check_forked_packets(0).transaction_id'range));
  begin
    -- A single request will probably generate many checks. Calculate the
    -- likely values and submit them here
    wait for 0 ns;
    for i in 0 to req.packet_count-1 loop
      pkt.start := '1' when i < check_forked_acks'length else '0';
      pkt.stop := '1' when i >= (req.packet_count-check_forked_acks'length) else '0';
      pkt.valid := '1';
      pkt.data := std_ulogic_vector(to_unsigned(i, pkt.data'length));
      pkt.transaction_id := req.transaction_id;

      SB.Push(i mod check_forked_acks'length, pkt);
    end loop;
  end procedure;
begin
  
  stimulus_gen : process is
  begin
    -- Setup Scoreboard index
    SB.SetArrayIndex(0, check_forked_acks'length-1);
    
    -- Wait until we are out of reset
    WaitForLevel(reset, '0');

    -- Set a plausible pause rate for our sinks
    set_pause_rate(50, sink_pause_pct);
    
    -- Generate a set of stream request
    for i in 0 to 1000 loop
      random_request(new_stream_request);
      scoreboard_request(new_stream_request);
    
      -- Issue the transaction
      init_transaction(new_stream_request.valid, new_stream_ack.ack);
    end loop;
    
    -- Allow sufficient time for data to get through
    WaitForClock(clk, 1000);

    -- Report success
    report "Well, that's good for now" severity note;
    std.env.stop;    
  end process;

  monitor : process(clk) is
  begin
    if rising_edge(clk) then
      for i in check_forked_acks'length-1 downto 0 loop
        if check_forked_acks(i).ack then
          -- Data destined for one of our sinks is being consumed.
          -- Ask the scoreboard if it matches expectations
          SB.check(i, check_forked_packets(i));
        end if;
      end loop;
    end if;
  end process;
end architecture;
