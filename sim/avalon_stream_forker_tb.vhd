library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library utility;
use utility.avalon_stream_forker_pkg.all;

library bfm;
use bfm.avalon_source_bfm_pkg.all;
use bfm.avalon_sink_bfm_pkg.all;

library types;
use types.bus_data_types_pkg.all;

library test;
use test.test_ctrl_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity avalon_stream_forker_tb is
  generic (
    data_bits        : positive := 32;
    transaction_bits : positive := 32;
    ports            : positive := 4
    );
end avalon_stream_forker_tb;

architecture test of avalon_stream_forker_tb is

  signal clk   : std_ulogic;
  signal reset : std_ulogic;

  -- Comms between the test generator and the source model
  signal test_request     : bfm.avalon_source_bfm_pkg.SOURCE_REQUEST_t(transaction_id(transaction_bits-1 downto 0));
  signal test_request_ack : types.bus_data_types_pkg.ACK_t;

  -- Packets streaming out of the generator toward our DUT
  signal gen_packet     : types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t(data(data_bits-1 downto 0), transaction_id(transaction_bits-1 downto 0));
  signal gen_packet_ack : types.bus_data_types_pkg.ACK_t;

  -- Post DUT packets
  signal forked_packets : types.bus_data_types_pkg.AVALON_PACKET_ARRAY_t(ports-1 downto 0)(data(data_bits-1 downto 0), transaction_id(transaction_bits-1 downto 0));
  signal forked_acks    : types.bus_data_types_pkg.ACK_ARRAY_t(ports-1 downto 0);

  -- Comms between test generator and the sink models
  signal pause_pct : natural range 0 to 99;

begin

  -- Basic clock and reset behaviour.
  CreateClock(clk, 5 ns, 0.5);
  reset <= '1', '0' after 20 ns;

  -- Create the test controller that controls what we are testing. There are
  -- many architectures for this guy, each representing a different test. We
  -- can choose between them via configurations if we wish. The controller
  -- sends requests to the models and monitors results. Internally a
  -- scoreboarding system will be checking if everything has worked as expected.
  controller : test.test_ctrl_pkg.test_ctrl
    port map (
      clk                  => clk,
      reset                => reset,
      new_stream_request   => test_request,
      new_stream_ack       => test_request_ack,
      check_forked_packets => forked_packets,
      check_forked_acks    => forked_acks,
      sink_pause_pct       => pause_pct
      );

  -- A model of a data stream generator. The test controller will make
  -- requests to this generator which will then perform the required bit
  -- twiddling for us
  generator : bfm.avalon_source_bfm_pkg.avalon_source_bfm
    port map (
      clk             => clk,
      reset           => reset,
      stream_req      => test_request,
      stream_req_resp => test_request_ack,
      packet          => gen_packet,
      packet_ack      => gen_packet_ack
      );

  -- The device under test
  dut : utility.avalon_stream_forker_pkg.avalon_stream_forker
    port map (
      clk         => clk,
      reset       => reset,
      in_packet   => gen_packet,
      in_ack      => gen_packet_ack,
      out_packets => forked_packets,
      out_acks    => forked_acks
      );

  -- Sink the data into these endpoints. We leave validating things to the
  -- test_ctrl which monitors these busses for us
  sinks : for i in 0 to ports-1 generate
    sink : bfm.avalon_sink_bfm_pkg.avalon_sink_bfm
      port map (
        clk        => clk,
        reset      => reset,
        pause_pct  => pause_pct,
        packet     => forked_packets(i),
        packet_ack => forked_acks(i)
        );
  end generate sinks;

  -- Wait around. If we exceed some maximum time, decide that the test has
  -- failed and get us out of simulation.
  timeout : process
  begin
    WaitForClock(clk, 100000);
    report "Reached timeout condition" severity failure;
    std.env.stop;
  end process;
  
end architecture;
