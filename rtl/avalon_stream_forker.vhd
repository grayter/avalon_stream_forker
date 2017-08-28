library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library types;
use types.bus_data_types_pkg.all;

library utility;

package avalon_stream_forker_pkg is
  component avalon_stream_forker
    port (
      -- Basic clocking
      clk     : in  std_ulogic;
      reset   : in  std_ulogic;
      -- Incoming stream to fork
      in_packet : in types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t;
      in_ack     : out types.bus_data_types_pkg.ACK_t;
      -- Outgoing streams
      out_packets : out types.bus_data_types_pkg.AVALON_PACKET_ARRAY_t;
      out_acks : in types.bus_data_types_pkg.ACK_ARRAY_t
      );
  end component;
end avalon_stream_forker_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.math_real.all;

library types;
use types.bus_data_types_pkg.all;

library utility;

entity avalon_stream_forker is
  port (
      -- Basic clocking
      clk     : in  std_ulogic;
      reset   : in  std_ulogic;
      -- Incoming stream to fork
      in_packet : in types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t;
      in_ack     : out types.bus_data_types_pkg.ACK_t;
      -- Outgoing streams
      out_packets : out types.bus_data_types_pkg.AVALON_PACKET_ARRAY_t;
      out_acks : in types.bus_data_types_pkg.ACK_ARRAY_t
    );
end avalon_stream_forker;

-- In a four output system for example, the fact that you just wrote the last
-- output port means that the first output port must be valid. You don't wait
-- until you get more data.
--
--   Cycle
-- O a  A    e   E
-- u  b      B   X
-- t   c         C
-- P    d        D
--

architecture careful_cycling of avalon_stream_forker is
  type state_t is (INGEST, INGEST_PUSH);
  signal state : state_t;

  constant ports : integer := out_acks'length;

  signal hungry : std_ulogic_vector(ports-1 downto 0);
  constant port_bits : integer := integer(ceil(log2(real(ports))));
  signal counter     : unsigned(port_bits-1 downto 0);  
begin

  -- Create helper hungry signal
  hungry_gen : for i in ports-1 downto 0 generate
    hungry(i) <= not out_packets(i).valid or out_acks(i).ack;
  end generate;
  
  -- Handle data acceptance
  in_ack.ack <= '1' when (hungry(to_integer(counter)) = '1' and in_packet.valid = '1') else '0';
  
  process(clk, reset) is
  begin
    if rising_edge(clk) then
      -- Handle the valid and ack default behaviour
      for i in ports-1 downto 0 loop
        out_packets(i).valid <= out_packets(i).valid and not out_acks(i).ack;
      end loop;
      
      case state is
        when INGEST =>
          if hungry(to_integer(counter)) and in_packet.valid  then
            -- Ingest mode is only for the first batch of inputs. Thus,
            -- everything gets to be a start on its turn by definition
            out_packets(to_integer(counter)).start <= '1';

            -- Stops are almost certainly just 0 here, but it's possible that
            -- an i_stop would need special handling (see below)
            out_packets(to_integer(counter)).stop <= '0';

            -- Data just moves across
            out_packets(to_integer(counter)).data <= in_packet.data;
            out_packets(to_integer(counter)).transaction_id <= in_packet.transaction_id;

            -- Move the counter on
            counter <= counter + 1;

            -- Are we putting the final port of data across? If so, that means
            -- we need to set port 0 to actually output, reset counter and move
            -- state
            if counter = (ports-1) then
              out_packets(0).valid <= '1';
              counter <= to_unsigned(0, port_bits);
              state <= INGEST_PUSH;
            end if;

            -- Though note, in some circumstances a stop will occur within this
            -- stage. In that case, output everything we have input so far and
            -- reset the counter. Note o_stop gets fully set - any other
            -- transactions that aren't ack'd yet will have been stops so this
            -- is just being lazy :) Given that we see a stop, we remain in
            -- this state.
            if in_packet.stop then
              for i in ports-1 downto 0 loop
                out_packets(i).valid <= '1' when i <= counter else (out_packets(i).valid and not out_acks(i).ack);
                out_packets(i).stop <= '1';
              end loop;

              counter <= to_unsigned(0, port_bits);
              state <= INGEST;
            end if;
          end if;

        when INGEST_PUSH =>
          -- If we've got to here we are now playing a game of one in one out.
          -- Whenever downstream has consumed the data we had previously given
          -- it, we can fill in data for our current counter value and let the
          -- next counter value be output. Once we get a stop, we let all
          -- values be output
          if hungry(to_integer(counter)) and in_packet.valid then
            -- Since we are in INGEST_PUSH we are definitely mid transaction.
            -- There is no way i_start is allowed to be high, so o_start is now
            -- just taking 0
            out_packets(to_integer(counter)).start <= '0';

            -- Data just marches on through
            out_packets(to_integer(counter)).data <= in_packet.data;
            out_packets(to_integer(counter)).transaction_id <= in_packet.transaction_id;            

            -- Since we are inputting data, that means the next port is now
            -- clear to output with its given settings (though if we are
            -- currently a stop we might be making additional changes below).
            -- TODO: Verify the synth behaviour of the mod here. In theory it's
            -- free, but...
            out_packets(to_integer((counter+1) mod ports)).valid <= '1';

            -- So we've either just moved into this state, or we've been
            -- looping for some time. Either way, the value of o_stop doesn't
            -- need changing until we get an i_stop. In that situation we can
            -- blanket the change out. This means that in this state we can
            -- just always take i_stop out to everyone
            for i in ports-1 downto 0 loop
              out_packets(i).stop <= in_packet.stop;
            end loop;

            -- Counter handling. Normally it increments, but stops cause it to
            -- reset. On a stop, we also move state and need to mark more
            -- streams as being valid.           
            if in_packet.stop then
              counter <= to_unsigned(0, port_bits);
              for i in ports-1 downto 0 loop
                out_packets(i).valid <= '1';
              end loop;
              state <= INGEST;
            else
              if counter = (ports-1) then
                counter <= to_unsigned(0, port_bits);
              else
                counter <= counter + 1;
              end if;
            end if;            
          end if;         
      end case;
    end if;

    if reset then
      state <= INGEST;
      counter <= to_unsigned(0, port_bits);
      for i in ports-1 downto 0 loop
        out_packets(i).valid <= '0';
      end loop;
    end if;
  end process;  
  
end architecture;
