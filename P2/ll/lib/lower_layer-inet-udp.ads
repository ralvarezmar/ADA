-- $Id: lower_layer-inet-udp.ads,v 1.11 1997/03/13 21:43:30 jgb Exp $
--
------------------------------------------------------------------------------
--                                                                          --
--    Copyright (C) 1997  Jesus M. Gonzalez Barahona                        --
--                                                                          --
--    This library is free software; you can redistribute it and/or         --
--    modify it under the terms of the GNU Library General Public           --
--    License as published by the Free Software Foundation; either          --
--    version 2 of the License, or (at your option) any later version.      --
--                                                                          --
--    This library is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     --
--    Library General Public License for more details.                      --
--                                                                          --
--    You should have received a copy of the GNU Library General Public     --
--    License along with this library; see file COPYING.LIB. If not,        --
--    write to the Free Software Foundation, Inc., 675 Mass Ave,            --
--    Cambridge, MA 02139, USA.                                             --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This package provides type Address, which is the root for all
--  types for UDP protocols (unicast and multicast UDP, for now).
------------------------------------------------------------------------------

package Lower_Layer.Inet.UDP is

   type Address is abstract new Lower_Layer.Inet.Address with private;

private

   type Address is abstract new Lower_Layer.Inet.Address with null record;

   -- Bind_Address will do the same thing for unicat and multicast.
   --
   procedure Bind_Address (An_Address: in Address;
                           Handler:    in Handler_A;
                           A_Socket:   in Ada_Sockets.Socket);

   -- Unbind_Address will do the same thing for unicat and multicast.
   --
   procedure Unbind_Address (An_Address: in Address);

   -- Both Send and Receive can be defined here (same things are done
   --  for unicast and multicast).

   procedure Send
     (To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1);

   procedure Send_From
     (From:   in     Address;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1);



   type Recv_Info is new Inet.Recv_Info with record
      Socket: Ada_Sockets.Socket;
   end record;

   procedure Receive
     (From : out Address_CA;
      To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset);


end Lower_Layer.Inet.UDP;
