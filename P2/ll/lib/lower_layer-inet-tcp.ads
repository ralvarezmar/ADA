-- $Id: lower_layer-inet-tcp.ads,v 1.8 1997/05/13 18:51:52 jgb Exp $
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
-- This package provides type Address for TCP protocol.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package Lower_Layer.Inet.TCP is

   type Address is new Lower_Layer.Inet.Address with private;

private

   type Address is new Lower_Layer.Inet.Address with null record;

   procedure Send
     (To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1);


   procedure Send_From
     (From:     in     Address;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1);

   procedure Bind_Address (An_Address: in Address;
                           Handler:    in Handler_A;
                           A_Socket:   in Ada_Sockets.Socket);

   procedure Unbind_Address (An_Address: in Address);

   procedure Bind (An_Address: in     Address;
                   A_Socket:   in out Ada_Sockets.Socket);

   procedure Bind_Any (An_Address: out    Address;
                       A_Socket:   in out Ada_Sockets.Socket);

   procedure Unbind (An_Address: in Address;
                     A_Socket:   in Ada_Sockets.Socket);

   function Valid (An_Address: in Address) return Boolean;

   --  First element of Sockets will be used for listening, the
   --  rest of them for receiving.
   --
   type Recv_Info is new Inet.Recv_Info with record
      Sockets: Ada_Sockets.Socket_Array_A;
   end record;

   procedure Receive
     (From: out Address_CA;
      To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset);

   procedure Receive
     (To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset);

   -- List of sending sockets.
   --
   type Send_Info is new Inet.Address_Info with record
      Socket: Ada_Sockets.Socket;
   end record;

   function Image (Info: in Send_Info) return String;

   function Image_Class (Info: in Send_Info'Class) return String;

   type Send_Info_CA is access all Send_Info'Class;

   procedure Free (Info: in out Send_Info_CA);

   function Image (Info: in Send_Info_CA) return String;

   -----------------------------------------------------------------------
   -- Send_Address: List of destination addresses.
   -----------------------------------------------------------------------

   -- Instantiations of keyed lists.
   --
   package Addr_Lists is new Keyed_Lists_Generic
     (Key           => Address_CA,
      Element       => Send_Info_CA,
      Capacity      => Lists_Capacity,
      Image_Key     => Image,
      Image_Element => Image);

   -- List of destination addresses.
   Send_Addresses: Addr_Lists.Keyed_List;

end Lower_Layer.Inet.TCP;
