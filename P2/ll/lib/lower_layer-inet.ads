-- $Id: lower_layer-inet.ads,v 1.21 1998/01/19 12:46:51 jgb Exp $
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
-- This package provides an Address for all the Inet protocols.
--  Currently it supports access to the protocols through BSD
--  style sockets.
------------------------------------------------------------------------------

-- Used in private section.
with Ada_Sockets;

-- Used in private section.
with Keyed_Lists_Generic;

-- Used in private section.
with Concurrency;

package Lower_Layer.Inet is

--   pragma Elaborate_Body;

   type Address is abstract new Lower_Layer.Address with private;

   -- Builds an Address from a valid IP number and a port.
   -- !!!: Cannot be a function because Address is abstract,
   --      and in that case the function should also be abstract.
   --
   procedure Build (An_Address: out Address;
                    IP:         in  String;
                    Port:       in  Natural);

   --
   -- Subprograms for binding/unbinding an address to a socket.
   --

   procedure Bind (An_Address: in     Address;
                   A_Socket:   in out Ada_Sockets.Socket) is abstract;

   procedure Bind_Any (An_Address: out    Address;
                       A_Socket:   in out Ada_Sockets.Socket) is abstract;

   procedure Unbind (An_Address: in Address;
                     A_Socket:   in Ada_Sockets.Socket) is abstract;

   -- Code common to Bind and Bind_Any (insertion in lists and so on).
   --
   procedure Bind_Address (An_Address: in Address;
                           Handler:    in Handler_A;
                           A_Socket:   in Ada_Sockets.Socket)
      is abstract;

   -- Insertion in lists and so on for Unbind.
   --
   procedure Unbind_Address (An_Address: in Address) is abstract;

   -- Send data to address through socket.
   --
   procedure Send
     (To_Addr: in     Address'Class;
      To_Sock: in     Ada_Sockets.Socket;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in     Ada.Streams.Stream_Element_Offset;
      First:   in     Ada.Streams.Stream_Element_Offset := 1);

   -- My primary host number.
   --
   My_IP: constant Ada_Sockets.Host_No;
   My_IP_String: constant String;

--private

   -- Is the Address valid? (used by Build to decide if an
   --  exception should be raised.
   --
   function Valid (An_Address: in Address) return Boolean is abstract;

   -- For comparing (and ordering) addresses.
   --
   function ">=" (Left, Right: Address) return Boolean;

   procedure Bind (An_Address: in Address;
                   Handler:    in Handler_A := null);

   procedure Bind_Any (An_Address: out Address;
                       Handler:    in Handler_A := null);

   procedure Unbind (An_Address: in Address);

   procedure Receive
     (From: out Address_CA;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in out Ada.Streams.Stream_Element_Offset);

   -- For getting a printable string.
   --
   function Image (An_Address: in Address) return String;

private

   -- The type for addresses is the one provided by Inet_Addr.
   --
   type Address is abstract new Lower_Layer.Address with record
      IP:   Ada_Sockets.Host_No;
      Port: Ada_Sockets.Port_No;
   end record;

   procedure Refresh_Receiver_Address;

   -- Capacity of addresses and socket lists.
   --
   Lists_Capacity: Positive := 30;

   -----------------------------------------------------------------------
   -- Address_Info.
   --
   -- Information related to an address. Root of this type, will be
   --  extended for UDP and TCP protocols.
   -----------------------------------------------------------------------

   type Address_Info is abstract tagged null record;

   function Image (Info: in Address_Info) return String is abstract;

   function Image_Class (Info: in Address_Info'Class) return String;

   type Address_Info_CA is access all Address_Info'Class;

   function Is_Null (Info: in Address_Info_CA) return Boolean;

   procedure Free (Info: in out Address_Info_CA);

   function Image (Info: in Address_Info_CA) return String;

   -----------------------------------------------------------------------
   -- Recv_Info.
   --
   -- Information related to an address used for receiving.
   -----------------------------------------------------------------------

   type Recv_Info is abstract new Address_Info with record
      Address:     Address_CA;
      The_Handler: Handler_A;
   end record;

   -- Receive data, destinated to address, through socket.
   --
   procedure Receive
     (From: out Address_CA;
      To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset) is abstract;

   function Image (Info: in Recv_Info) return String;

   function Image_Class (Info: in Recv_Info'Class) return String;

   type Recv_Info_CA is access all Recv_Info'Class;

   procedure Free (Info: in out Recv_Info_CA);

   function Image (Info: in Recv_Info_CA) return String;

   -----------------------------------------------------------------------
   -- Recv_Address: List of receiving addresses.
   --
   --  When a new address for receiving is to be bound (Bind
   --  operation), it is added to the list. When it is
   --  unbound (Unbind operation), it is removed. When
   --  a Receive is issued, a lookup in the list will
   --  be done, to find the appropiate address info.
   -----------------------------------------------------------------------

   -- Instantiations of keyed lists.
   --
   package Addr_Lists is new Keyed_Lists_Generic
     (Key           => Address_CA,
      Element       => Recv_Info_CA,
      Capacity      => Lists_Capacity,
      Image_Key     => Image,
      Image_Element => Image);

   -- List of receiving addresses.
   Recv_Addresses: Addr_Lists.Keyed_List;

   -----------------------------------------------------------------------
   -- Management of receiving sockets (only for addresses with handler).
   --
   --  Address_Info for addresses with handler are stored in a list,
   --  keyed by socket.
   --  When a socket is ready for reading, the corresponding
   --  address is searched in this list.
   -----------------------------------------------------------------------

   -- We need visibility of "=" and ">=" for next instantiation.
   --
   use type Ada_Sockets.Socket;

   package Sock_Lists is new Keyed_Lists_Generic
     (Key           => Ada_Sockets.Socket,
      Element       => Recv_Info_CA,
      Capacity      => Lists_Capacity,
      Image_Key     => Ada_Sockets.Image,
      Image_Element => Image);

   -- List of receiving sockets (for receiver task, have
   --  registered handler)
   --
   Recv_Sockets:      Inet.Sock_Lists.Keyed_List;

   -- List of addresses for receiver task changed?
   --
   Recv_Address_Change: Boolean := False;

   -- Receiver task must finalize (or not).
   --
   Recv_Finalize: Boolean := False;

   -- Receiver task, for addresses with handler.
   --
   task type Receiver_Task is

      -- XXX: For sure it's unnecesary such a large stack.
      --
      pragma Storage_Size (400_000);

      entry Finished;
   end Receiver_Task;

   type Receiver_Task_A is access Receiver_Task;

   The_Receiver: Receiver_Task_A;

   -- Mutex for ensuring that no two tasks access simultaneously
   --  to receiver information.
   --
   The_Mutex: Concurrency.Mutex;

   -- Dummy socket (and its address) used for unhang receiver by
   --  sending it a message.
   --
   Dummy_Socket:  Ada_Sockets.Socket;
   Dummy_Address: Address_CA;

   -- Control exclusive access to dummy socket creation.
   --
   Dummy_Mutex:   Concurrency.Mutex;


   --   Receive_Buffer_Size: constant := 5_000;
   -- Maximum data size for a buffer, measured in bytes:
   --     Max_IP_dgram_size - (IP_hdr_size + UDP_hdr_size)
   Receive_Buffer_Size: constant := 65535 - (20+8);

   ------------------------------------------------------
   --
   -- Some constants.
   --
   ------------------------------------------------------

   -- My primary host number.
   --
   My_IP: constant Ada_Sockets.Host_No :=
     Ada_Sockets.Get_Addr_By_Name (Ada_Sockets.Get_Host_Name);

   My_IP_String: constant String := Ada_Sockets.To_String (My_IP);

   -- First and last multicast (class D) address.
   --
   First_IP_Multi: constant Ada_Sockets.Host_No :=
     Ada_Sockets.To_Host ("224.0.0.0");
   Last_IP_Multi:  constant Ada_Sockets.Host_No :=
     Ada_Sockets.To_Host ("239.255.255.255");

end Lower_Layer.Inet;
