-- $Id: lower_layer-inet-tcp-misc.adb,v 1.3 1997/03/20 00:09:27 jgb Exp $
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

with Interfaces.C;
with C_Sockets;

package body Lower_Layer.Inet.TCP.Misc is

   package C renames Interfaces.C;

   package Listen_Lists is new Keyed_Lists_Generic
     (Key           => Address,
      Element       => Ada_Sockets.Socket,
      Capacity      => Lists_Capacity,
      Image_Key     => Image,
      Image_Element => Ada_Sockets.Image);

   Listen_List: Listen_Lists.Keyed_List;

   procedure Connect (An_Address: in Address'Class;
                      A_Connection: out Connection) is

   begin
      Ada_Sockets.Create_Socket (Ada_Sockets.TCP,
                                 A_Connection.Socket);
      Ada_Sockets.Connect_Socket (A_Connection.Socket,
                                  Inet.Address (An_Address).IP,
                                  Inet.Address (An_Address).Port);
   end Connect;

   procedure Dispose (A_Connection: in out Connection) is

   begin
      Ada_Sockets.Destroy_Socket (A_Connection.Socket);
   end Dispose;

   procedure Listen_Connection (An_Address: in Address'Class) is

      A_Socket: Ada_Sockets.Socket;
      Done:     Boolean;
   begin
      Listen_Lists.Search (Listen_List, Address (An_Address), Done);
      if not Done then
         -- Not found, bind and insert a new socket.
         Ada_Sockets.Create_Socket (Ada_Sockets.TCP,
                                    A_Socket);
         Ada_Sockets.Bind_Socket (A_Socket,
                                  Inet.Address (An_Address).IP,
--                                  Ada_Sockets.Void_Host,
                                  Inet.Address (An_Address).Port);
         Ada_Sockets.Listen_Socket (A_Socket);
         Listen_Lists.Insert (Listen_List, Address (An_Address),
                              A_Socket, Done);
      else
         raise Already_Listening;
      end if;
   end Listen_Connection;

   procedure Wait_Connection (An_Address: in Address'Class;
                              A_Connection: out Connection) is

      Listen_Socket: Ada_Sockets.Socket;
      Done:          Boolean;
   begin
      Listen_Lists.Get_Element (Listen_List, Address (An_Address),
                                Listen_Socket, Done);
      if not Done then
         -- Not found, raise an exception
         raise Not_Listening;
      else
         Ada_Sockets.Accept_Socket (Listen_Socket, A_Connection.Socket);
      end if;
   end Wait_Connection;

   procedure Read
     (A_Connection: in out Connection;
      Data:         out    Ada.Streams.Stream_Element_Array;
      Length:       out    Ada.Streams.Stream_Element_Offset) is

      use type C.Int;
      use type Ada.Streams.Stream_Element_Offset;

      -- We nned an array for receiving, because C_Sockets.Recv
      --  expects an access to Ada.Streams.Stream_Element_Array,
      --  so it must be aliased.
      -- XXX: This forces an unneeded copy, but I don't see how
      --      to easily avoid it.
      Recv_Data: Ada.Streams.Stream_Element_Array (Data'Range) :=
        (others => 0);
      -- No of bytes received.
      Received:      C.Int;
   begin
      -- Receive data.
      Received := C_Sockets.Recv (C.Int (A_Connection.Socket),
                                  Recv_Data,
                                  Recv_Data'Length, 0);
      -- Check errors.
      if Received < 0 then
         raise Receiving_Error;
      end if;
      Length := Ada.Streams.Stream_Element_Offset (Received);
      Data (Data'First .. Data'First + Length - 1) :=
        Recv_Data (Recv_Data'First .. Recv_Data'First + Length - 1);
   end Read;

   procedure Write
     (A_Connection: in out Connection;
      Data:         in     Ada.Streams.Stream_Element_Array) is

      use type C.Int;

      Sent: C.Int;
   begin
      Sent := C_Sockets.Send (C.Int (A_Connection.Socket),
                              Data, Data'Length, 0);
      if Sent /= C.Int (Data'Length) then
         raise Sending_Error;
      end if;
   end Write;

end Lower_Layer.Inet.TCP.Misc;
