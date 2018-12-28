-- $Id: lower_layer-inet-tcp.adb,v 1.11 1997/12/30 11:21:29 jgb Exp $
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

-- !!!: When sending messages, the length of the array to send is
--      previously written to the TCP socket. This allows the receiver
--      to get first the length of the expected data. TCP doesn't
--      guarantee that messages sent in a unique Sendto operation
--      are received in a unique Recvfrom operation. Partial messages
--      can be received. More than one message can also be received.
--      The length will be used on receiving for reading from the
--      socket exactly the amount of bytes expected.
--
-- XXX: The sending of the data length could be done better. For
--      instance, four bytes, at the beggining of the data to send,
--      could be used fo rstoring that length. Or better, the
--      scattering/gathering version of Send could be used.
--

with Ada.Task_Identification;

with Misc_Util_Accesses;
with Misc_Util;
with Interfaces.C;
with C_Sockets;

with Debugging;

package body Lower_Layer.Inet.TCP is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := True;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Inet.TCP";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   package C renames Interfaces.C;


   ----------------------------------------------------------------------
   -- Image (Send_Info).
   ----------------------------------------------------------------------

   function Image (Info: in Send_Info) return String is

   begin
      return "Socket: " & Ada_Sockets.Image (Info.Socket);
   end Image;

   ----------------------------------------------------------------------
   -- Image_Class (Send_Info'Class).
   ----------------------------------------------------------------------

   function Image_Class (Info: in Send_Info'Class) return String is

   begin
      return Image (Info);
   end Image_Class;

   ----------------------------------------------------------------------
   -- Free (Send_Info_CA).
   ----------------------------------------------------------------------

   procedure Free (Info: in out Send_Info_CA) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Send_Info'Class,
         Name   => Send_Info_CA);

   begin
      if not Is_Null (Address_Info_CA (Info)) then
         Deallocate (Info);
      end if;
   end Free;

   ----------------------------------------------------------------------
   -- Image (Send_Info_CA).
   ----------------------------------------------------------------------

   function Image (Info: in Send_Info_CA) return String is

      function Image_CA is new Misc_Util_Accesses.Image_CA
        (Object => Send_Info,
         Name   => Send_Info_CA);
   begin
      return Image_CA (Info);
   end Image;

   ---------------------------------------------------------------------
   -- Bind_Address.
   --  Performs the code common to Bind and Bind_Any.
   ---------------------------------------------------------------------

   procedure Bind_Address (An_Address: in Address;
                           Handler:    in Handler_A;
                           A_Socket:   in Ada_Sockets.Socket) is

      The_Address:  Address_CA;
      Info:         Recv_Info_CA;
      Done:         Boolean;
   begin
      The_Address := new Address'Class'(Address'Class (An_Address));
      Info := new Recv_Info'
        ((Address     => The_Address,
          The_Handler => Handler,
          Sockets     => new Ada_Sockets.Socket_Array'(1 => A_Socket)));
      -- We're going to touch data structures for bound addresses,
      --  so we have to protect against concurrent access.
      The_Mutex.Enter (Ada.Task_Identification.Current_Task);
      Inet.Addr_Lists.Insert (Recv_Addresses,
                              The_Address,
                              Info, Done);
      if not Done then
         -- The address was already in the list (unlock mutex).
         The_Mutex.Leave;
         raise Lower_Layer.Binding_Error;
      end if;
      if Handler /= null then
         -- We have a handler.
         Sock_Lists.Insert (Recv_Sockets,
                            A_Socket,
                            Info, Done);
         Inet.Refresh_Receiver_Address;
         Recv_Address_Change := True;
      end if;
      The_Mutex.Leave;
   end Bind_Address;

   ---------------------------------------------------------------------
   -- Unbind_Address.
   -- !!!: It is convenient to have this procedure, which does
   --      all the work of unbinding that must be locked.
   --      The lock must set and released by the caller.
   --      This allows not only Unbind to call this code (for
   --      instance, it's also called by termination of
   --      receiver task.
   ---------------------------------------------------------------------

   procedure Unbind_Address (An_Address: in Address) is

      -- Address for looking for receiving socket.
      Removed_Info: Recv_Info_CA;
      Found:        Boolean;
      The_Address:  Address_CA;
   begin
      The_Address := new Address'Class'(Address'Class (An_Address));
      Inet.Addr_Lists.Remove (Recv_Addresses,
                              The_Address,
                              Found,
                              Removed_Info);
      -- The_Address no longer needed.
      Free (The_Address);
      if not Found then
         raise Lower_Layer.Unbinding_Error;
      end if;
      Free (Removed_Info.Address);
      if Removed_Info.The_Handler /= null then
         -- We have handler, let's remove sockets from Recv_Sockets.
         for Count in Recv_Info (Removed_Info.all).Sockets.all'Range loop
            Sock_Lists.Remove (Recv_Sockets,
                               Recv_Info (Removed_Info.all).Sockets (Count),
                               Found);
         end loop;
         if Found then
            -- If the socket was found, the receiver task is in charge of it
            --  (address with handler).
            Recv_Address_Change := True;
            Refresh_Receiver_Address;
         end if;
      end if;
      for Count in Recv_Info (Removed_Info.all).Sockets.all'Range loop
         Unbind (Address'Class (An_Address),
                 Recv_Info (Removed_Info.all).Sockets (Count));
         Ada_Sockets.Destroy_Socket
           (Recv_Info (Removed_Info.all).Sockets (Count));
      end loop;
      Free (Removed_Info);
   exception
      when others =>
         raise Lower_Layer.Unbinding_Error;
   end Unbind_Address;

   -------------------------------------------------------------------
   -- Bind.
   --  Raises Binding_Error when Insert fails (returns False,
   --  that is, there was a socket in the
   --  list with the same address).
   --
   --  XXX: It should be checked that port is not 0. It it
   --       is, the user should call Bind_Any, not Bind.
   -------------------------------------------------------------------

   procedure Bind (An_Address: in     Address;
                   A_Socket:   in out Ada_Sockets.Socket) is

      -- We need "=".
      use type Ada_Sockets.Host_No;

   begin
      -- Let's open the socket.
      Ada_Sockets.Create_Socket (Ada_Sockets.TCP, A_Socket);
      if Inet.My_IP /= Inet.Address (An_Address).IP then
         raise Lower_Layer.Binding_Error;
      end if;
      -- Let's bind the socket to the port.
      --
      Ada_Sockets.Bind_Socket (A_Socket,
                               Inet.Address (An_Address).IP,
                               Inet.Address (An_Address).Port);
      Ada_Sockets.Listen_Socket (A_Socket);
   end Bind;

   -------------------------------------------------------------------
   -- Bind_Any.
   --  Works like Bind.
   -------------------------------------------------------------------

   procedure Bind_Any (An_Address: out    Address;
                       A_Socket:   in out Ada_Sockets.Socket) is

   begin
      -- Let's open the socket.
      Ada_Sockets.Create_Socket (Ada_Sockets.TCP, A_Socket);
      Inet.Address (An_Address).IP := Inet.My_IP;
      -- Let's bind the socket to the port, and insert in the list
      --  of receiving sockets. Note that the IP address used for
      --  binding is Void_Host, not The_IP.
      --
      Ada_Sockets.Bind_Socket_Any (A_Socket,
                                   Inet.Address (An_Address).IP,
                                   Inet.Address (An_Address).Port);
   end Bind_Any;

   -------------------------------------------------------------------
   -- Unbind.
   -------------------------------------------------------------------

   procedure Unbind (An_Address: in Address;
                     A_Socket:   in Ada_Sockets.Socket) is

   begin
      -- Set SO_LINGER so that Close waits until the address
      -- XXX: Is this working????
      Ada_Sockets.Set_Option (A_Socket,
                              Ada_Sockets.Build_Option (Ada_Sockets.Set, 1));
   end Unbind;

   ------------------------------------------------------------------
   -- Send.
   ------------------------------------------------------------------

   procedure Send
     (To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1) is

      use type Ada.Streams.Stream_Element_Offset;

      To_Address:  Lower_Layer.Address_CA :=
       new Address'Class'(Address'Class (To));
      Info: Send_Info_CA;
      Found: Boolean;
      Length_Stream: aliased
        Stream (Ada.Streams.Stream_Element_Offset'Size/8);
   begin
      Addr_Lists.Get_Element (Send_Addresses,
                              To_Address,
                              Info,
                              Found);
      if not Found then
         -- No connection with To, let's open one.
         Info := new Send_Info;
         Ada_Sockets.Create_Socket (Ada_Sockets.TCP,
                                    Info.Socket);
         Ada_Sockets.Connect_Socket (Info.Socket,
                                     Inet.Address (To).IP,
                                     Inet.Address (To).Port);
         Addr_Lists.Insert (Send_Addresses,
                            To_Address,
                            Info,
                            Found);
      end if;
      Ada.Streams.Stream_Element_Offset'Output
        (Length_Stream'Access, Length);
      Inet.Send (To, Send_Info (Info.all).Socket,
                 Length_Stream.Data'Unrestricted_Access,
                 Length_Stream.Current_Length, Length_Stream.First);
      Inet.Send (To, Send_Info (Info.all).Socket,
                 Data, Length, First);
   end Send;


   -------------------------------------------------------------------
   -- Send_From
   -------------------------------------------------------------------
   procedure Send_From
     (From:     in     Address;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1) is
   begin
      raise Forbidden_Call;
   end Send_From;


   procedure Receive
     (From: out Address_CA;
      To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Forbidden_Call;
   end Receive;

   -------------------------------------------------------------------
   -- Receive (Socket).
   -------------------------------------------------------------------

   procedure Receive (Socket: in Ada_Sockets.Socket;
                      Data:   access Ada.Streams.Stream_Element_Array;
                      Length: in out Ada.Streams.Stream_Element_Offset) is

      use type Ada.Streams.Stream_Element_Offset;
      use type C.Int;

      -- Sock_Addr record used for Recvfrom.
      Sock_Addr:     aliased C_Sockets.Sockaddr_In;
      Sock_Addr_Len: aliased C.Int := Sock_Addr'Size/C.CHAR_BIT;
      -- No of bytes received.
      Received:      C.Int;
   begin
      -- Receive data.
      Received := C_Sockets.Recvfrom (C.Int (Socket),
                                      -- Data, Data.all'Length,
                                      Data.all, Data.all'Length,
                                      0, Sock_Addr'Access,
                                      Sock_Addr_Len'Access);
      -- Check errors.
      if Received < 0 then
         raise Receiving_Error;
      end if;
      Length := Ada.Streams.Stream_Element_Offset (Received);
   end Receive;

   -------------------------------------------------------------------
   -- Receive (Recv_Info).
   -------------------------------------------------------------------

   procedure Receive
     (To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset) is

      use type Ada.Streams.Stream_Element_Offset;

      Received:      Boolean := False;
      The_Socket:    Ada_Sockets.Socket;
      New_Sockets:   Ada_Sockets.Socket_Array_A;
      -- Length of remaining data to be received.
      Length_Data:   Ada.Streams.Stream_Element_Offset;
      Length_Stream: aliased
        Stream (Ada.Streams.Stream_Element_Offset'Size/8);
      -- Length of data actually received.
      Length_Recv:   Ada.Streams.Stream_Element_Offset := 0;
      -- Length of data already stored in Data.
      Lenght_Stored: Ada.Streams.Stream_Element_Offset := 0;
      Done:          Boolean;
      -- "True" info of this address.
      True_Info:     Recv_Info_CA;
   begin
      while not Received loop
         declare
            Ready: Ada_Sockets.Socket_Array := Ada_Sockets.Ready_Socket
              (Ada_Sockets.Socket_Array (To_Info.Sockets.all), 60.0);
         begin
            if Ready (Ready'First) =
              To_Info.Sockets (To_Info.Sockets.all'First) then
               -- The socket is the listening one, new connection.
               Ada_Sockets.Accept_Socket (Ready (Ready'First),
                                          The_Socket);
               -- New array of waiting sockets.
               New_Sockets :=
                new Ada_Sockets.Socket_Array
                 (To_Info.Sockets.all'First ..
                  To_Info.Sockets.all'Last + 1);
               New_Sockets (To_Info.Sockets.all'Range) :=
                 To_Info.Sockets.all;
               New_Sockets (To_Info.Sockets.all'Last + 1) :=
                 The_Socket;
               -- We can now free the old list.
               Ada_Sockets.Free (To_Info.Sockets);
               To_Info.Sockets := New_Sockets;
               if To_Info.The_Handler /= null then
                  -- We have a handler, we should insert the new socket
                  --  in Recv_Sockets.
                  -- !!!: We can be here only if we were called from
                  --      Service_Socket, from Receiver task. In this
                  --      case, we're protected by The_Mutex, so
                  --      we dont have to "Enter" it.
                  Inet.Addr_Lists.Get_Element (Recv_Addresses,
                                               To_Info.Address,
                                               True_Info, Done);
                  Inet.Sock_Lists.Insert (Recv_Sockets,
                                          The_Socket,
                                          True_Info, Done);
                  Recv_Address_Change := True;
               end if;
               Put_Line (Inet.Addr_Lists.Image (Recv_Addresses),
                         " (Receive, after accepting)");
            else
               -- We got something, let's receive it.
               Received := True;
               -- First, let's get the expected length.
               Receive (Ready (Ready'First),
                        Length_Stream.Data'Unrestricted_Access,
                        Length_Stream.Current_Length);
               Length :=
                 Ada.Streams.Stream_Element_Offset'Input
                 (Length_Stream'Access);
               Length_Data := Length;
               Length_Recv := Length;
               -- Now, let's read the message (which could come
               --  in some chuncks, so let's read while we don't
               --  have it all.
               Receive (Ready (Ready'First), Data, Length_Recv);
               -- While there is still something left, read it.
               while Length_Recv < Length_Data loop
                  Lenght_Stored := Lenght_Stored + Length_Recv;
                  Length_Data := Length_Data - Length_Recv;
                  declare
                     Recv_Array: aliased Ada.Streams.Stream_Element_Array
                       (1 .. Length_Data);
                  begin
                     Length_Recv := Length_Data;
                     Receive (Ready (Ready'First),
                              Recv_Array'Unrestricted_Access,
                              Length_Recv);
                     Data.all
                       (Data.all'First + Lenght_Stored ..
                        Data.all'First + Lenght_Stored + Length_Recv - 1)
                       := Recv_Array (1 .. Length_Recv);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Receive;

   ------------------------------------------------------------------
   -- Valid.
   ------------------------------------------------------------------

   function Valid (An_Address: in Address) return Boolean is

      -- We need ">=".
      use type Ada_Sockets.Host_No;

   begin
     if Inet.Address (An_Address).IP >= Inet.First_IP_Multi then
        -- Not an unicast address.
        return False;
     else
        return True;
     end if;
   end Valid;

end Lower_Layer.Inet.TCP;
