-- $Id: lower_layer-inet-udp.adb,v 1.17 1998/01/19 12:46:47 jgb Exp $
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

-- The dummy socket will be bound to an unicast UDP address.
with Ada.Task_Identification;
with Lower_Layer.Inet.UDP.Uni;
with Interfaces.C;
with C_Sockets;
with C_Sockets.Arch;
with Debugging;

package body Lower_Layer.Inet.UDP is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Inet.UDP";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);


   package C renames Interfaces.C;


   -------------------------------------------------------------------
   -- Socket for sending.
   --  Open in body of this package.
   -------------------------------------------------------------------

   Send_Socket: Ada_Sockets.Socket;

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
      Put_Line ("Entering", " (Bind_Address)");
      The_Address := new Address'Class'(Address'Class (An_Address));
      Info := new Recv_Info'((Address     => The_Address,
                              The_Handler => Handler,
                              Socket      => A_Socket));
      -- We're going to touch data structures for bound addresses,
      --  so we have to protect against concurrent access.
      Put_Line ("Going to enter The_Mutex: " &
                The_Mutex.Image, " (Bind_Address)");
      The_Mutex.Enter (Ada.Task_Identification.Current_Task);
      Put_Line ("Going to insert in Addr_Lists", " (Bind_Address)");
      Addr_Lists.Insert (Recv_Addresses,
                         The_Address,
                         Info, Done);
      if not Done then
         -- The address was already in the list (unlock mutex).
         The_Mutex.Leave;
         raise Lower_Layer.Binding_Error;
      end if;
      if Handler /= null then
         -- We have a handler.
         Put_Line ("Going to insert in Recv_Sockets", " (Bind_Address)");
         Sock_Lists.Insert (Recv_Sockets,
                            A_Socket,
                            Info, Done);
         Inet.Refresh_Receiver_Address;
         Recv_Address_Change := True;
      end if;
      The_Mutex.Leave;
      Put_Line ("Leaving", " (Bind_Address)");
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
      Addr_Lists.Remove (Recv_Addresses,
                         The_Address,
                         Found,
                         Removed_Info);
      -- The_Address no longer needed.
      Free (The_Address);
      if not Found then
         raise Lower_Layer.Unbinding_Error;
      end if;
      Put_Line ("Going to remove socket, Recv_Sockets = " &
                Sock_Lists.Image (Recv_Sockets),
                " (Unbind_Address)");
      -- XXX: When called from Receiver_Task (for finishing, when its
      --      loop is over), it seems that Removed_Info.Address
      --      point to the same place than An_Address. I'm not
      --      completely sure, but this seems to cause a
      --      segmentation fault. However, with the following line
      --      commented out, the fault doen't appear, byt a memory
      --      leak appears...
--      Free (Removed_Info.Address);
      -- Just in case, try to remove from Recv_Sockets
      --  (only will be there if it has a handler).
      Sock_Lists.Remove (Recv_Sockets,
                         Recv_Info (Removed_Info.all).Socket,
                         Found);
      if Found then
         -- If the socket was found, the receiver task is in charge of it
         --  (address with handler).
         Recv_Address_Change := True;
         Refresh_Receiver_Address;
      end if;
      -- Unbind and destroy the socket after removing it from the list,
      --  If the receiver still didn't got the message sent
      --  by Refresh_Receiver_Address, Unbind and/or  Destroy_Socket
      --  will hang until Ready_Socket (which should have the socket
      --  locked at the pthreads layer) finishes and let them progress.
      Unbind (Address'Class (An_Address),
              Recv_Info (Removed_Info.all).Socket);
      Ada_Sockets.Destroy_Socket (Recv_Info (Removed_Info.all).Socket);
      Free (Removed_Info);
   exception
      when others =>
         raise Lower_Layer.Unbinding_Error;
   end Unbind_Address;







   ------------------------------------------------------------------
   -- Send (Address).
   ------------------------------------------------------------------

   procedure Send
     (To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1) is

   begin
      Inet.Send (To, Send_Socket, Data, Length, First);
   exception
      when Sending_Error =>
         declare
            Error: C.Int := C_Sockets.C_Errno;
         begin
            -- Ignore errors which violate the Lower_Layer model.
            if not C_Sockets.Arch.Send_UDP_Error_Ignored (Error) then
               Put_Line ("Message sent incorrectly to " &
                         Image (To) &
                         ", Errno: " & C.Int'Image (Error) & ")",
                         " (Send)");
               raise;
            else
               Put_Line ("Not really an error in Linux, sending to " &
                         Image (To) &
                         ", Errno: " & C.Int'Image (Error) & ")",
                         " (Send)");
            end if;
         end;
   end Send;





   ------------------------------------------------------------------
   -- Send_From
   ------------------------------------------------------------------

   procedure Send_From
     (From:   in     Address;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1) is

      Found:       Boolean;
      The_Address: Address_CA := new Address'Class'(Address'Class (From));
      Info:        Recv_Info_CA := new Recv_Info;

   begin

      --  Are we bound to the From End_Point? If so, which is its socket?
      Addr_Lists.Get_Element (Recv_Addresses,
                              The_Address,
                              Info, Found);

--PHQ
      if not Found then
         Ada_Sockets.Create_Socket (Ada_Sockets.UDP, Send_Socket);

         Put_Line ("gonna send through " & Ada_Sockets.Image(Send_Socket));
         Inet.Send (To, Send_Socket, Data, Length, First);
         Ada_Sockets.Destroy_Socket(Send_Socket);
      else
         Put_Line ("gonna send through " & Ada_Sockets.Image(Recv_Info(Info.all).Socket));
         Inet.Send (To, Recv_Info(Info.all).Socket, Data, Length, First);
      end if;

      Free (The_Address);

   exception
      when Sending_Error =>
         declare
            Error: C.Int := C_Sockets.C_Errno;
         begin
            -- Ignore errors which violate the Lower_Layer model.
            if not C_Sockets.Arch.Send_UDP_Error_Ignored (Error) then
               Put_Line ("Message sent incorrectly to " &
                         Image (To) &
                         ", Errno: " & C.Int'Image (Error) & ")",
                         " (Send_From)");
               raise;
            else
               Put_Line ("Not really an error in Linux, sending to " &
                         Image (To) &
                         ", Errno: " & C.Int'Image (Error) & ")",
                         " (Send_From)");
            end if;
         end;
   end Send_From;

   -------------------------------------------------------------------
   -- Receive (Recv_Info).
   -------------------------------------------------------------------

   procedure Receive
     (From : out Address_CA;
      To_Info: in out Recv_Info;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset) is

      use type Ada.Streams.Stream_Element_Offset;
      use type C.Int;

      -- Sock_Addr record used for Recvfrom.
      Sock_Addr:     aliased C_Sockets.Sockaddr_In;
      Sock_Addr_Len: aliased C.Int := Sock_Addr'Size/C.CHAR_BIT;
      -- No of bytes received.
      Received:      C.Int;

      Origin_Address : Lower_Layer.Inet.Udp.Uni.Address;
   begin
      -- Receive data.
      Received := C_Sockets.Recvfrom (C.Int (To_Info.Socket),
                                      -- Data, Data.all'Length,
                                      Data.all, Data.all'Length,
                                      0, Sock_Addr'Access,
                                      Sock_Addr_Len'Access);

--PHQ
      Lower_Layer.Inet.Build (Lower_Layer.Inet.Address (Origin_Address),
             Ada_Sockets.To_String (Ada_Sockets.Host_No (Sock_Addr.Sin_Addr)),
             Ada_Sockets.To_Integer (Ada_Sockets.Port_No (Sock_Addr.Sin_Port)));

      Put_Line ("Recibimos del origen: " & Lower_Layer.Inet.Udp.Uni.Image(Origin_Address));

-- XXXXXXX: To remove gnat warning
      From := From;

      From.all := Lower_Layer.Inet.Address'Class (Origin_Address);
--PHQ
--      From := new Lower_Layer.Inet.Udp.Uni.Address'(Origin_Address);



      -- Check errors.
      if Received < 0 then
         raise Receiving_Error;
      end if;
      Length := Ada.Streams.Stream_Element_Offset (Received);
   end Receive;

begin
   Ada_Sockets.Create_Socket (Ada_Sockets.UDP, Send_Socket);
end Lower_Layer.Inet.UDP;
