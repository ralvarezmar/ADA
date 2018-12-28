-- $Id: lower_layer-inet.adb,v 1.24 1998/02/10 20:20:13 jgb Exp $
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


-- Miscellaneous utilities related to access types.
--
with Misc_Util_Accesses;

with Ada.Unchecked_Deallocation;

-- Used for defining dummy socket (receiver task).
--
with Lower_Layer.Inet.UDP.Uni;

with Lower_Layer.Faults;

-- For registering terminator of receiver task.
with Misc_Util_Terminators;

with Ada.Task_Identification;

with C_Sockets;
with C_Sockets.Arch;

with Interfaces.C;

with Ada.Exceptions;
with Debugging;

package body Lower_Layer.Inet is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Inet";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);
   procedure Put_Line_Error is new Debugging.Put_Line_Error_Gen
     (Name => Name);
   procedure Put_Exception_Error is new Debugging.Put_Exception_Error_Gen
     (Name => Name);

   -----------------------------------------------------------------------
   -- Renaming.
   -----------------------------------------------------------------------

   package C renames Interfaces.C;

   ---------------------------------------------------------------------
   -- Image_Class (Address_Info'Class).
   ---------------------------------------------------------------------

   function Image_Class (Info: in Address_Info'Class) return String is

   begin
      return Image (Info);
   end Image_Class;

   ---------------------------------------------------------------------
   -- Is_Null (Address_Info_CA).
   ---------------------------------------------------------------------

   function Is_Null (Info: in Address_Info_CA) return Boolean is

      -- !!!: Next type is needed because "=" is overloaded for
      --      Address_CA.
      type Tmp_Address_Info_CA is access all Address_Info'Class;
   begin
      return Tmp_Address_Info_CA (Info) = null;
   end Is_Null;

   ---------------------------------------------------------------------
   -- Free (Address_Info_CA).
   ---------------------------------------------------------------------

   procedure Free (Info: in out Address_Info_CA) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Address_Info'Class,
         Name   => Address_Info_CA);

   begin
      if not Is_Null (Info) then
         Deallocate (Info);
      end if;
   end Free;

   ---------------------------------------------------------------------
   -- Image (Address_Info_CA).
   ---------------------------------------------------------------------

   function Image (Info: in Address_Info_CA) return String is

      function Image_CA is new Misc_Util_Accesses.Image_CA
        (Object => Address_Info,
         Name   => Address_Info_CA);
   begin
      return Image_CA (Info);
   end Image;

   ---------------------------------------------------------------------
   -- Image (Recv_Info).
   ---------------------------------------------------------------------

   function Image (Info: in Recv_Info) return String is

   begin
      if Info.The_Handler = null then
         return "Handler ausent"  &
           ", Address: " & Image (Info.Address);
      else
         return "Handler present" &
           ", Address: " & Image (Info.Address);
      end if;
   end Image;

   ---------------------------------------------------------------------
   -- Image_Class (Recv_Info'Class).
   ---------------------------------------------------------------------

   function Image_Class (Info: in Recv_Info'Class) return String is

   begin
      return Image (Info);
   end Image_Class;

   ---------------------------------------------------------------------
   -- Receive (Recv_Info_CA).
   ---------------------------------------------------------------------
-- PHQ
   procedure Receive
     (From: out     Address_CA;
      To_Info: in     Recv_Info_CA;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in out Ada.Streams.Stream_Element_Offset) is

   begin
      Receive (From, To_Info.all, Data, Length);
   end Receive;


   ---------------------------------------------------------------------
   -- Free (Recv_Info_CA).
   ---------------------------------------------------------------------

   procedure Free (Info: in out Recv_Info_CA) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Recv_Info'Class,
         Name   => Recv_Info_CA);

   begin
      if not Is_Null (Address_Info_CA (Info)) then
         Deallocate (Info);
      end if;
   end Free;

   ---------------------------------------------------------------------
   -- Image (Recv_Info_CA).
   ---------------------------------------------------------------------

   function Image (Info: in Recv_Info_CA) return String is

      function Image_CA is new Misc_Util_Accesses.Image_CA
        (Object => Recv_Info,
         Name   => Recv_Info_CA);
   begin
      return Image_CA (Info);
   end Image;

   ---------------------------------------------------------------------
   -- Build (Address).
   ---------------------------------------------------------------------

   procedure Build (An_Address: out Address;
                    IP:         in  String;
                    Port:       in  Natural) is

   begin
      An_Address.IP   := Ada_Sockets.To_Host (IP);
      An_Address.Port := Ada_Sockets.To_Port (Port);
      if not Valid (Address'Class (An_Address)) then
         raise Lower_Layer.Bad_Address;
      end if;
   end Build;

   ---------------------------------------------------------------------
   -- ">=".
   ---------------------------------------------------------------------

   function ">=" (Left, Right: Address) return Boolean is

      use type Ada_Sockets.Port_No;
      use type Ada_Sockets.Host_No;
   begin
      if Left.IP >= Right.IP then
         if Left.IP = Right.IP then
            -- Left and Right are equal.
            return Left.Port >= Right.Port;
         else
            -- Left is greater than right.
            return True;
         end if;
      else
         -- Left is not greater or equal than right.
         return False;
      end if;
   end ">=";

   ---------------------------------------------------------------------
   -- Image.
   ---------------------------------------------------------------------

   function Image (An_Address: in Address) return String is

   begin
      return Lower_Layer.Image (Lower_Layer.Address (An_Address)) &
        " IP: " & Ada_Sockets.To_String (An_Address.IP) &
        ", Port: " &
        Integer'Image (Ada_Sockets.To_Integer (An_Address.Port));
   end Image;

   ------------------------------------------------------------------
   -- Refresh_Receiver_Address.
   --  Just send a mesage to the dummy socket, so that Ready_Socket
   --  (in the receiver task) unhangs, releasing the locks on
   --  the sockets.
   ------------------------------------------------------------------

   procedure Refresh_Receiver_Address is

      Buffer: aliased Lower_Layer.Stream (8);
   begin
      Boolean'Output (Buffer'Access, True);
      -- !!!: We shouldn't use plain Send, since it could
      --      be afected by fault injection.
      Lower_Layer.Send_No_Faults (Dummy_Address, Buffer'Access);
   end Refresh_Receiver_Address;

   ---------------------------------------------------------------------
   -- Handler_Dummy.
   --  Just for the dummy socket to have a handler...
   ---------------------------------------------------------------------

   procedure Handler_Dummy (From : in Address_CA;
                            To:   in     Address_CA;
                            Data: access Stream) is

   begin
      null;
   end Handler_Dummy;

   ------------------------------------------------------------------
   -- Terminate_Receiver (internal use only).
   --
   --  Terminates the receiver task, in an orderly way.
   ------------------------------------------------------------------

   procedure Terminate_Receiver is

   begin
      -- "Order" the receiver task to terminate.
      Put_Line ("Before entering, The_Mutex = " &
                The_Mutex.Image, " (Terminate_Receiver)");
      The_Mutex.Enter (Ada.Task_Identification.Current_Task);
      Recv_Address_Change := True;
      Recv_Finalize := True;
      Put_Line ("Before leaving, The_Mutex = " &
                The_Mutex.Image, " (Terminate_Receiver)");
      The_Mutex.Leave;
      -- Unhang the receiver, just in case.
      Refresh_Receiver_Address;
      -- Wait until the receiver finishes.
      Put_Line ("Waiting for The_Receiver to finish, The_Mutex = " &
                The_Mutex.Image, " (Terminate_Receiver)");
      The_Receiver.Finished;
      -- Unlock the mutex (was locked by the receiver task).
      The_Mutex.Leave;
   end Terminate_Receiver;

   ---------------------------------------------------------------------
   -- Check_Dummy.
   ---------------------------------------------------------------------

   procedure Check_Dummy is

   begin
      Put_Line ("Entering...", " (Check_Dummy)");
      Dummy_Mutex.Enter (Ada.Task_Identification.Current_Task);
      if Sock_Lists.Size_Of (Recv_Sockets) = 0 then
         -- Empty list, let's create dummy socket.
         Put_Line ("Creating dummy socket", "(Check_Dummy)");
         Dummy_Address := new UDP.Uni.Address;
         Bind_Any (Address'Class (Dummy_Address.all), Dummy_Socket);
         Bind_Address (Address'Class (Dummy_Address.all),
                       Handler_Dummy'Access, Dummy_Socket);
         -- Receiver task is started. It should hang waiting for
         --  The_Mutex to be released (so that Recv_Address_Change
         --  can be set later).
         The_Receiver := new Receiver_Task;
         -- Register terminator for The_Receiver.
         Misc_Util_Terminators.Register_Terminator (Terminate_Receiver'Access,
                                                    "Terminate_Receiver");
      end if;
      Dummy_Mutex.Leave;
      Put_Line ("Leaving...", " (Check_Dummy)");
   exception
      when Except: others =>
--          Put_Exception_Error (Except, "Unexpected exception raised: ",
--                               " (Check_Dummy)");
         Put_Line ("Unexpected exception raised: " &
                   Ada.Exceptions.Exception_Name(Except),
                   " (Check_Dummy)");
         raise;
   end Check_Dummy;

   ---------------------------------------------------------------------
   -- Bind.
   ---------------------------------------------------------------------

   procedure Bind (An_Address: in Address;
                   Handler:    in Handler_A := null) is

      -- Socket to bind.
      The_Socket: Ada_Sockets.Socket := -1;
   begin
      Put_Line ("Entering", " (Bind)");
      if Handler /= null then
         Check_Dummy;
      end if;
      Bind (Address'Class (An_Address), The_Socket);
      Put_Line ("Going to call Bind_Address, An_Address: " &
                Image (An_Address), " (Bind)");
      Bind_Address (Address'Class (An_Address),
                    Handler, The_Socket);
      Put_Line ("Leaving", " (Bind)");
   exception
      when Except: others =>
--          Put_Exception_Error (Except, "Unexpected exception raised: ",
--                               " (Bind)");
         Put_Line ("Unexpected exception raised: " &
                   Ada.Exceptions.Exception_Name(Except),
                              " (Bind)");
         Ada_Sockets.Destroy_Socket (The_Socket);
         raise Lower_Layer.Binding_Error;
   end Bind;

   ---------------------------------------------------------------------
   -- Bind_Any.
   ---------------------------------------------------------------------

   procedure Bind_Any (An_Address: out Address;
                       Handler:    in Handler_A := null) is

      -- Socket to bind.
      The_Socket: Ada_Sockets.Socket := -1;
   begin
      if Handler /= null then
         Check_Dummy;
      end if;
      Bind_Any (Address'Class (An_Address), The_Socket);
      Put_Line ("Bind_Any done", "(Bind_Any)");
      Bind_Address (Address'Class (An_Address),
                    Handler, The_Socket);
   exception
      when others =>
         Ada_Sockets.Destroy_Socket (The_Socket);
         raise Lower_Layer.Binding_Error;
   end Bind_Any;


   ---------------------------------------------------------------------
   -- Unbind.
   ---------------------------------------------------------------------

   procedure Unbind (An_Address: in Address) is

   begin
      -- We're going to touch data structures for bound addresses,
      --  so we have to protect against concurrent access.
      The_Mutex.Enter (Ada.Task_Identification.Current_Task);
      Unbind_Address (Address'Class (An_Address));
      The_Mutex.Leave;
   exception
      when others =>
         The_Mutex.Leave;
         raise;
   end Unbind;


   ------------------------------------------------------------------
   -- Receive (Address).
   ------------------------------------------------------------------

   procedure Receive
     (From : out Address_CA;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in out Ada.Streams.Stream_Element_Offset) is

      Found:       Boolean;
      -- Address for looking for receiving socket.
      The_Address: Address_CA := new Address'Class'(Address'Class (To));
      Info:        Recv_Info_CA;
   begin
      Addr_Lists.Get_Element (Recv_Addresses,
                              The_Address,
                              Info, Found);
      Free (The_Address);
      if not Found then
         raise Lower_Layer.Unbound_Address;
      end if;
      Receive (From, Info, Data, Length);
   end Receive;

   ------------------------------------------------------------------
   -- Send (Address, Socket).
   --
   -- XXX: Exceptions raised by Sock.Send should be catched.
   -- XXX: Only implemented for First = 1.
   ------------------------------------------------------------------

   procedure Send
     (To_Addr: in     Address'Class;
      To_Sock: in     Ada_Sockets.Socket;
      Data:    access Ada.Streams.Stream_Element_Array;
      Length:  in     Ada.Streams.Stream_Element_Offset;
      First:   in     Ada.Streams.Stream_Element_Offset := 1) is

      use type C.Int;

      Sock_Addr: aliased C_Sockets.Sockaddr_In;
      Sent: C.Int;

      Resend : Boolean := True;

   begin
      Put_Line ("Going to send message to " &
                Image (To_Addr),
                " (Send)");

     -------------------------------------------------------------------
      -- Patch added by jcenteno, paurea (jcenteno@gsyc.escet.urjc.es) --
      --                                 (paurea@gsyc.escet.urjc.es)   --
      -- for compatibility problems with Ubuntu64
      --------------------------------------------------------------

      -- Usually, invocations to 'bind' system calls are preceded by
      -- a call to 'bzero' to zero-fill 'in_addr_t' POSIX structure.
      -- In GNU/Linux systems, MAYBE this is not necessary, because
      -- (sometimes) 'bind' call is ready to process no cleaned address
      -- structure. This is not true, at least, in Darwin, Mac OS X and
      -- Ubuntu64 systems. For this reason, a zero-filler procedure is
      -- needed in such systems.

      -- This zero-filler is declared in C_Sockets.Arch package
      -- (c_sockets-arch.ads at line 76). A previous call to it must
      -- be done before 'bind' is called.

      declare
        use type Interfaces.Unsigned_32;
      begin
        C_Sockets.Arch.bzero(Sock_Addr'access, Sock_Addr'size/C.CHAR_BIT);
      end;

      -------------------
      -- End of patch. --
      -------------------
      -- Fill in the Sock_Addr record for "to" address.
      Sock_Addr.Sin_Family := C.Short (C_Sockets.AF_INET);
      Sock_Addr.Sin_Addr   := C_Sockets.In_Addr (Address (To_Addr).IP);
      Sock_Addr.Sin_Port   := C.Unsigned_Short (Address (To_Addr).Port);


      Resend := True;
      while Resend loop
         -- Send data.
         Sent := C_Sockets.Sendto (C.Int (To_Sock),
                                   Data.all,
                                   C.Int (Length),
                                   0, Sock_Addr'Access,
                                   Sock_Addr'Size/C.CHAR_BIT);

         -- Check errors.
         if Sent /= C.Int (Length) then

            declare
               Error: C.Int := C_Sockets.C_Errno;
            begin
               -- Ignore errors which violate the Lower_Layer model.
               if not C_Sockets.Arch.Send_UDP_Error_Ignored (Error) then
                  Put_Line ("Message sent incorrectly to " &
                            Image (To_Addr) & " (result code: " &
                            C.Int'Image (Sent) &
                            " (Send)");
                  raise Sending_Error;
               else
                  -- Possible problem with Linux UDP sockets, so we will
                  -- resend the message
                  null;
               end if;
            end;

         else
            Put_Line ("Message sent correctly to " & Image (To_Addr),
                      " (Send)");
            Resend := False;
         end if;
      end loop;


   end Send;

   ---------------------------------------------------------------------
   -- Service_Socket.
   --  A socket is ready for reading. Do whatever is convenient
   --  (usually, read a message).
   --
   -- !!!: This procedure should be granted exclusive access by
   --      locking The_Mutex before entering.
   ---------------------------------------------------------------------

   procedure Service_Socket (Info: in Recv_Info_CA;
                             A_Socket: in Ada_Sockets.Socket) is

      use type Ada.Streams.Stream_Element_Offset;

      Data: aliased Stream (Receive_Buffer_Size);
      The_Length: Ada.Streams.Stream_Element_Offset;

      --PHQ
--      From : Address_CA; --Initialized in lower_layer.inet.udp.receive (From...)
      From : Address_CA := new Lower_Layer.Inet.Udp.Uni.Address;

   begin
      Reset (Data);
      The_Length := Data.Length;
      Receive (From,
               To_Info => Info,
               Data    => Data.Data'Unrestricted_Access,
               Length  => The_Length);
      Data.Current_Length := The_Length;
      if A_Socket /= Dummy_Socket and then
        Data.Current_Length /= 0 then
         -- Message was not for Dummy_Socket, and something was
         --  received. Thereafer, a real
         --  message has been received. Let's call its handler.
         if Faults.Should_Fail (Faults.Receive) or
           Faults.Should_Fail (Faults.Any) then
            Faults.Print_Failed (True, Faults.Receiving);
            Put_Line ("Received message, but failed");
         else
            Faults.Print_Failed (False, Faults.Receiving);

-- jcenteno 20061017
-- Changed from Data'Access to Data'Unrestricted_Access
-- to avoid Programa_Error with gnat-4.0 in Ubuntu Dapper
            Info.The_Handler.all
              (From, Info.Address, Data'Unrestricted_Access);
--PHQ
--            Free (From);
         end if;
      end if;
   end Service_Socket;

   ------------------------------------------------------------------
   -- Receiver_Task.
   ------------------------------------------------------------------

   task body Receiver_Task is

      Sockets: Ada_Sockets.Socket_Array_A;
      Found:   Boolean;
      Info:    Recv_Info_CA;
   begin
      loop
         -- We're going to touch data structures for bound addresses,
         --  so we have to protect against concurrent access.
         The_Mutex.Enter (Ada.Task_Identification.Current_Task);
         Put_Line ("1");
         if Recv_Address_Change then
            -- If there is no socket to wait for, or we
            --  Recv_Finalize was set, let's exit the loop.
            exit when (Recv_Finalize or else
              (Sock_Lists.Size_Of (Recv_Sockets) < 1));
            Ada_Sockets.Free (Sockets);
            Sockets := new Ada_Sockets.Socket_Array'
              (Ada_Sockets.Socket_Array
               (Sock_Lists.Get_Keys (Recv_Sockets)));
            Recv_Address_Change := False;
         end if;
         Put_Line ("2");
         Put_Line (Addr_Lists.Image (Recv_Addresses));
         The_Mutex.Leave;
         begin
         -- This new scope is just to catch exceptions from Ready_Sockets.
            declare
               Ready: Ada_Sockets.Socket_Array := Ada_Sockets.Ready_Socket
--                 (Ada_Sockets.Socket_Array (Sockets.all), 1.0);
                 (Ada_Sockets.Socket_Array (Sockets.all), 60.0);
            begin
               -- !!!: There is a chance that, when Ready_Socket returns,
               --      Ready corresponds to a socket already removed from
               --      Recv_Sockets. In that case, nothing should be received
               --      (perhaps even the corresponding socket is already
               --      closed). That's why we protect the code from consulting
               --      Recv_Sockets until the Receive, and that's why we
               --      don't try to receive if the socket is not found.
               The_Mutex.Enter (Ada.Task_Identification.Current_Task);
               Put_Line ("Got something");
               --  Get the address_info corresponding to that socket.
               Sock_Lists.Get_Element (Recv_Sockets,
                                            Ready (Ready'First),
                                            Info, Found);
               -- Do whatever is needed for servicing the socket, which
               --  is ready to read. Call handler if appropriate.
               Service_Socket (Info, Ready (Ready'First));
               -- !!!: Caution!!!
               --      The_Mutex is unlocked only after handling has been
               --      executed. This will prevent an Unbind to be performed
               --      while the handler is running, which could be
               --      quite strange.
               The_Mutex.Leave;
            exception
               when others =>
                  -- If we have an exception in the loop, just release the
                  --  mutex and propagate it.
                  The_Mutex.Leave;
                  Put_Line ("Exception raised");
                  raise;
            end; -- declare
         exception
            when Ada_Sockets.Timeout_Expired =>
               -- Do nothing right now.
               -- XXX: This is a good place for collecting statistics.
               null;
               Put_Line ("Timeout Expired");
            when others =>
               -- If we have an exception in the loop, just release the
               --  mutex and propagate it.
               Put_Line ("Exception raised");
               raise;
         end; -- scope for catching exceptions
      end loop;
      Put_Line ("Out of the loop", " (Receiver_Task)");
      -- Let's wait until the killer synchronizes with us.
      -- This point is reached only when the exit sentence becomes true
      --  (see above). Here we're still locked. The caller to entry
      --  Finished must unlock The_Mutex.
      if (Sock_Lists.Size_Of (Recv_Sockets) >= 1) then
         -- Let's unbound addresses.
         declare
            Bound_Sockets: Ada_Sockets.Socket_Array :=
              Ada_Sockets.Socket_Array
               (Sock_Lists.Get_Keys (Recv_Sockets));
            Found: Boolean;
            Bound_Info: Recv_Info_CA;
         begin
            for Count in Bound_Sockets'Range loop
               Sock_Lists.Get_Element (Recv_Sockets,
                                       Bound_Sockets (Count),
                                       Bound_Info,
                                       Found);
               Unbind_Address
                 (Address'Class (Recv_Info (Bound_Info.all).Address.all));
               -- XXX: Addresses and Info should be freed, but
               --      TCP info is currently accessed from all the
               --      sockets for a given "to" address. This
               --      leads to freeing already freed info and
               --      addresses if things aren't done with care.
--             Free (Recv_Info (Bound_Info.all).Address);
--             Free (Bound_Info);
            end loop;
         end;
      end if;
      accept Finished;
   exception
      when Except: others =>
         Put_Exception_Error (Except, "Unexpected exception raised",
                              " (Receiver_Task)");
         -- In case of unexpected exception, just terminate...
         Misc_Util_Terminators.Execute_Terminators;
         Put_Line_Error ("Everything should terminate...",
                         " (Receiver_Task)");
   end Receiver_Task;

end Lower_Layer.Inet;
