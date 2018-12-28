-- $Id: ada_sockets.adb,v 1.13 1998/01/19 12:46:40 jgb Exp $
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
-- Thick interface to BSD sockets (UDP only)
------------------------------------------------------------------------------

with Interfaces.C.Strings;
with C_Sockets.Arch;
with Debugging;

package body Ada_Sockets is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Ada_Sockets";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   -----------------------------------------------------------------------
   -- Renaming.
   -----------------------------------------------------------------------

   package C_Str  renames Interfaces.C.Strings;
   package C_Sock renames C_Sockets;

   -----------------------------------------------------------------------
   --
   -- Subprograms for Host_No and Port_No.
   --
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   -- To_Host (Host_No).
   -----------------------------------------------------------------------

   function To_Host (Host: in String) return Host_No is

      In_Host: aliased C_Sock.In_Addr;
--      Error: C.Int;
      Result:  Interfaces.unsigned_32;
   begin -- To_Host
--      Error := C_Sock.Inet_Aton (C_Str.New_String (Host),
--                               In_Host'Access);
      Result := C_Sock.Inet_Addr (C_Str.New_String (Host));
--      if Integer (Error) /= 1 then

      --------------------------------------------------------------
      -- Patch added by Alvaro Polo (apoloval@gsyc.escet.urjc.es) --
      -- for GNAT 4.0 compability.                                --
      --------------------------------------------------------------

      -- Next commented code was originally written by Jesus Barahona.
      -- In GNAT version prior to 4.0, it works well. Otherwise it
      -- fails with a 'constraint_error' exception when result value
      -- overflows integer type bounds (see code below).

      -- if Integer (Result) = -1 then
      --    raise Bad_Host_No;
      -- end if;

      -- To avoid this problem, a new constant value INADDR_NONE is
      -- added to 'C_Sockets' package (at c_sockets.ads, line
      -- 536) which represents true value return by 'inet_addr' system
      -- call, compatible with Interfaces.unsigned_32 type of 'result'
      -- variable.

      -- This is the new valid code:

      declare
        use type Interfaces.unsigned_32;
      begin
        if Result = C_Sock.INADDR_NONE then
          raise Bad_Host_No;
        end if;
      end;

      -------------------
      -- End of patch. --
      -------------------

      In_Host.S_Addr := Result;
      return Host_No (In_Host);
   end To_Host;

   -----------------------------------------------------------------------
   -- To_String (Host_No).
   -----------------------------------------------------------------------

   function To_String (Host: in Host_No) return String is

   begin -- To_String
      return C_Str.Value (C_Sock.Inet_Ntoa (C_Sock.In_addr (Host)));
   end To_String;

   -----------------------------------------------------------------------
   -- ">=" (Host_No).
   --  Compares two IP network-ordered host numbers (converting
   --  them previously to host order, before comparing).
   -----------------------------------------------------------------------

   function ">=" (Left, Right: in Host_No) return Boolean is

      use type Interfaces.Unsigned_32;
   begin
      return C_Sock.Htonl (C_Sock.In_Addr(Left).S_Addr)
        >= C_Sock.Htonl (C_Sock.In_Addr(Right).S_Addr);
   end ">=";

   -----------------------------------------------------------------------
   -- Get_Addr_By_Name (Host_No).
   -----------------------------------------------------------------------

   function Get_Addr_By_Name (Name: in String) return Host_No is

      use type C_Sock.Hostent_Ptr;

      Host_Data: C_Sock.Hostent_Ptr;
   begin
      Host_Data := C_Sock.Get_Host_By_Name (C_Str.New_String (Name));
      if Host_Data = null then
         raise Host_Name_Not_Found;
      end if;
      return Host_No (Host_Data.H_Addr_List.all.all);
   end Get_Addr_By_Name;

   -----------------------------------------------------------------------
   -- Get_Host_Name.

   -----------------------------------------------------------------------

   function Get_Host_Name return String is

      subtype Name_Array is C.Char_Array
        (1 .. C.Size_T (C_Sock.Max_Host_Name_Len));

      Error:    C.Int;
      Name:     C_Str.Char_Array_Access := new Name_Array;
      Name_Len: C.Int := C_Sock.Max_Host_Name_Len;
   begin
      Error := C_Sock.Get_Host_Name (C_Str.To_Chars_Ptr (Name), Name_Len);
      return C.To_Ada (Name.All);
   end Get_Host_Name;

   -----------------------------------------------------------------------
   -- To_Port (Port_No).
   -----------------------------------------------------------------------

   function To_Port (Port: in Integer) return Port_No is

   begin -- To_Port
      return Port_No (C_Sock.Htons(C.Unsigned_Short(Port)));
   end To_Port;

   -----------------------------------------------------------------------
   -- To_Integer (Port_No).
   -----------------------------------------------------------------------

   function To_Integer (Port: in Port_No) return Integer is

   begin -- To_Integer
      return Integer (C_Sock.Ntohs(C.Unsigned_Short(Port)));
   end To_Integer;

   -----------------------------------------------------------------------
   -- ">=" (Port_No).
   -----------------------------------------------------------------------

   function ">=" (Left, Right: in Port_No) return Boolean is

      use type C.Unsigned_Short;
   begin
      return C_Sock.Htons (C.Unsigned_Short (Left))
        >= C_Sock.Htons (C.Unsigned_Short (Right));
   end ">=";

   ----------------------------------------------------------------------
   --
   -- Sockets.
   --
   ----------------------------------------------------------------------

   -----------------------------------------------------------------------
   -- Max_Socket (internal use).
   -----------------------------------------------------------------------

   function Max_Socket (Set: in Socket_Array) return Socket is

      Max: Socket;
   begin
      if Set'Length = 0 then
         raise Socket_Array_Empty;
      else
         Max := Set (Set'First);
      end if;
      for Count in Set'Range loop
         if Max < Set (Count) then
            Max := Set (Count);
         end if;
      end loop;
      return Max;
   end Max_Socket;

   -----------------------------------------------------------------------
   -- Create_Socket.
   -----------------------------------------------------------------------

   procedure Create_Socket (A_Kind:   in  Kind;
                            A_Socket: out Socket) is

      The_Kind: C.Int;
   begin
      case A_Kind is
        when TCP => The_Kind := C_Sock.SOCK_STREAM;
        when UDP => The_Kind := C_Sock.SOCK_DGRAM;
      end case;
      A_Socket := Socket (C_Sock.Socket (C_Sock.AF_INET,
                                         The_Kind, 0));
      if A_Socket < 0 then
         raise Socket_Failed;
      end if;
   end Create_Socket;

   -----------------------------------------------------------------------
   -- Destroy_Socket.
   -----------------------------------------------------------------------

   procedure Destroy_Socket (A_Socket: in Socket) is

      Error: C.Int;
   begin
      Error := C_Sock.Close (C.Int (A_Socket));
   end Destroy_Socket;

   -----------------------------------------------------------------------
   -- Bind_Socket.
   -----------------------------------------------------------------------

   procedure Bind_Socket (A_Socket: in Socket;
                          A_Host:   in Host_No;
                          A_Port:   in Port_No) is

      use type C.Int;

      Sock_Addr: aliased C_Sock.Sockaddr_In;
      Error: C.Int;
   begin -- Bind_Socket
      --------------------------------------------------------------
      -- Patch added by Alvaro Polo (apoloval@gsyc.escet.urjc.es) --
      -- for Mac OS X and Darwin OS compability.                  --
      --------------------------------------------------------------

      -- Usually, invocations to 'bind' system calls are preceded by
      -- a call to 'bzero' to zero-fill 'in_addr_t' POSIX structure.
      -- In GNU/Linux systems, this is not necessary, because (at least
      -- in theory) 'bind' call is ready to process no cleaned address
      -- structure. This is not true in Darwin and Mac OS X systems.
      -- For this reason, a zero-filler procedure is needed in such
      -- systems.

      -- This zero-filler is declared in C_Sockets.Arch package
      -- (c_sockets-arch.ads at line 76). A previous call to it must
      -- be done before 'bind' is called.

      declare
        use type Interfaces.Unsigned_32;
      begin
        C_Sock.Arch.bzero(sock_addr'access, sock_addr'size/C.CHAR_BIT);
      end;

      -------------------
      -- End of patch. --
      -------------------

      Sock_Addr.sin_family := C.Short (C_Sock.AF_INET);
      Sock_Addr.Sin_Addr   := C_Sockets.In_addr (A_Host);
      Sock_Addr.sin_port   := C.Unsigned_Short (A_Port);
      Error := C_Sock.Bind (C.Int (A_Socket),
                            Sock_Addr'Access, Sock_Addr'Size/C.CHAR_BIT);
      if Error < 0 then
         Put_Line ("Bind failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Bind_Failed;
      else
         Put_Line ("Bind done, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
      end if;
   end Bind_Socket;

   -----------------------------------------------------------------------
   -- Bind_Socket_Any.
   -----------------------------------------------------------------------

   procedure Bind_Socket_Any (A_Socket: in  Socket;
                              A_Host:   in  Host_No;
                              A_Port:   out Port_No) is

      use type C.Int;

      Sock_Addr: aliased C_Sock.Sockaddr_In;
      Sock_Addr_Len: aliased C.Int := Sock_Addr'Size/C.CHAR_BIT;
      Error: C.Int;
   begin -- Bind_Socket
      --------------------------------------------------------------
      -- Patch added by Alvaro Polo (apoloval@gsyc.escet.urjc.es) --
      -- for Mac OS X and Darwin OS compability.                  --
      --------------------------------------------------------------

      -- Usually, invocations to 'bind' system calls are preceded by
      -- a call to 'bzero' to zero-fill 'in_addr_t' POSIX structure.
      -- In GNU/Linux systems, this is not necessary, because (at least
      -- in theory) 'bind' call is ready to process no cleaned address
      -- structure. This is not true in Darwin and Mac OS X systems.
      -- For this reason, a zero-filler procedure is needed in such
      -- systems.

      -- This zero-filler is declared in C_Sockets.Arch package
      -- (c_sockets-arch.ads at line 76). A previous call to it must
      -- be done before 'bind' is called.

      declare
        use type Interfaces.Unsigned_32;
      begin
        C_Sock.Arch.bzero(sock_addr'access, sock_addr'size/C.CHAR_BIT);
      end;

      -------------------
      -- End of patch. --
      -------------------

      Sock_Addr.sin_family := C.Short (C_Sock.AF_INET);
      Sock_Addr.Sin_Addr   := C_Sockets.In_addr (A_Host);
      Sock_Addr.sin_port   := C.Unsigned_Short (0);
      Error := C_Sock.Bind (C.Int (A_Socket),
                            Sock_Addr'Access, Sock_Addr'Size/C.CHAR_BIT);
      if Error < 0 then
         Put_Line ("(Bind_Socket_Any): bind failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Bind_Failed;
      end if;
      Error := C_Sock.Get_Sock_Name (C.Int (A_Socket),
                                     Sock_Addr'Access,
                                     Sock_Addr_Len'Access);
      if Error /= 0 then
         Put_Line ("(Bind_Socket_Any): error getting address, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Bind_Failed;
      end if;
      A_Port := Port_No (Sock_Addr.Sin_Port);
   end Bind_Socket_Any;

   -----------------------------------------------------------------------
   -- Connect_Socket.
   -----------------------------------------------------------------------

   procedure Connect_Socket (A_Socket: in Socket;
                             A_Host:   in Host_No;
                             A_Port:   in Port_No) is

      use type C.Int;

      Sock_Addr: aliased C_Sock.Sockaddr_In;
      Error: C.Int;
   begin
      -------------------------------------------------------------------
      -- Patch added by jcenteno, paurea (jcenteno@gsyc.escet.urjc.es) --
      --                                 (paurea@gsyc.escet.urjc.es)   --
      -- for potential future problems not yet discovered
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
        C_Sock.Arch.bzero(Sock_Addr'access, Sock_Addr'size/C.CHAR_BIT);
      end;

      -------------------
      -- End of patch. --
      -------------------
      Sock_Addr.sin_family := C.Short (C_Sock.AF_INET);
      Sock_Addr.Sin_Addr   := C_Sockets.In_addr (A_Host);
      Sock_Addr.sin_port   := C.Unsigned_Short (A_Port);
      Error := C_Sock.Connect (C.Int (A_Socket),
                               Sock_Addr'Access, Sock_Addr'Size/C.CHAR_BIT);
      if Error < 0 then
         Put_Line ("Connect failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Connect_Failed;
      end if;
   end Connect_Socket;

   -----------------------------------------------------------------------
   -- Listen_Socket.
   -----------------------------------------------------------------------

   procedure Listen_Socket (Listen_Socket: in Socket) is

      use type C.Int;

      Error: C.Int;
   begin
      Error := C_Sock.Listen (C.Int (Listen_Socket),
                              Back_Log);
      if Error < 0 then
         Put_Line ("Listen failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Listen_Failed;
      end if;
   end Listen_Socket;

   -----------------------------------------------------------------------
   -- Accept_Socket.
   -----------------------------------------------------------------------

   procedure Accept_Socket (Listen_Socket:   in  Socket;
                            Accepted_Socket: out Socket) is

      use type C.Int;

      Sock_Addr: aliased C_Sock.Sockaddr_In;
      Len : aliased C.Int := Sock_Addr'Size/C.CHAR_BIT;
      Error: C.Int;
   begin
      Error := C_Sock.FD_Accept (C.Int (Listen_Socket),
                                 Sock_Addr'Access,
                                 Len'Access);
      if Error < 0 then
         Put_Line ("Accept failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno) &
                   ", Listen_Socket: " & Image (Listen_Socket));
         raise Accept_Failed;
      else
         Accepted_Socket := Socket (Error);
      end if;
   end Accept_Socket;

   -----------------------------------------------------------------------
   -- Ready_Socket.
   -----------------------------------------------------------------------

   function Ready_Socket (Set:     in Socket_Array;
                          Timeout: in Duration := Duration (0))
                          return Socket_Array is

      use type C.Int;

      Error:       C.Int;
      Read_Mask:   aliased C_Sock.FD_Set := (others => False);
      Write_Mask:  aliased C_Sock.FD_Set := (others => False);
      Except_Mask: aliased C_Sock.FD_Set := (others => False);
      Time: aliased C_Sock.Timeval :=
        (Tv_Sec =>  C.Int (Long_Integer (Timeout + 0.5) - 1),
         Tv_Usec =>
           C.Int (Long_Integer
                  ((Timeout - Duration (Long_Integer (Timeout + 0.5) - 1))
                   * 1_000_000.0)));
   begin
      -- Let's fill the mask for selecting sockets ready to be read.
      for Count in Set'Range loop
         if Integer (Set (Count)) in Read_Mask'Range then
            C_Sockets.Arch.Set_In_Mask (Read_Mask,
                                        Integer (Set (Count)));
         else
            raise Select_Failed;
         end if;
      end loop;
      Error := C_Sock.FD_Select (C.Int (Max_Socket (Set)) + 1,
                                 Read_Mask'Access,
                                 Write_Mask'Access,
                                 Except_Mask'Access,
                                 Time'Access);
      if Error < 0 then
         -- Error in select
         raise Select_Failed;
      elsif Error = 0 then
         -- Timeout expired.
         raise Timeout_Expired;
      else
         -- There are some sockets ready to be read.
         declare
            Result: Socket_Array (1..Integer(Error));
            Count:  Integer range Set'Range := Set'First;
         begin
            for Ready_Socket in 1 .. Integer (Error) loop
               -- XXX: Reading the FD_Set should go to arch directory.
               -- Next line works for NetBSD and Linux
               while not C_Sockets.Arch.Is_In_Mask
                 (Read_Mask, Integer (Set (Count))) loop
--               while Read_Mask (Integer (Set (Count))) /= True loop
               -- Next line works for Solaris 2.5
--               while Read_Mask (((Integer (Set (Count)) / 32) * 32) +
--                       (31 - Integer (Set (Count)) mod 32)) /= True loop
                  Count := Count + 1;
               end loop;
               Result (Ready_Socket) := Set (Count);
            end loop;
            return Result;
         end;
      end if;
   end Ready_Socket;

   -----------------------------------------------------------------------
   -- Image (Socket).
   -----------------------------------------------------------------------

   function Image (A_Socket: in Socket) return String is

   begin
      return Socket'Image (A_Socket);
   end Image;

   -----------------------------------------------------------------------
   -- Image (Socket_Array).
   -----------------------------------------------------------------------

   function Image (An_Array: in Socket_Array) return String is

      The_String: Debugging.Debug_String;
   begin
      Debugging.Add_To (The_String, "[");
      for Count in An_Array'Range loop
         if Count /= An_Array'First then
            Debugging.Add_To (The_String, ",");
         end if;
         Debugging.Add_To (The_String,
                           Image (An_Array (Count)));
      end loop;
      Debugging.Add_To (The_String, "]");
      return "Length: (" & Natural'Image (An_Array'First) & ".." &
        Natural'Image (An_Array'Last) & "), Contents: " &
        (Debugging.To_String (The_String));
   end Image;

   -----------------------------------------------------------------------
   -- Image (Socket_Array_A).
   -----------------------------------------------------------------------

   function Image (Array_A: in Socket_Array_A) return String is

   begin
      if Array_A = null then
         return "null";
      else
         return Image (Array_A.all);
      end if;
   end Image;

   -----------------------------------------------------------------------
   --
   -- Socket options.
   --
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   -- Build_Option (Option_S_Bool).
   -----------------------------------------------------------------------

   function Build_Option (Flag: in Togle;
                          Name: in Option_S_Bool_Name)
                          return Option_S_Bool is

      The_Option: Option_S_Bool;
   begin
      case Flag is
         when Set   => The_Option.Value := 1;
         when Unset => The_Option.Value := 0;
      end case;
      case Name is
         when Reuse_Port =>
            The_Option.Name := C_Sock.SO_REUSEPORT;
      end case;
      return The_Option;
   end Build_Option;

   -----------------------------------------------------------------------
   -- Set_Option (Option_S_Bool).
   -----------------------------------------------------------------------

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_S_Bool) is

      use type C.Int;

      Value:    aliased C.Int := An_Option.Value;
      Result:   C.Int;
   begin
      Result := C_Sock.SetSockOpt (C.Int (A_Socket), C_Sock.SOL_SOCKET,
                                   An_Option.Name,
                                   Value'Access, Value'Size/C.CHAR_BIT);
      if Result /= 0 then
         Put_Line ("Ada_Sockets (Set_Option, Linger): setsockopt failed, " &
                   "Errno: " & C.Int'Image (C_Sock.Errno));
         raise Setsock_Failed;
      end if;
   end Set_Option;

   -----------------------------------------------------------------------
   -- Build_Option (Option_S_Linger).
   -----------------------------------------------------------------------


   function Build_Option (Flag:   in Togle;
                          Linger: in Integer)
                          return Option_S_Linger is

      The_Option: Option_S_Linger;
   begin
      case Flag is
         when Set   => The_Option.OnOff := 1;
         when Unset => The_Option.OnOff := 0;
      end case;
      The_Option.Linger := C.Int (Linger);
      return The_Option;
   end Build_Option;

   -----------------------------------------------------------------------
   -- Set_Option (Option_S_Linger).
   -----------------------------------------------------------------------

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_S_Linger) is

      use type C.Int;

      Value: aliased C_Sock.Linger := (OnOff  => An_Option.OnOff,
                                       Linger => An_Option.Linger);
      Result:   C.Int;
   begin
      Result := C_Sock.SetSockOpt (C.Int (A_Socket), C_Sock.SOL_SOCKET,
                                   C_Sock.SO_LINGER,
                                   Value'Access, Value'Size/C.CHAR_BIT);
   end Set_Option;

   -----------------------------------------------------------------------
   -- Build_Option (Option_IP_Multi).
   -----------------------------------------------------------------------

   function Build_Option (Name: in Option_IP_Multi_Name;
                          Addr: in Host_No)
                          return Option_IP_Multi is

      The_Option: Option_IP_Multi;
   begin
      case Name is
         when Add_Membership =>
            The_Option.Name := C_Sock.IP_ADD_MEMBERSHIP;
         when Drop_Membership =>
            The_Option.Name := C_Sock.IP_DROP_MEMBERSHIP;
      end case;
      The_Option.Mreq := (Imr_Multiaddr => C_Sock.In_Addr (Addr),
                          Imr_Interface => (s_Addr => C_Sock.INADDR_ANY));
      return The_Option;
   end Build_Option;

   -----------------------------------------------------------------------
   -- Set_Option (Option_IP_Multi).
   -----------------------------------------------------------------------

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_IP_Multi) is

      use type C.Int;

      Value:  aliased C_Sock.IP_Mreq := An_Option.Mreq;
      Result: C.Int;
   begin
      Result := C_Sock.SetSockOpt (C.Int (A_Socket), C_Sock.IPPROTO_IP,
                                   An_Option.Name,
                                   Value'Access, Value'Size/C.CHAR_BIT);
   end Set_Option;

end Ada_Sockets;
