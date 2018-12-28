-- $Id: lower_layer-inet-udp-uni.adb,v 1.13 1998/01/19 12:46:45 jgb Exp $
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

with Debugging;

package body Lower_Layer.Inet.UDP.Uni is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Inet.UDP.Uni";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   -----------------------------------------------------------------------
   -- Renaming.
   -----------------------------------------------------------------------

   package Inet renames Lower_Layer.Inet;

   -------------------------------------------------------------------
   -- Bind.
   --
   --  XXX: It should be checked that port is not 0. It it
   --       is, the user should call Bind_Any, not Bind.
   -------------------------------------------------------------------

   procedure Bind (An_Address: in     Address;
                   A_Socket:   in out Ada_Sockets.Socket) is

      -- We need "=".
      use type Ada_Sockets.Host_No;

   begin
      Put_Line ("Going to open the socket", "(Bind)");
      -- Let's open the socket.
      Ada_Sockets.Create_Socket (Ada_Sockets.UDP, A_Socket);
      -- Let's bind the socket to the port.
      --
      Ada_Sockets.Bind_Socket (A_Socket,
                               Inet.Address (An_Address).IP,
                               Inet.Address (An_Address).Port);
   exception
      when others =>
         raise Lower_Layer.Binding_Error;
   end Bind;

   -------------------------------------------------------------------
   -- Bind_Any.
   --  Works like Bind.
   -------------------------------------------------------------------

   procedure Bind_Any (An_Address: out    Address;
                       A_Socket:   in out Ada_Sockets.Socket) is

   begin
      -- Let's open the socket.
      Ada_Sockets.Create_Socket (Ada_Sockets.UDP, A_Socket);
      Inet.Address (An_Address).IP := Inet.My_IP;
      -- Let's bind the socket to the port, and insert in the list
      --  of receiving sockets. Note that the IP address used for
      --  binding is Void_Host, not The_IP.
      --
      Ada_Sockets.Bind_Socket_Any (A_Socket,
                                   Inet.Address (An_Address).IP,
                                   Inet.Address (An_Address).Port);
   exception
      when others =>
         raise Lower_Layer.Binding_Error;
   end Bind_Any;

   -------------------------------------------------------------------
   -- Unbind.
   -- XXX: SO_LINGER shouldn't be needed for UDP sockets,
   --      After reading the source, it seems it could be
   --      convenient for Linux 1.3.x... But it isn't used
   --      for now (that's why the code is comented).
   --      This should probably be Linux dependent code...
   -------------------------------------------------------------------

   procedure Unbind (An_Address: in Address;
                     A_Socket:   in Ada_Sockets.Socket) is

   begin
      -- Set SO_LINGER so that Close waits until the address
      --  is actually unbound (see comment above).
      -- Currently commented out.
--      Ada_Sockets.Set_Option (The_Socket,
--                            Ada_Sockets.Build_Option (Ada_Sockets.Set, 1));
      null;
   end Unbind;

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

end Lower_Layer.Inet.UDP.Uni;
