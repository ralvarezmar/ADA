-- $Id: lower_layer-inet-udp-multi.adb,v 1.8 1997/03/13 21:43:25 jgb Exp $
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

package body Lower_Layer.Inet.UDP.Multi is

   ------------------------------------------------------
   -- Bind.
   --  Raises Binding_Error when Insert fails (returns False, 
   --  that is, there was a socket in the
   --  list with the same address).
   --
   ------------------------------------------------------

   procedure Bind (An_Address: in Address;
		   A_Socket:   in out Ada_Sockets.Socket) is

      -- We need "=".
      use type Ada_Sockets.Host_No;

   begin
      -- Let's open the socket.
      Ada_Sockets.Create_Socket (Ada_Sockets.UDP, A_Socket);
      -- Let's set reusing of port and join the IP group.
      Ada_Sockets.Set_Option 
	(A_Socket,
	 Ada_Sockets.Build_Option (Ada_Sockets.Set, 
				   Ada_Sockets.Reuse_Port));
      Ada_Sockets.Set_Option 
	(A_Socket,
	 Ada_Sockets.Build_Option (Ada_Sockets.Add_Membership, 
				   Inet.Address (An_Address).IP));
      Ada_Sockets.Bind_Socket (A_Socket,
			       Inet.Address (An_Address).IP,
			       Inet.Address (An_Address).Port);
   end Bind;

   ------------------------------------------------------
   -- Bind_Any.
   --  Always raises Forbidden call, since it doesn't make
   --  sense getting any address for a multicast protocol
   --  with only local information.
   ------------------------------------------------------

   procedure Bind_Any (An_Address: out Address;
		       A_Socket:   in out Ada_Sockets.Socket) is

   begin
      raise Forbidden_Call;
   end Bind_Any;

   -------------------------------------------------------------------
   -- Unbind.
   -------------------------------------------------------------------

   procedure Unbind (An_Address: in Address;
		     A_Socket:   in Ada_Sockets.Socket) is

   begin
      -- Let's now drop the multicast group given by address.
      Ada_Sockets.Set_Option 
	(A_Socket,
	 Ada_Sockets.Build_Option (Ada_Sockets.Drop_Membership, 
				   Inet.Address (An_Address).IP));     
   end Unbind;

   function Valid (An_Address: in Address) return Boolean is

      -- We need ">=".
      use type Ada_Sockets.Host_No;

   begin
      if Inet.Address (An_Address).IP >= Inet.First_IP_Multi and then 
	Last_IP_Multi >= Inet.Address (An_Address).IP then
	 -- Valid multicast address.
	 return True;
      else
	 return False;
      end if;
   end Valid;

end Lower_Layer.Inet.UDP.Multi;
