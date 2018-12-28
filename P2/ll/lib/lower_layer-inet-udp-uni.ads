-- $Id: lower_layer-inet-udp-uni.ads,v 1.7 1997/03/13 21:43:28 jgb Exp $
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
-- Package providing access to UDP unicast.
------------------------------------------------------------------------------

package Lower_Layer.Inet.UDP.Uni is

   type Address is new Lower_Layer.Inet.UDP.Address with private;

private

   type Address is new Lower_Layer.Inet.UDP.Address with 
     null record;

   procedure Bind (An_Address: in     Address;
		   A_Socket:   in out Ada_Sockets.Socket);

   procedure Bind_Any (An_Address: out    Address;
		       A_Socket:   in out Ada_Sockets.Socket);

   procedure Unbind (An_Address: in Address;
		     A_Socket:   in Ada_Sockets.Socket);

   function Valid (An_Address: in Address) return Boolean;

end Lower_Layer.Inet.UDP.Uni;
