-- $Id$
--
------------------------------------------------------------------------------
--                                                                          --
--    Copyright (C) 1997-2000  Jesus M. Gonzalez Barahona                   --
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

with Lower_Layer.Inet.TCP.Misc;
with Ada.Streams;

package Lower_Layer_TCP is


   -- subtype for declaring communications end-points
   subtype End_Point is Lower_Layer.Address_CA;

   -- gives a string representation of an End_Point
   function Image (An_EP: in End_Point) return String;

   -- checks if an End_Point contains a value
   function Is_Null (An_EP: in End_Point) return Boolean;

   -- operator to compare End_Points
   function "=" (Left, Right: in End_Point) return Boolean;



   -- Builds an End_Point from an IP address and a port number
   function Build(IP: String; Port: Natural) return End_Point;

   subtype Connection is Lower_Layer.Inet.TCP.Misc.Connection;

   procedure Connect (An_End_Point: in  End_Point;
                      A_Connection: out Connection);

   procedure Dispose (A_Connection: in out Connection);

   procedure Listen_Connection (An_End_Point: in End_Point);

   procedure Wait_Connection (An_End_Point: in  End_Point;
                              A_Connection: out Connection);

   -- Raised when a Wait_Connection is done for a not listenning address.
   Not_Listening: exception;

   -- Attempt to listen on an already "listened" address.
   Already_Listening: exception;

end Lower_Layer_TCP;
