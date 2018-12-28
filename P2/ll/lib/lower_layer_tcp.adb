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

with Lower_Layer.Inet.TCP;
with Ada.Streams;

package body Lower_Layer_TCP is

   use type Lower_Layer.Address_Ca; -- for "=" operator


   -- Uses directly its Lower_Layer equivalent
   --
   function Image (An_EP: in End_Point) return String
     renames Lower_Layer.Image;


   -- Uses directly its Lower_Layer equivalent
   --
   function Is_Null (An_EP: in End_Point) return Boolean
     renames Lower_Layer.Is_Null;

   -- Uses directly its Lower_Layer equivalent
   --
   function "=" (Left, Right: in End_Point) return Boolean
     renames Lower_Layer."=";


   -- Creates an End_Point and calls to Lower_Layer.TCP.Build
   --
   function Build(IP: String; Port: Natural) return End_Point is

      Addr: Lower_Layer.Address_CA;

   begin
      Addr := new Lower_Layer.Inet.TCP.Address;
      Lower_Layer.Inet.TCP.Build
        (Lower_Layer.Inet.TCP.Address(Addr.all), IP, Port);
      return Addr;
   end Build;

   -- Uses directly its Lower_Layer equivalent
   --
   procedure Connect (An_End_Point: in  End_Point;
                      A_Connection: out Connection) is

   begin
      Lower_Layer.Inet.TCP.Misc.Connect
        (Lower_Layer.Inet.TCP.Address'Class(An_End_Point.all), A_Connection);
   end Connect;

   -- Uses directly its Lower_Layer equivalent
   --
   procedure Dispose (A_Connection: in out Connection) is

   begin
      Lower_Layer.Inet.TCP.Misc.Dispose (A_Connection);
   end Dispose;

   -- Uses directly its Lower_Layer equivalent
   --
   procedure Listen_Connection (An_End_Point: in End_Point) is

   begin
      Lower_Layer.Inet.TCP.Misc.Listen_Connection
        (Lower_Layer.Inet.TCP.Address'Class (An_End_Point.all));
   end Listen_Connection;

   -- Uses directly its Lower_Layer equivalent
   --
   procedure Wait_Connection (An_End_Point: in  End_Point;
                              A_Connection: out Connection) is

   begin
      Lower_Layer.Inet.TCP.Misc.Wait_Connection
        (Lower_Layer.Inet.TCP.Address (An_End_Point.all), A_Connection);
   end Wait_Connection;


end Lower_Layer_TCP;
