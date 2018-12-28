-- $Id$

with Ada_Sockets;

package body Lower_Layer.Inet.Misc is

--   function To_Name (IP: String) return String is

--   begin
--   end To_Name;

   function To_IP (Name: in String) return String is

   begin
      return Ada_Sockets.To_String (Ada_Sockets.Get_Addr_By_Name (Name));
   end To_IP;

end Lower_Layer.Inet.Misc;
