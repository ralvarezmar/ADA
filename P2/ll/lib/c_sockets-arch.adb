-- $id$
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
-- Linux version.
------------------------------------------------------------------------------

package body C_Sockets.Arch is

   -- <asm/errno.h>
   -- pheras

   ECONNREFUSED : constant C.Int := 111;

   ---------------------------------------------------------------------
   -- Send_Error_Ignored
   --  According to the semantics of Lower_Layer, ECONNREFUSED,
   --  returned by Linux when the destination UDP port is not bound,
   --  should be ignored.
   ---------------------------------------------------------------------

   function Send_UDP_Error_Ignored (Error: C.Int) return Boolean is

      use type C.Int;
   begin
      if Error = ECONNREFUSED then
         return True;
      else
         return False;
      end if;
   end Send_UDP_Error_Ignored;

   ---------------------------------------------------------------------
   -- Set_In_Mask
   ---------------------------------------------------------------------

   procedure Set_In_Mask (Mask:    in out FD_Set;
                          Element: in     FD_Set_Range) is

   begin
      Mask (Integer (Element)) := True;
   end Set_In_Mask;

   ---------------------------------------------------------------------
   -- Is_In_Mask
   ---------------------------------------------------------------------

   function Is_In_Mask (Mask:    in FD_Set;
                        Element: in FD_Set_Range)
                        return Boolean is

   begin
      return Mask (Integer (Element));
   end Is_In_Mask;

end C_Sockets.Arch;
