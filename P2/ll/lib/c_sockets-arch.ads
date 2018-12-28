-- $Id$
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
-- In this package goes the architecture dependent code (not directly used
-- by C_Sockets, and in general using facilites from C_Sockets).
-- There should be a version of the body for each architecture, but
-- specification is common.
------------------------------------------------------------------------------

package C_Sockets.Arch is

   pragma Preelaborate;

   -- This function checks if an errno got after Send should be ignored.
   --  !!!: In Linux, UDP returns an ICMP if the port to which we send is
   --       not bound. But the semantics of Lower_Layer are such that this
   --       should not raise an exception.
   --       (noted by pheras, jcenteno)

   function Send_UDP_Error_Ignored (Error: C.Int) return Boolean;
   pragma Inline (Send_UDP_Error_Ignored);

   -- Set the Element in Mask.
   --  In other words, set the corresponding socket element in
   --  the mask.

   procedure Set_In_Mask (Mask:    in out FD_Set;
                          Element: in     FD_Set_Range);
   pragma Inline (Set_In_Mask);

   -- Is Element in Mask?
   --   In other words, is set the corresponding socket element in
   --  the mask?

   function Is_In_Mask (Mask:    in FD_Set;
                        Element: in FD_Set_Range)
                        return Boolean;
   pragma Inline (Is_In_Mask);


   --------------------------------------------------------------
   -- Patch added by Alvaro Polo (apoloval@gsyc.escet.urjc.es) --
   -- for Mac OS X and Darwin OS compability.                  --
   --------------------------------------------------------------

   -- Usually, invocations to 'bind' system calls are preceded by
   -- a call to 'bzero' to zero-fill 'sockaddr_t' POSIX structure.
   -- In GNU/Linux systems, this is not necessary, because (at least
   -- in theory) 'bind' call is ready to process no cleaned address
   -- structure. This is not true in Darwin and Mac OS X systems.
   -- For this reason, a zero-filler procedure is needed in such
   -- systems.

   -- This procedure performs this 'sockaddr_in' type cleaning.
   procedure bzero(addr: access sockaddr_in;
                   addr_size: Interfaces.Unsigned_32);
   pragma import(C, bzero, "bzero");

   -------------------
   -- End of patch. --
   -------------------

end C_Sockets.Arch;
