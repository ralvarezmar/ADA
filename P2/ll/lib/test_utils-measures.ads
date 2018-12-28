-- $Id: test_utils-measures.ads,v 1.2 1998/01/30 14:08:26 jgb Exp $
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
-- Subprograms useful for performing measures.
------------------------------------------------------------------------------

package Test_Utils.Measures is

   -- Start a measure (gets current time).
   --
   procedure Start_Measure;

   -- End a measure (gets current time and prints a message with
   --  elapsed time and unitary time.
   --
   procedure End_Measure (Message:       in String;
                          Num_Loops:     in Natural;
                          Short_Message: in String);

end Test_Utils.Measures;
