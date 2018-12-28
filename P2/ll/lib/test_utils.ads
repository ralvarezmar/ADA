-- $Id: test_utils.ads,v 1.7 1998/01/30 14:08:26 jgb Exp $
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
-- Some utilities for test programs.
--
------------------------------------------------------------------------------

package Test_Utils is

   -- Compares the actual string with the correct one, and
   --  updates Passed (if True) with True if both are equal or
   --  false if not. If passed was False, it always returns False.
   --
   procedure Evaluate_Results (Correct: in     String;
                               Actual:  in     String;
                               Passed:  in out Boolean);

   -- Puts a line indicated if test was passed or not, according to
   --  the value of Passed.
   --
   procedure Put_Results (Name:   in String;
                          Passed: in Boolean);

   -- Like Put_Results, but also calls terminators, if any.
   --
   procedure Put_Results_And_Terminate (Name:   in String;
                                        Passed: in Boolean);

   generic
      Name:  String;
      Debug: Boolean;
   procedure Show_And_Evaluate_Gen (Result:  in     String;
                                    Correct: in     String;
                                    Passed:  in out Boolean;
                                    Message: in     String := "");

   procedure Show_And_Evaluate (Name:    in     String;
                                Debug:   in     Boolean;
                                Result:  in     String;
                                Correct: in     String;
                                Passed:  in out Boolean;
                                Message: in     String := "");

end Test_Utils;
