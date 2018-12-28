-- $Id: misc_util_terminators.adb,v 1.3 1998/01/30 14:08:08 jgb Exp $
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

with Lists_Generic;
-- Some debugging subprograms can be useful here.
with Debugging;
-- Some subprograms for tests.
with Test_Utils;

package body Misc_Util_Terminators is

   ----------------------------------------------------------
   -- Debugging.
   ----------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Misc_Util_Terminators";

   type String_Access is access String;

   type Terminator_Record is
      record
         Code: Teminator_Access;
         Name: String_Access;
      end record;

   function Image (A_Record: in Terminator_Record) return String is

   begin
      return A_Record.Name.all;
   end Image;

   package Terminator_Lists is new Lists_Generic
     (Element => Terminator_Record,
      Image   => Image);

   Terminator_List: Terminator_Lists.List;

   -----------------------------------------------------------------------
   -- Register_Terminator
   -----------------------------------------------------------------------

   procedure Register_Terminator (Code: in Teminator_Access;
                                  Name: in String) is

      The_Name: String_Access := new String'(Name);
      The_Record: Terminator_Record :=
        (Code => Code, Name => The_Name);

   begin
      Terminator_Lists.Insert_Head (Terminator_List, The_Record);
   end Register_Terminator;

   -----------------------------------------------------------------------
   -- Execute_Terminators
   -----------------------------------------------------------------------

   procedure Execute_Terminators is

      A_Reference: Terminator_Lists.Reference;
      A_Record:    Terminator_Record;
   begin
      while not Terminator_Lists.Is_Empty (Terminator_List) loop
         A_Reference := Terminator_Lists.First (Terminator_List);
         A_Record := Terminator_Lists.Get_Element (Terminator_List,
                                                   A_Reference);
         Debugging.Put_Line (Name, Debug,
                             "Executing terminator " &
                             Image (A_Record));
         A_Record.Code.all;
         Terminator_Lists.Remove (Terminator_List, A_Reference);
      end loop;
   end Execute_Terminators;

   -----------------------------------------------------------------------
   -- Image
   -----------------------------------------------------------------------

   function Image return String is

   begin
      return Terminator_Lists.Image (Terminator_List);
   end Image;

begin
   Terminator_Lists.Initialize (Terminator_List);
end Misc_Util_Terminators;
