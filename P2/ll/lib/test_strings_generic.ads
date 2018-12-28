-- $Id: test_strings_generic.ads,v 1.4 1998/01/19 12:47:29 jgb Exp $
--

-- Provides a generic Test_String type, which can be instantiated
--  with the desired length.

-- For root type Test_Strings.
with Test_Strings;

-- For bounded string type (used in private section).
with Ada.Strings.Bounded;

generic
   Length: Positive;
package Test_Strings_Generic is

   pragma Elaborate_Body;

   -------------------------------------------------------
   -- When instantiated, this type will correspond to a bounded
   --  string of length Length.
   -------------------------------------------------------

   type Test_String is new Test_Strings.Test_String with private;

   -- Get a string from a test string.
   --
   function Image (A_Test_String: in Test_String) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String;


private


   package BStr is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Length);


   type Test_String is new Test_Strings.Test_String with
      record
         Data: BStr.Bounded_String;
      end record;


end Test_Strings_Generic;
