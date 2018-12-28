-- $Id: test_strings.ads,v 1.3 1996/11/26 00:41:42 jgb Exp $
--

-- Hierarchy of types, rooted at Test_String. It can be used when
--  a test need types of a hierarchy.

-- For using bounded strings (used in private part).
with Ada.Strings.Bounded;

package Test_Strings is

   ----------------------------------------------------
   -- Test_String
   --  Test string, for having a common root.
   ----------------------------------------------------

   type Test_String is abstract tagged null record;

   -- Get a string from a test string.
   --
   function Image (A_Test_String: in Test_String) return String
     is abstract;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String
     is abstract;

   type Test_String_CA is access Test_String'Class;

   function Image (A_Test_String: in Test_String_CA) return String;

   type Test_String_Array is array (Natural range <>) of Test_String_CA;

   function Image (A_Test_Array: in Test_String_Array) return String;

   ----------------------------------------------------
   -- Test_String_5
   --  Strings of a maximun of 5 characters. Direct son of Test_String.
   ----------------------------------------------------  
 
   type Test_String_5 is new Test_String with private;

   -- Get a string from a test string.
   --
   function Image (A_Test_String: in Test_String_5) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_5;


   ----------------------------------------------------
   -- Test_String_10
   --  Strings of a maximun of 10 characters. Direct son of Test_String.
   ----------------------------------------------------  
 
   type Test_String_10 is new Test_String with private;

   -- Get a string  from a test string.
   --
   function Image (A_Test_String: in Test_String_10) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_10;


private

   -- Let's instantiate bounded strings with a length of 5 and 10.
   --  These will be used later for defining test strings of that
   --  lengths.

   package BStr_5 is new 
     Ada.Strings.Bounded.Generic_Bounded_Length (5);

   package BStr_10 is new 
     Ada.Strings.Bounded.Generic_Bounded_Length (10);


   type Test_String_5 is new Test_String with
      record
	 Data: BStr_5.Bounded_String;
      end record;

   type Test_String_10 is new Test_String with
      record
	 Data: BStr_10.Bounded_String;
      end record;

end Test_Strings;
