-- $Id: test_strings_instantiated.ads,v 1.4 1998/01/19 12:47:30 jgb Exp $
--

-- Some instantiated String_Type for direct use by test programs.
--

-- For root String_Type.
with Test_Strings;

-- For generic descendants of Test_Strings (user in private section).
with Test_Strings_Generic;

package Test_Strings_Instantiated is

   pragma Elaborate_Body;

   package TStrings_1  is new
     Test_Strings_Generic (Length => 1);

   package TStrings_5  is new
     Test_Strings_Generic (Length => 5);

   package TStrings_10 is new
     Test_Strings_Generic (Length => 10);

   package TStrings_600 is new
     Test_Strings_Generic (Length => 600);

   package TStrings_1024 is new
     Test_Strings_Generic (Length => 1024);

   ----------------------------------------------------
   -- Test_String_1
   --  Strings of a maximun of 1 characters. Direct son of Test_String.
   ----------------------------------------------------

   subtype Test_String_1  is TStrings_1.Test_String;

   -- Get a string from a test string.
   --
   function Image (A_Test_String: in Test_String_1) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_1;


   ----------------------------------------------------
   -- Test_String_5
   --  Strings of a maximun of 5 characters. Direct son of Test_String.
   ----------------------------------------------------

   subtype Test_String_5  is TStrings_5.Test_String;

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

   subtype Test_String_10 is TStrings_10.Test_String;

   -- Get a string  from a test string.
   --
   function Image (A_Test_String: in Test_String_10) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_10;

   ----------------------------------------------------
   -- Test_String_600
   --  Strings of a maximun of 600 characters. Direct son of Test_String.
   ----------------------------------------------------

   subtype Test_String_600 is TStrings_600.Test_String;

   -- Get a string  from a test string.
   --
   function Image (A_Test_String: in Test_String_600) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_600;

   ----------------------------------------------------
   -- Test_String_1024
   --  Strings of a maximun of 1024 characters. Direct son of Test_String.
   ----------------------------------------------------

   subtype Test_String_1024 is TStrings_1024.Test_String;

   -- Get a string  from a test string.
   --
   function Image (A_Test_String: in Test_String_1024) return String;

   -- Get a test string from a string.
   --
   function To_Test (A_String: in String) return Test_String_1024;

end Test_Strings_Instantiated;



