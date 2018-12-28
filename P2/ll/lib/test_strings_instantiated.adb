-- $Id: test_strings_instantiated.adb,v 1.3 1996/12/16 00:26:28 jgb Exp $
--


package body Test_Strings_Instantiated is


   function Image (A_Test_String: in Test_String_1) return String renames
     TStrings_1.Image;

   function To_Test (A_String: in String) return Test_String_1 renames
     TStrings_1.To_Test;

 
   function Image (A_Test_String: in Test_String_5) return String renames
     TStrings_5.Image;

   function To_Test (A_String: in String) return Test_String_5 renames
     TStrings_5.To_Test;


   function Image (A_Test_String: in Test_String_10) return String renames
     TStrings_10.Image;

   function To_Test (A_String: in String) return Test_String_10 renames
     TStrings_10.To_Test;


   function Image (A_Test_String: in Test_String_600) return String renames
     TStrings_600.Image;

   function To_Test (A_String: in String) return Test_String_600 renames
     TStrings_600.To_Test;


   function Image (A_Test_String: in Test_String_1024) return String renames
     TStrings_1024.Image;

   function To_Test (A_String: in String) return Test_String_1024 renames
     TStrings_1024.To_Test;

end Test_Strings_Instantiated;





