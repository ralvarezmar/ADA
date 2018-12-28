-- $Id: test_strings_generic.adb,v 1.2 1996/08/20 22:00:36 jgb Exp $
--


package body Test_Strings_Generic is


   ----------------------------------------------------
   -- Image
   ----------------------------------------------------

   function Image (A_Test_String: in Test_String) return String is

   begin
      return BStr.To_String (A_Test_String.Data);
   end Image;


   ----------------------------------------------------
   -- To_Test
   ----------------------------------------------------

   function To_Test (A_String: in String) return Test_String is
     
   begin
      return (Data => BStr.To_Bounded_String (A_String));
   end To_Test;
     

end Test_Strings_Generic;
