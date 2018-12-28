-- $Id: test_hierarchy.adb,v 1.2 1996/12/16 00:26:26 jgb Exp $
--

package body Test_Hierarchy is

   ------------------------------------------------------------
   -- Image (Derived_1)
   ------------------------------------------------------------

   function Image (A_Derived_1: in Derived_1) return String is

   begin
      return TStrings.Image (A_Derived_1.Data);
   end Image;


   ------------------------------------------------------------
   -- To_Derived (Derived_1)
   ------------------------------------------------------------

   function To_Derived (A_String: in String) return Derived_1 is

      A_Derived_1: Derived_1;
   begin
      A_Derived_1.Data := TStrings.To_Test (A_String);
      return A_Derived_1;
   end To_Derived;


   ------------------------------------------------------------
   -- Image (Derived_5)
   ------------------------------------------------------------

   function Image (A_Derived_5: in Derived_5) return String is

   begin
      return TStrings.Image (A_Derived_5.Data);
   end Image;


   ------------------------------------------------------------
   -- To_Derived (Derived_5)
   ------------------------------------------------------------

   function To_Derived (A_String: in String) return Derived_5 is

      A_Derived_5: Derived_5;
   begin
      A_Derived_5.Data := TStrings.To_Test (A_String);
      return A_Derived_5;
   end To_Derived;


   ------------------------------------------------------------
   -- Image (Derived_10)
   ------------------------------------------------------------

   function Image (A_Derived_10: in Derived_10) return String is

   begin
      return TStrings.Image (A_Derived_10.Data);
   end Image;


   ------------------------------------------------------------
   -- To_Derived (Derived_10)
   ------------------------------------------------------------

   function To_Derived (A_String: in String) return Derived_10 is

      A_Derived_10: Derived_10;
   begin
      A_Derived_10.Data := TStrings.To_Test (A_String);
      return A_Derived_10;
   end To_Derived;


   ------------------------------------------------------------
   -- Image (Derived_600)
   ------------------------------------------------------------

   function Image (A_Derived_600: in Derived_600) return String is

   begin
      return TStrings.Image (A_Derived_600.Data);
   end Image;


   ------------------------------------------------------------
   -- To_Derived (Derived_600)
   ------------------------------------------------------------

   function To_Derived (A_String: in String) return Derived_600 is

      A_Derived_600: Derived_600;
   begin
      A_Derived_600.Data := TStrings.To_Test (A_String);
      return A_Derived_600;
   end To_Derived;

   ------------------------------------------------------------
   -- Image (Derived_1024)
   ------------------------------------------------------------

   function Image (A_Derived_1024: in Derived_1024) return String is

   begin
      return TStrings.Image (A_Derived_1024.Data);
   end Image;


   ------------------------------------------------------------
   -- To_Derived (Derived_1024)
   ------------------------------------------------------------

   function To_Derived (A_String: in String) return Derived_1024 is

      A_Derived_1024: Derived_1024;
   begin
      A_Derived_1024.Data := TStrings.To_Test (A_String);
      return A_Derived_1024;
   end To_Derived;

end Test_Hierarchy;
