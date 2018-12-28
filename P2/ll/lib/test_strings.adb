-- $Id: test_strings.adb,v 1.3 1996/11/26 00:41:41 jgb Exp $
--

with Debugging;

package body Test_Strings is

   ----------------------------------------------------
   -- Image (for Test_String_CA)
   ----------------------------------------------------

   function Image (A_Test_String: in Test_String_CA) return String is

   begin
      if A_Test_String = null then
	 return "null";
      else
	 return Image (A_Test_String.all);
      end if;
   end Image;

   ----------------------------------------------------
   -- Image (for Test_String_Array)
   ----------------------------------------------------

   function Image (A_Test_Array: in Test_String_Array) return String is

      A_String: Debugging.Debug_String;
   begin
      Debugging.Add_To (A_String, "(");
      for Count in A_Test_Array'Range loop
	 if Count /= A_Test_Array'First then
	    Debugging.Add_To (A_String, ", ");
	 end if;
	 Debugging.Add_To (A_String, Image (A_Test_Array (Count)));
      end loop;
      Debugging.Add_To (A_String, ")");
      return Debugging.To_String (A_String);
   end Image;

   ----------------------------------------------------
   -- Image (for Test_String_5)
   ----------------------------------------------------

   function Image (A_Test_String: in Test_String_5) return String is

   begin
      return BStr_5.To_String (A_Test_String.Data);
   end Image;


   ----------------------------------------------------
   -- To_Test (for Test_String_5)
   ----------------------------------------------------

   function To_Test (A_String: in String) return Test_String_5 is
     
   begin
      return (Data => BStr_5.To_Bounded_String (A_String));
   end To_Test;
     

   ----------------------------------------------------
   -- Image (for Test_String_10)
   ----------------------------------------------------

   function Image (A_Test_String: in Test_String_10) return String is

   begin
      return BStr_10.To_String (A_Test_String.Data);
   end Image;


   ----------------------------------------------------
   -- To_Test (for Test_String_10)
   ----------------------------------------------------

   function To_Test (A_String: in String) return Test_String_10 is

   begin
      return (Data => BStr_10.To_Bounded_String (A_String));
   end To_Test;


end Test_Strings;
