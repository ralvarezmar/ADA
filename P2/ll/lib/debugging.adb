-- $Id: debugging.adb,v 1.6 1998/02/10 20:20:09 jgb Exp $
--

--
-- Some suprograms useful for debugging.
--

-- For printing messages.
with Text_IO;

-- For releasing storage (intermediate strings):
with Unchecked_Deallocation;

package body Debugging is

   -- Controls if error information is printed or not
   --  (Put_Line_Error_Gen, Put_Exception_Error_Gen)
   --
   Error_Debug: constant Boolean := True;

   procedure Free is new Unchecked_Deallocation (String,
                                                 String_Access);


   ------------------------------------------------
   -- Finalize
   ------------------------------------------------

   procedure Finalize (A_Debug_String: in out Debug_String) is

   begin
      Free (A_Debug_String.The_Data);
   end Finalize;


   ------------------------------------------------
   -- Add_To
   ------------------------------------------------

   procedure Add_To (A_Debug_String: in out Debug_String;
                     A_String:       in     String) is

      Tmp_String: String_Access;
   begin
      if A_Debug_String.The_Data = null then
         A_Debug_String.The_Data := new String'(A_String);
      else
         Tmp_String := A_Debug_String.The_Data;
         A_Debug_String.The_Data := new String'(Tmp_String.all & A_String);
         Free (Tmp_String);
      end if;
   end Add_To;


   ------------------------------------------------
   -- To_String
   ------------------------------------------------

   function To_String (A_Debug_String: in Debug_String) return String is

   begin
      if A_Debug_String.The_Data /= null then
         return A_Debug_String.The_Data.all;
      else
         return "";
      end if;
   end To_String;


   ------------------------------------------------
   -- Put_Line (for Debug_String)
   ------------------------------------------------

   procedure Put_Line (Name:           in String;
                       Debug:          in Boolean;
                       A_Debug_String: in Debug_String) is

   begin
      if Debug then
         Text_IO.Put_Line ("=> " & Name & ": " & A_Debug_String.The_Data.all);
      end if;
   end Put_Line;


   ------------------------------------------------
   -- Put_Line (for String)
   ------------------------------------------------

   procedure Put_Line (Name:     in String;
                       Debug:    in Boolean;
                       A_String: in String) is

   begin
      if Debug then
         Text_IO.Put_Line ("=> " & Name & ": " & A_String);
      end if;
   end Put_Line;

   ------------------------------------------------
   -- Put (for String)
   ------------------------------------------------

   procedure Put (Name:     in String;
                  Debug:    in Boolean;
                  A_String: in String) is

   begin
      if Debug then
         Text_IO.Put (Name & A_String);
      end if;
   end Put;

   ------------------------------------------------
   -- Put_Line
   ------------------------------------------------

   procedure New_Line (Debug: in Boolean) is

   begin
      if Debug then
         Text_IO.New_Line;
      end if;
   end New_Line;

   ------------------------------------------------
   -- Put_Line_Gen (for String. generic version)
   ------------------------------------------------

   procedure Put_Line_Gen (A_String: in String;
                           Message:  in String := "") is

   begin
      Put_Line (Name & Message, Debug, A_String);
   end Put_Line_Gen;

   ------------------------------------------------
   -- Put_Gen (for String. generic version)
   ------------------------------------------------

   procedure Put_Gen (A_String: in String;
                      Message:  in String := "") is

   begin
      Put (Message, Debug, A_String);
   end Put_Gen;

   ------------------------------------------------
   -- Put_Line_Empty_Gen
   ------------------------------------------------

   procedure New_Line_Gen is

   begin
      New_Line (Debug);
   end New_Line_Gen;

   ------------------------------------------------
   -- Put_Line (for Exceptions. generic version)
   ------------------------------------------------

   procedure Put_Exception_Gen
     (A_Exception: in Ada.Exceptions.Exception_Occurrence;
      Explanation: in String := "";
      Message:     in String := "") is

   begin
      Put_Line (Name & Message, Debug,
                Explanation & " " &
                Ada.Exceptions.Exception_Name (A_Exception));
   end Put_Exception_Gen;

   ------------------------------------------------
   -- Put_Line_Error_Gen
   ------------------------------------------------

   procedure Put_Line_Error_Gen (A_String: in String;
                                 Message:  in String := "") is

   begin
      Put_Line (Name & Message, Error_Debug, A_String);
   end Put_Line_Error_Gen;

   ------------------------------------------------
   -- Put_Exception_Error_Gen
   ------------------------------------------------

   procedure Put_Exception_Error_Gen
     (A_Exception: in Ada.Exceptions.Exception_Occurrence;
      Explanation: in String := "";
      Message:     in String := "") is

   begin
      Put_Line (Name & Message, Error_Debug,
                Explanation & " " &
                Ada.Exceptions.Exception_Name (A_Exception));
   end Put_Exception_Error_Gen;
end Debugging;
