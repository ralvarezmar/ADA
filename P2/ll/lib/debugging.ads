-- $Id: debugging.ads,v 1.6 1998/02/10 20:20:09 jgb Exp $
--

-- Some suprograms useful for debugging. For simplicity, controlled type
--  Debug_String is provided. Automatic deallocation for it is
--  provided.

-- Debug_String is controlled.
with Ada.Finalization;

with Ada.Exceptions;

package Debugging is

   pragma Elaborate_Body;

   package Final renames Ada.Finalization;

   -- ADT provided for managing strings.
   type Debug_String is new Final.Controlled with private;

   -- Overriding of Finalize for providing automatic deallocation.
   procedure Finalize (A_Debug_String: in out Debug_String);


   ------------------------------------------------
   -- User subprograms for managing Debug_String.
   ------------------------------------------------

   -- Adds a String at the end of a Debug_String.
   --
   procedure Add_To (A_Debug_String: in out Debug_String;
                     A_String:       in     String);

   -- Gets a String from a Debug_String.
   --
   function To_String (A_Debug_String: in Debug_String) return String;

   -- Resembles Text_IO.Put_Line for Debug_String.
   --  Name is the string to appear at the beggining of the line
   --  (e.g. the package name).
   --
   procedure Put_Line (Name:           in String;
                       Debug:          in Boolean;
                       A_Debug_String: in Debug_String);

   ------------------------------------------------
   -- User subprograms for easying the use of Text_IO.
   ------------------------------------------------

   -- Resembles Text_IO.Put_Line.
   --  Name is the string to appear at the beggining of the line
   --  (e.g. the package name).
   --
   procedure Put_Line (Name:     in String;
                       Debug:    in Boolean;
                       A_String: in String);
   procedure Put (Name:     in String;
                  Debug:    in Boolean;
                  A_String: in String);
   procedure New_Line (Debug: in Boolean);

   -- Resembles Text_IO.Put_Line (generic version).
   --  Name is the string to appear at the beggining of the line
   --  (e.g. the package name).
   --  Message will appear immediately after Name.
   --
   generic
      Name: String;
      Debug: Boolean;
   procedure Put_Line_Gen (A_String: in String;
                           Message:  in String := "");

   generic
      Name: String;
      Debug: Boolean;
   procedure Put_Gen (A_String: in String;
                      Message:  in String := "");

   generic
      Debug: Boolean;
   procedure New_Line_Gen;

   -- Like Put_Line_Gen, but specialized for writing information
   --  related to an exception.
   --
   generic
      Name: String;
      Debug: Boolean;
   procedure Put_Exception_Gen
     (A_Exception: in Ada.Exceptions.Exception_Occurrence;
      Explanation: in String := "";
      Message:     in String := "");

   -- Like Put_Line_Gen, but specialized for writing error information.
   --  No Debug parameter, since it is controlled in the body
   --  of this package.
   --
   generic
      Name: String;
   procedure Put_Line_Error_Gen (A_String: in String;
                                 Message:  in String := "");

   -- Like Put_Line_Gen, but specialized for writing error
   --  information related to an exception.
   --  No Debug parameter, since it is controlled in the body
   --  of this package.
   --
   generic
      Name: String;
   procedure Put_Exception_Error_Gen
     (A_Exception: in Ada.Exceptions.Exception_Occurrence;
      Explanation: in String := "";
      Message:     in String := "");

private

   type String_Access is access String;

   type Debug_String is new Final.Controlled with
      record
         The_Data: String_Access;
      end record;

end Debugging;

