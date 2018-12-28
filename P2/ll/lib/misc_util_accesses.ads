-- $Id: misc_util_accesses.ads,v 1.7 1998/01/19 12:46:54 jgb Exp $
--
-- Miscellaneous utilities for accesses.
--

with Ada.Streams;

package Misc_Util_Accesses is

   pragma Elaborate_Body;

   generic
      type Object is limited private;
      type Name is access Object;
   function Is_Null_A (A_Name: in Name) return Boolean;

   generic
      type Object is private;
      type Name is access Object;
      with function Image_Object (An_Object: in Object)
                                  return String is <>;
   function Image_A (A_Name: in Name) return String;

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
   function Is_Null_CA (A_Name: in Name) return Boolean;

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
      with function Image_Class (An_Object: in Object'Class)
                                 return String is <>;
   function Image_CA (A_Name: in Name) return String;

   generic
      type Object is private;
      type Name is access Object;
--      with function "=" (Left, Right: in Object) return Boolean is <>;
   function Equal (Left, Right: in Name) return Boolean;

   generic
      type Object is tagged private;
      type Name is access Object'Class;
      with function ">=" (Left, Right: in Object'Class) return Boolean is <>;
   function Greater_Equal_CA (Left, Right: in Name) return Boolean;

   generic
      type Object is private;
      type Name is access Object;
      with procedure Read_Object
        (Stream: access Ada.Streams.Root_Stream_Type'Class;
         Item:   out    Object);
   procedure Read_A (Stream: access Ada.Streams.Root_Stream_Type'Class;
                     Item:   out    Name);

   generic
      type Object is private;
      type Name is access Object;
      with procedure Write_Object
        (Stream: access Ada.Streams.Root_Stream_Type'Class;
         Item:   in Object);
   procedure Write_A (Stream: access Ada.Streams.Root_Stream_Type'Class;
                      Item:   in Name);

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
--      with procedure Read_Object
--        (Stream: access Ada.Streams.Root_Stream_Type'Class;
--         Item:   out    Object);
   procedure Read_CA (Stream: access Ada.Streams.Root_Stream_Type'Class;
                      Item:   out    Name);

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
--      with procedure Write_Object
--        (Stream: access Ada.Streams.Root_Stream_Type'Class;
--         Item:   in Object);
   procedure Write_CA (Stream: access Ada.Streams.Root_Stream_Type'Class;
                       Item:   in Name);

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
   function Get_New_CA (A_Name: in Name) return Name;

   generic
      type Object is private;
      type Name is access Object;
   procedure Free_A (A_Name: in out Name);

   generic
      type Object is abstract tagged private;
      type Name is access Object'Class;
   procedure Free_CA (A_Name: in out Name);

   Bad_Access: exception;

end Misc_Util_Accesses;
