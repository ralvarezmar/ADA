-- $Id: misc_util_accesses.adb,v 1.5 1997/12/30 11:21:44 jgb Exp $
--

with Ada.Tags;
with Ada.Unchecked_Deallocation;

package body Misc_Util_Accesses is

   -----------------------------------------------------------------------
   -- Is_Null_A
   -----------------------------------------------------------------------

   function Is_Null_A (A_Name: in Name) return Boolean is

      -- Just in case "=" is redefined, we use other type.
      type Tmp_Name is access all Object;
   begin
      return Tmp_Name (A_Name) = null;
   end Is_Null_A;

   -----------------------------------------------------------------------
   -- Image_A
   -----------------------------------------------------------------------

   function Image_A (A_Name: in Name) return String is

      function Is_Null is new Is_Null_A (Object, Name);
   begin
      if Is_Null (A_Name) then
         return "null";
      else
         return Image_Object (A_Name.all);
      end if;
   end Image_A;

   -----------------------------------------------------------------------
   -- Is_Null_CA
   -----------------------------------------------------------------------

   function Is_Null_CA (A_Name: in Name) return Boolean is

      -- Just in case "=" is redefined, we use other type.
      type Tmp_Name is access all Object;
   begin
      return Tmp_Name (A_Name) = null;
   end Is_Null_CA;

   -----------------------------------------------------------------------
   -- Image_CA
   -----------------------------------------------------------------------

   function Image_CA (A_Name: in Name) return String is

      function Is_Null is new Is_Null_CA (Object, Name);
   begin
      if Is_Null (A_Name) then
         return "null";
      else
         return Image_Class (A_Name.all);
      end if;
   end Image_CA;

   -----------------------------------------------------------------------
   -- Equal
   -----------------------------------------------------------------------

   function Equal (Left, Right: in Name) return Boolean is

      function Is_Null is new Is_Null_A (Object, Name);
   begin
      if Is_Null (Left) or else Is_Null (Right) then
         return False;
      else
         return Left.all = Right.all;
      end if;
   end Equal;

   -----------------------------------------------------------------------
   -- Greater_Equal_CA
   -----------------------------------------------------------------------

   function Greater_Equal_CA (Left, Right: in Name) return Boolean is

      use type Ada.Tags.Tag;
      function Is_Null is new Is_Null_CA (Object, Name);
   begin
      if Is_Null (Left) or else Is_Null (Right) then
         raise Bad_Access;
      else
         -- Not equal, let's check tag.
         if Left.all'Tag = Right.all'Tag then
            -- Same tag, let's compare them
            return Left.all >= Right.all;
         else
            -- Different tag, let's compare their external tags.
            return Ada.Tags.External_Tag (Left.all'Tag) >=
              Ada.Tags.External_Tag (Right.all'Tag);
         end if;
      end if;
   end Greater_Equal_CA;

   type Access_Marshalled is (Null_Access, Non_Null_Access);

   -----------------------------------------------------------------------
   -- Read_A
   -----------------------------------------------------------------------

   procedure Read_A (Stream: access Ada.Streams.Root_Stream_Type'Class;
                     Item:   out    Name) is

      Content: Access_Marshalled;
   begin
      Access_Marshalled'Read (Stream, Content);
      if Content = Null_Access then
         Item := null;
      else
         Item := new Object'(Object'Input (Stream));
      end if;
   end Read_A;

   -----------------------------------------------------------------------
   -- Write_A
   -----------------------------------------------------------------------

   procedure Write_A (Stream: access Ada.Streams.Root_Stream_Type'Class;
                      Item:   in Name) is

      function Is_Null is new Is_Null_A (Object, Name);
   begin
      if Is_Null (Item) then
         Access_Marshalled'Write (Stream, Null_Access);
      else
         Access_Marshalled'Write (Stream, Non_Null_Access);
         Object'Output (Stream, Item.all);
      end if;
   end Write_A;

   -----------------------------------------------------------------------
   -- Read_CA
   -----------------------------------------------------------------------

   procedure Read_CA (Stream: access Ada.Streams.Root_Stream_Type'Class;
                      Item:   out    Name) is

      Content: Access_Marshalled;
   begin
      Access_Marshalled'Read (Stream, Content);
      if Content = Null_Access then
         Item := null;
      else
         Item := new Object'Class'(Object'Class'Input (Stream));
      end if;
   end Read_CA;

   -----------------------------------------------------------------------
   -- Write_CA
   -----------------------------------------------------------------------

   procedure Write_CA (Stream: access Ada.Streams.Root_Stream_Type'Class;
                       Item:   in Name) is

      function Is_Null is new Is_Null_CA (Object, Name);
   begin
      if Is_Null (Item) then
         Access_Marshalled'Write (Stream, Null_Access);
      else
         Access_Marshalled'Write (Stream, Non_Null_Access);
         Object'Class'Output (Stream, Object'Class (Item.all));
      end if;
   end Write_CA;

   -----------------------------------------------------------------------
   -- Get_New_CA
   -----------------------------------------------------------------------

   function Get_New_CA (A_Name: in Name) return Name is

      function Is_Null is new Is_Null_CA (Object, Name);
   begin
      if Is_Null (A_Name) then
         return null;
      else
         return new Object'Class' (A_Name.all);
      end if;
   end Get_New_CA;

   -----------------------------------------------------------------------
   -- Free_A
   -----------------------------------------------------------------------

   procedure Free_A (A_Name: in out Name) is

      function Is_Null is new Is_Null_A (Object, Name);

      procedure Free_Name is new Ada.Unchecked_Deallocation
        (Object => Object,
         Name   => Name);
   begin
      if not Is_Null (A_Name) then
         Free_Name (A_Name);
         A_Name := null;
      end if;
   end Free_A;

   -----------------------------------------------------------------------
   -- Free_CA
   -----------------------------------------------------------------------

   procedure Free_CA (A_Name: in out Name) is

      function Is_Null is new Is_Null_CA (Object, Name);

      procedure Free_Name is new Ada.Unchecked_Deallocation
        (Object => Object'Class,
         Name   => Name);
   begin
      if not Is_Null (A_Name) then
         Free_Name (A_Name);
         A_Name := null;
      end if;
   end Free_CA;

end Misc_Util_Accesses;
