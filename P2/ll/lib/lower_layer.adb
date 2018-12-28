-- $Id: lower_layer.adb,v 1.13 1998/02/10 20:20:15 jgb Exp $
--
------------------------------------------------------------------------------
--                                                                          --
--    Copyright (C) 1997  Jesus M. Gonzalez Barahona                        --
--                                                                          --
--    This library is free software; you can redistribute it and/or         --
--    modify it under the terms of the GNU Library General Public           --
--    License as published by the Free Software Foundation; either          --
--    version 2 of the License, or (at your option) any later version.      --
--                                                                          --
--    This library is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     --
--    Library General Public License for more details.                      --
--                                                                          --
--    You should have received a copy of the GNU Library General Public     --
--    License along with this library; see file COPYING.LIB. If not,        --
--    write to the Free Software Foundation, Inc., 675 Mass Ave,            --
--    Cambridge, MA 02139, USA.                                             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Lower_Layer.Faults;
with Misc_Util_Accesses;
with Debugging;

package body Lower_Layer is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);
   procedure Put is new Debugging.Put_Gen
     (Name => Name, Debug => Debug);
   procedure New_Line is new Debugging.New_Line_Gen
     (Debug => Debug);

   -------------------------------------------------------------------
   -- Reset (Stream).
   --  This should be consistent with the data used as
   --  default values for the Stream type.
   -------------------------------------------------------------------

   procedure Reset
     (A_Stream: in out Stream;
      Length:   in     Ada.Streams.Stream_Element_Offset := 0) is

   begin
      A_Stream.Current_Length := Length;
      A_Stream.First          := 1;
   end Reset;

   -------------------------------------------------------------------
   -- Read (Stream).
   --  Every read operation on a stream returns, if available,
   --  as much elements as the length of the Data parameter.
   --  If several objects are Writed to the stream, the Read attribute
   --  should be smart enough to ask for the convenient number
   --  of elements so that it can discriminate where each object
   --  ends. This seems to work o.k. in Gnat.
   -------------------------------------------------------------------

   procedure Read
     (A_Stream: in out Stream;
      Data:     out    Ada.Streams.Stream_Element_Array;
      Length:   out    Ada.Streams.Stream_Element_Offset) is

      -- We need some operations on Stream_Element_Offset type.
      --
      use type Ada.Streams.Stream_Element_Offset;


   begin
      if A_Stream.Current_Length >= Data'Length then
         -- The buffer has enough elements, let's take the required ammount.
         Data (Data'First .. Data'Last) :=
           A_Stream.Data (A_Stream.First .. A_Stream.First + Data'Length - 1);
         A_Stream.First := A_Stream.First + Data'Length;
         A_Stream.Current_Length := A_Stream.Current_Length - Data'Length;
         Length := Data'Length;
      else
         -- The buffer hasn't enough elements, let's take what we have.
         Data (Data'First .. Data'First + A_Stream.Current_Length - 1) :=
           A_Stream.Data (A_Stream.First ..
                          A_Stream.First + A_Stream.Current_Length - 1);
         A_Stream.First := A_Stream.First + A_Stream.Current_Length;
         Length := A_Stream.Current_Length;
         A_Stream.Current_Length := 0;
      end if;
   end Read;


   -------------------------------------------------------------------
   -- Write (Stream).
   -------------------------------------------------------------------

   procedure Write
     (A_Stream: in out Stream;
      Data:     in     Ada.Streams.Stream_Element_Array) is

      -- We need some operations on Stream_Element_Offset type.
      --
      use type Ada.Streams.Stream_Element_Offset;

      First_Empty: Ada.Streams.Stream_Element_Offset :=
        A_Stream.First + A_Stream.Current_Length;
      Empty_Elements: Ada.Streams.Stream_Element_Offset :=
        A_Stream.Length - First_Empty + 1;
   begin
      Put ("First_Empty: " &
           Ada.Streams.Stream_Element_Offset'Image (First_Empty) &
           ", Empty_Elements: " &
           Ada.Streams.Stream_Element_Offset'Image (Empty_Elements) &
           ", Data'Length: " &
           Natural'Image (Data'Length));
      if Data'Length = 1 then
         Put (", Character: ");
         if Natural (Data (Data'First)) < 126 and then
           Natural (Data (Data'First)) > 32 then
            Put (String'(1 => Character'Val (Data (Data'First))));
         else
            Put (".");
         end if;
      else
         Put (", ");
         for Cont in Data'Range loop
            Put (".");
         end loop;
      end if;
      New_Line;
      if Empty_Elements < Data'Length then
         -- There aren't enough empty elements.
         raise Stream_Overflow;
      else
         -- There are enough empty elements.
         A_Stream.Data (First_Empty .. First_Empty + Data'Length - 1) :=
           Data (Data'First .. Data'Last);
         A_Stream.Current_Length :=
           A_Stream.Current_Length + Data'Length;
      end if;
   end Write;


   -------------------------------------------------------------------
   -- Get_All (Stream).
   -------------------------------------------------------------------

   function Get_All (A_Stream : in Stream)
                     return Ada.Streams.Stream_Element_Array is

      -- We need visibility of "+".
      use type Ada.Streams.Stream_Element_Offset;
   begin
      return A_Stream.Data (A_Stream.First ..
                            A_Stream.First + A_Stream.Current_Length - 1);
   end Get_All;

   -------------------------------------------------------------------
   -- Copy (Stream).
   -------------------------------------------------------------------

   procedure Copy (Destination: access Stream;
                   Source:      access Stream) is

      -- We need visibility of "+".
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Destination.Current_Length := Source.Current_Length;
      Destination.First := Source.First;
      Destination.Data
        (Source.First .. Source.First + Source.Current_Length - 1) :=
        Source.Data
        (Source.First .. Source.First + Source.Current_Length - 1);
   end Copy;

   -------------------------------------------------------------------
   -- Image (Stream).
   -------------------------------------------------------------------

   function Image (A_Stream: in Stream) return String is

   begin
      return ("Stream: (Length: " &
              Ada.Streams.Stream_Element_Offset'Image (A_Stream.Length) &
              ", Current_Length: "         &
              Ada.Streams.Stream_Element_Offset'Image
                (A_Stream.Current_Length) &
              ", First:  "         &
              Ada.Streams.Stream_Element_Offset'Image (A_Stream.First) &
              ")");
   end Image;


   ------------------------------------------------------
   -- Image (Address)
   ------------------------------------------------------

   function Image (An_Address: in Address) return String is

   begin
      return Ada.Tags.External_Tag (Address'Class (An_Address)'Tag);
   end Image;

   ------------------------------------------------------
   -- Bind (Address_CA)
   ------------------------------------------------------

   procedure Bind (An_Address: in Address_CA;
                   Handler:    in Handler_A := null) is

   begin
      if Is_Null (An_Address) then
         raise Null_Address;
      else
         Bind (An_Address.all, Handler);
      end if;
   end Bind;

   ------------------------------------------------------
   -- Bind_Any (Address_CA)
   ------------------------------------------------------

   procedure Bind_Any (An_Address: in Address_CA;
                       Handler:    in Handler_A := null) is

   begin
      if Is_Null (An_Address) then
         raise Null_Address;
      else
         Bind_Any (An_Address.all, Handler);
      end if;
   end Bind_Any;

   ------------------------------------------------------
   -- Unbind (Address_CA)
   ------------------------------------------------------

   procedure Unbind (An_Address: in Address_CA) is

   begin
      if Is_Null (An_Address) then
         raise Null_Address;
      else
         Unbind (An_Address.all);
      end if;
   end Unbind;

   ------------------------------------------------------
   -- Send_No_Faults (Address_CA)
   ------------------------------------------------------

   procedure Send_No_Faults (To:   in     Address_CA;
                             Data: access Stream) is

   begin
      if Is_Null (To) then
         raise Null_Address;
      else
         -- !!!: Unrestricted_Access is used  is used because if I use Access
         --      Gnat-3.05 says: "object subtype must statically
         --      match designated subtype".
         --      Unchecked_Access did the trick until Gnat-3.09
         --      (not with 3.10p)
         Send (To.all,
               Data.Data'Unrestricted_Access,
               Data.Current_Length, Data.First);
--         Reset (Data.all);
      end if;
   end Send_No_Faults;

   ------------------------------------------------------
   -- Send_No_Faults
   ------------------------------------------------------

   procedure Send_No_Faults (From:   in     Address_CA;
                             To:   in     Address_CA;
                             Data: access Stream) is

   begin
      if Is_Null (To) then
         raise Null_Address;
      else
         -- !!!: Unrestricted_Access is used  is used because if I use Access
         --      Gnat-3.05 says: "object subtype must statically
         --      match designated subtype".
         --      Unchecked_Access did the trick until Gnat-3.09
         --      (not with 3.10p)
         Send_From (From.all,
                    To.all,
                    Data.Data'Unrestricted_Access,
                    Data.Current_Length,
                    Data.First);
--         Reset (Data.all);
      end if;
   end Send_No_Faults;


   ------------------------------------------------------
   -- Send (Address_CA)
   ------------------------------------------------------

   procedure Send (To:   in     Address_CA;
                   Data: access Stream) is

   begin
      if Faults.Should_Fail (Faults.Send) or
        Faults.Should_Fail (Faults.Any) then
         Faults.Print_Failed (True, Faults.Sending);
--         Reset (Data.all);
      else
         -- No faults model or shoudn't fail.
         Faults.Print_Failed (False, Faults.Sending);
         Send_No_Faults (To, Data);
      end if;
   end Send;

   ------------------------------------------------------
   -- Send
   ------------------------------------------------------

   procedure Send (From:   in     Address_CA;
                   To:   in     Address_CA;
                   Data: access Stream) is

   begin
      if Faults.Should_Fail (Faults.Send) or
        Faults.Should_Fail (Faults.Any) then
         Faults.Print_Failed (True, Faults.Sending);
--         Reset (Data.all);
      else
         -- No faults model or shoudn't fail.
         Faults.Print_Failed (False, Faults.Sending);
         Send_No_Faults (From, To, Data);
      end if;
   end Send;


   -----------------------------------------------------------------------
   -- Receive (Address_CA)
   -----------------------------------------------------------------------

   procedure Receive (From : out Address_CA;
                      To:   in     Address_CA;
                      Data: access Stream) is

      The_Length: Ada.Streams.Stream_Element_Offset;
      Receive_More: Boolean := True;
   begin
      if Is_Null (To) then
         raise Null_Address;
      else
         Reset (Data.all);
         The_Length := Data.Length;
         -- !!!: Unrestricted_Access is used  is used because if I use Access
         --      Gnat-3.05 says: "object subtype must statically
         --      match designated subtype".
         --      Unchecked_Access did the trick until Gnat-3.09
         --      (not with 3.10p)
         while Receive_More loop
            Receive (From,
                     To => Address'Class (To.all),
                     Data => Data.Data'Unrestricted_Access,
                     Length => The_Length);
            Data.Current_Length := The_Length;
            if (Faults.Should_Fail (Faults.Receive) or
                Faults.Should_Fail (Faults.Any)) then
               Faults.Print_Failed (True, Faults.Receiving);
            else
               Faults.Print_Failed (False, Faults.Receiving);
               Receive_More := False;
            end if;
         end loop;
      end if;
   end Receive;

   -----------------------------------------------------------------------
   -- ">=" (Address_CA)
   -----------------------------------------------------------------------

   function ">=" (Left, Right: Address_CA) return Boolean is

      use type Ada.Tags.Tag;
   begin
      if Is_Null (Left) or else Is_Null (Right) then
         raise Null_Address;
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
   end ">=";

   ------------------------------------------------------
   -- "=" (Address_CA)
   ------------------------------------------------------

   function "=" (Left, Right: Address_CA) return Boolean is

   begin
      if Is_Null (Left) and Is_Null (Right) then
         return True;
      elsif Is_Null (Left) or Is_Null (Right) then
         return False;
      else
         return Left.all = Right.all;
      end if;
   end "=";

   ------------------------------------------------------
   -- Get_New (Address_CA)
   ------------------------------------------------------

   function Get_New (An_Address: in Address_CA) return Address_CA is

      function Get_New_CA is new Misc_Util_Accesses.Get_New_CA
        (Object => Address,
         Name   => Address_CA);
   begin
      return Get_New_CA (An_Address);
   end Get_New;

   ------------------------------------------------------
   -- Is_Null (Address_CA).
   ------------------------------------------------------

   function Is_Null (An_Address: in Address_CA) return Boolean is

      -- !!!: Next type is needed because "=" is overloaded for
      --      Address_CA.
      type Tmp_Address_CA is access all Address'Class;
   begin
      return Tmp_Address_CA (An_Address) = null;
   end Is_Null;

   ------------------------------------------------------
   -- Free (Address_CA).
   --
   -- XXX: I instantiate inside Free, instead of using the
   --      instantiation as implementation of Free because
   --      I'm unable of getting it done (with Gnat-3.05).
   --      I'm not sure if this is forced by Ada95 or
   --      if I'm missing something...
   ------------------------------------------------------

   procedure Free (An_Address: in out Address_CA) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Address'Class,
         Name   => Address_CA);

   begin
      if not Is_Null (An_Address) then
         Deallocate (An_Address);
         An_Address := null;
      end if;
   end Free;

   ------------------------------------------------------
   -- Image (Address_CA)
   ------------------------------------------------------

   function Image (An_Address: in Address_CA) return String is

   begin
      if Is_Null (An_Address) then
         return "null";
      else
         return Image (An_Address.all);
      end if;
   end Image;

   type Access_Marshall is (Null_Access, Non_Null_Access);

   ------------------------------------------------------
   -- Write (Address_CA)
   ------------------------------------------------------

   procedure Write
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     in     Address_CA) is

      procedure Write_CA is new Misc_Util_Accesses.Write_CA
        (Object => Address,
         Name   => Address_CA);

   begin
      Write_CA (A_Stream, Item);
   end Write;

   ------------------------------------------------------
   -- Read (Address_CA)
   ------------------------------------------------------

   procedure Read
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     out    Address_CA) is

      procedure Read_CA is new Misc_Util_Accesses.Read_CA
        (Object => Address,
         Name   => Address_CA);

   begin
      Read_CA (A_Stream, Item);
   end Read;

end Lower_Layer;
