-- $Id: double_keyed_lists_generic.adb,v 1.4 1997/08/22 10:47:12 jgb Exp $
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

with Ada.Unchecked_Deallocation;

package body Double_Keyed_Lists_Generic is

   --------------------------------------------------------------------
   -- Image (List_Element)
   --------------------------------------------------------------------

   function Image (A_List_Element: in List_Element) return String is

   begin
      return "The_Key1: " & Image1 (A_List_Element.The_Key1) &
	", The_Key2: " & Image2 (A_List_Element.The_Key2) &
	", The_Element: " & Image (A_List_Element.The_Element);
   end Image;

   --------------------------------------------------------------------
   -- Image (List_Element_A)
   --------------------------------------------------------------------

   function Image (A_List_Element: in List_Element_A) return String is

   begin
      if A_List_Element = null then
	 return "null";
      else
	 return Image (A_List_Element.all);
      end if;
   end Image;

   --------------------------------------------------------------------
   -- Free (List_Element_A)
   --  (internal use only).
   --------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => List_Element,
      Name   => List_Element_A);

   --------------------------------------------------------------------
   -- Reset
   --------------------------------------------------------------------

   procedure Reset (A_Keyed_List: in out Keyed_List) is

   begin
      Key1_Lists.Reset (A_Keyed_List.List1);
      Key2_Lists.Reset (A_Keyed_List.List2);
   end Reset;

   --------------------------------------------------------------------
   -- Search1
   --------------------------------------------------------------------

   procedure Search1 (A_Keyed_List: in  Keyed_List;
		      A_Key:        in  Key1;
		      Found:        out Boolean) is

   begin
      Key1_Lists.Search (A_Keyed_List.List1, A_Key, Found);
   end Search1;

   --------------------------------------------------------------------
   -- Search2
   --------------------------------------------------------------------

   procedure Search2 (A_Keyed_List: in  Keyed_List;
		      A_Key:        in  Key2;
		      Found:        out Boolean) is

   begin
      Key2_Lists.Search (A_Keyed_List.List2, A_Key, Found);
   end Search2;

   --------------------------------------------------------------------
   -- Insert
   --------------------------------------------------------------------
   
   procedure Insert (A_Keyed_List: in out Keyed_List;
		     A_Key1:       in     Key1;
		     A_Key2:       in     Key2;
		     An_Element:   in     Element;
		     Done:         out    Boolean) is

      New_Element: List_Element_A :=
	new List_Element'(The_Key1    => A_Key1,
			  The_Key2    => A_Key2,
			  The_Element => An_Element);
   begin
      Key1_Lists.Insert (A_Keyed_List.List1, A_Key1, New_Element, Done);
      Key2_Lists.Insert (A_Keyed_List.List2, A_Key2, New_Element, Done);
   end Insert;

   --------------------------------------------------------------------
   -- Remove1
   --------------------------------------------------------------------

   procedure Remove1 (A_Keyed_List: in out Keyed_List;
		      A_Key:        in     Key1;
		      Found:        out    Boolean) is

      Removed_Element: Element;
      Removed_Key1:    Key1;
      Removed_Key2:    Key2;
   begin
      Remove1 (A_Keyed_List, A_Key, Found,
	       Removed_Element, Removed_Key1, Removed_Key2);
   end Remove1;

   --------------------------------------------------------------------
   -- Remove2
   --------------------------------------------------------------------

   procedure Remove2 (A_Keyed_List: in out Keyed_List;
		      A_Key:        in     Key2;
		      Found:        out    Boolean) is

      Removed_Element: Element;
      Removed_Key1:    Key1;
      Removed_Key2:    Key2;
   begin
      Remove2 (A_Keyed_List, A_Key, Found,
	       Removed_Element, Removed_Key1, Removed_Key2);
   end Remove2;

   --------------------------------------------------------------------
   -- Remove1 (returning removed element)
   --------------------------------------------------------------------

   procedure Remove1 (A_Keyed_List:    in out Keyed_List;
		      A_Key:           in     Key1;
		      Found:           out    Boolean;
		      Removed_Element: out    Element) is

      Removed_Key1:    Key1;
      Removed_Key2:    Key2;
   begin
      Remove1 (A_Keyed_List, A_Key, Found,
	       Removed_Element, Removed_Key1, Removed_Key2);
   end Remove1;

   --------------------------------------------------------------------
   -- Remove2 (returning removed element)
   --------------------------------------------------------------------

   procedure Remove2 (A_Keyed_List:    in out Keyed_List;
		      A_Key:           in     Key2;
		      Found:           out    Boolean;
		      Removed_Element: out    Element) is

      Removed_Key1:    Key1;
      Removed_Key2:    Key2;
   begin
      Remove2 (A_Keyed_List, A_Key, Found,
	       Removed_Element, Removed_Key1, Removed_Key2);
   end Remove2;

   --------------------------------------------------------------------
   -- Remove1 (returning removed element and keys)
   --------------------------------------------------------------------
   
   procedure Remove1 (A_Keyed_List:    in out Keyed_List;
		      A_Key:           in     Key1;
		      Found:           out    Boolean;
		      Removed_Element: out    Element;
		      Removed_Key1:    out    Key1;
		      Removed_Key2:    out    Key2) is

      Removed_Access: List_Element_A;
   begin
      Key1_Lists.Remove (A_Keyed_List.List1, A_Key, Found, 
			 Removed_Access, Removed_Key1);
      if Found then
	 -- Remove from the other list only if were found.
	 Key2_Lists.Remove (A_Keyed_List.List2, Removed_Access.The_Key2,
			    Found,
			    Removed_Access, Removed_Key2);
	 Removed_Element := Removed_Access.The_Element;
	 Free (Removed_Access);
      end if;
   end Remove1;

   --------------------------------------------------------------------
   -- Remove2 (returning removed element and keys)
   --------------------------------------------------------------------
   
   procedure Remove2 (A_Keyed_List:    in out Keyed_List;
		      A_Key:           in     Key2;
		      Found:           out    Boolean;
		      Removed_Element: out    Element;
		      Removed_Key1:    out    Key1;
		      Removed_Key2:    out    Key2) is

      Removed_Access: List_Element_A;
   begin
      Key2_Lists.Remove (A_Keyed_List.List2, A_Key, Found, 
			 Removed_Access, Removed_Key2);
      if Found then
	 Key1_Lists.Remove (A_Keyed_List.List1, Removed_Access.The_Key1,
			    Found,
			    Removed_Access, Removed_Key1);
	 Removed_Element := Removed_Access.The_Element;
	 Free (Removed_Access);
      end if;
   end Remove2;

   --------------------------------------------------------------------
   -- Get_Element1
   --------------------------------------------------------------------

   procedure Get_Element1 (A_Keyed_List: in out Keyed_List;
			   A_Key:        in     Key1;
			   An_Element:   out    Element;
			   Found:        out    Boolean) is

      Element: List_Element_A;
   begin
      Key1_Lists.Get_Element (A_Keyed_List.List1, A_Key,
			      Element, Found);
      An_Element := Element.The_Element;
   end Get_Element1;

   --------------------------------------------------------------------
   -- Get_Element2
   --------------------------------------------------------------------

   procedure Get_Element2 (A_Keyed_List: in out Keyed_List;
			   A_Key:        in     Key2;
			   An_Element:   out    Element;
			   Found:        out    Boolean) is

      Element: List_Element_A;
   begin
      Key2_Lists.Get_Element (A_Keyed_List.List2, A_Key,
			      Element, Found);
      An_Element := Element.The_Element;
   end Get_Element2;

   --------------------------------------------------------------------
   -- Get_Head1
   --------------------------------------------------------------------

   procedure Get_Head1 (A_Keyed_List: in  Keyed_List;
			A_Key1:       out Key1;
			A_Key2:       out Key2;
			An_Element:   out Element;
			Found:        out Boolean) is

      Element: List_Element_A;
   begin
      Key1_Lists.Get_Head (A_Keyed_List.List1, A_Key1,
			   Element, Found);
      if Found then
	 A_Key2 := Element.The_Key2;
	 An_Element := Element.The_Element;
      end if;
   end Get_Head1;

   --------------------------------------------------------------------
   -- Get_Head2
   --------------------------------------------------------------------

   procedure Get_Head2 (A_Keyed_List: in  Keyed_List;
			A_Key1:       out Key1;
			A_Key2:       out Key2;
			An_Element:   out Element;
			Found:        out Boolean) is

      Element: List_Element_A;
   begin
      Key2_Lists.Get_Head (A_Keyed_List.List2, A_Key2,
			   Element, Found);
      if Found then
	 A_Key1 := Element.The_Key1;
	 An_Element := Element.The_Element;
      end if;
   end Get_Head2;

   --------------------------------------------------------------------
   -- Size_Of
   --------------------------------------------------------------------

   function Size_Of (A_Keyed_List: in Keyed_List) return Natural is

   begin
      return Key1_Lists.Size_Of (A_Keyed_List.List1);
   end Size_Of;

   --------------------------------------------------------------------
   -- Image (Keyed_List)
   --------------------------------------------------------------------

   function Image (A_Keyed_List: in Keyed_List) return String is

   begin
      return "List1: " & Key1_Lists.Image (A_Keyed_List.List1) & 
	", List2: " & Key2_Lists.Image (A_Keyed_List.List2);
   end Image;

end Double_Keyed_Lists_Generic;
