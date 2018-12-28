-- $Id: double_keyed_lists_generic.ads,v 1.4 1998/01/19 12:46:19 jgb Exp $
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

with Keyed_Lists_Generic;

generic

   type Key1    is private;
   type Key2    is private;
   type Element is private;
   Capacity:    in Positive;
   with function ">=" (Left, Right: in Key1) return Boolean is <>;
   with function "=" (Left, Right: in Key1) return Boolean is <>;
   with function Image1 (A_Key: in Key1) return String;
   with function ">=" (Left, Right: in Key2) return Boolean is <>;
   with function "=" (Left, Right: in Key2) return Boolean is <>;
   with function Image2 (A_Key: in Key2) return String;
   with function Image (An_Element: in Element) return String is <>;

package Double_Keyed_Lists_Generic is

   pragma Elaborate_Body;

   -------------------------------------------------------
   --
   -- Keyed Lists and auxiliary types.
   --
   -------------------------------------------------------

   -- Type for keyed lists (ADT).
   --
   type Keyed_List is private;

   type Keyed_List_A is access Keyed_List;

   --
   -- Some types for handling contents of the lists.
   --
   type Keys1_Array    is array (Natural range <>) of Key1;
   type Keys2_Array    is array (Natural range <>) of Key2;
   type Elements_Array is array (Natural range <>) of Element;

   -------------------------------------------------------
   -- Subprograms on keyed lists.
   -------------------------------------------------------

   -- Resets a keyed list.
   --
   procedure Reset (A_Keyed_List: in out Keyed_List);

   -- Search a key in the list.
   -- May raise Uninitialized_List.
   --
   procedure Search1 (A_Keyed_List: in  Keyed_List;
                      A_Key:        in  Key1;
                      Found:        out Boolean);

   procedure Search2 (A_Keyed_List: in  Keyed_List;
                      A_Key:        in  Key2;
                      Found:        out Boolean);

   -- Inserts an element and its associated key. If key is already
   --  stored, Done returns as False, and element isn't inserted.
   --  Insertion is done according to key order.
   -- May raise Uninitialized_List, Storage_Exhausted.
   --
   procedure Insert (A_Keyed_List: in out Keyed_List;
                     A_Key1:       in     Key1;
                     A_Key2:       in     Key2;
                     An_Element:   in     Element;
                     Done:         out    Boolean);


   -- If found, removes a key (and its associated element) from a list.
   -- May raise Uninitialized_List.
   --
   procedure Remove1 (A_Keyed_List: in out Keyed_List;
                      A_Key:        in     Key1;
                      Found:        out    Boolean);

   procedure Remove2 (A_Keyed_List: in out Keyed_List;
                      A_Key:        in     Key2;
                      Found:        out    Boolean);

   -- Sometimes we want the removed element (for instance, to free it
   --  if it is an access).
   --
   procedure Remove1 (A_Keyed_List:    in out Keyed_List;
                      A_Key:           in     Key1;
                      Found:           out    Boolean;
                      Removed_Element: out    Element);

   procedure Remove2 (A_Keyed_List:    in out Keyed_List;
                      A_Key:           in     Key2;
                      Found:           out    Boolean;
                      Removed_Element: out    Element);

   -- And sometimes we want even the removed keys (for instance, if
   --  it is an access pointing to some object that we want to
   --  deallocate...
   --
   procedure Remove1 (A_Keyed_List:    in out Keyed_List;
                      A_Key:           in     Key1;
                      Found:           out    Boolean;
                      Removed_Element: out    Element;
                      Removed_Key1:    out    Key1;
                      Removed_Key2:    out    Key2);

   procedure Remove2 (A_Keyed_List:    in out Keyed_List;
                      A_Key:           in     Key2;
                      Found:           out    Boolean;
                      Removed_Element: out    Element;
                      Removed_Key1:    out    Key1;
                      Removed_Key2:    out    Key2);

   -- If found, get the element associated with the given key.
   -- May raise Uninitialized_List.
   --
   procedure Get_Element1 (A_Keyed_List: in out Keyed_List;
                           A_Key:        in     Key1;
                           An_Element:   out    Element;
                           Found:        out    Boolean);

   procedure Get_Element2 (A_Keyed_List: in out Keyed_List;
                           A_Key:        in     Key2;
                           An_Element:   out    Element;
                           Found:        out    Boolean);

   -- Get the keys and the element stored at the head of the list.
   --
   procedure Get_Head1 (A_Keyed_List: in  Keyed_List;
                        A_Key1:       out Key1;
                        A_Key2:       out Key2;
                        An_Element:   out Element;
                        Found:        out Boolean);

   procedure Get_Head2 (A_Keyed_List: in  Keyed_List;
                        A_Key1:       out Key1;
                        A_Key2:       out Key2;
                        An_Element:   out Element;
                        Found:        out Boolean);

   -- Actual size of the list (number of elements stored).
   -- May raise Uninitialized_List.
   --
   function Size_Of (A_Keyed_List: in Keyed_List) return Natural;


   -------------------------------------------------------
   -- Subprograms for helping when debugging.
   -------------------------------------------------------

   -- Get a printable string from a keyed list.
   --
   function Image (A_Keyed_List: in Keyed_List) return String;


   -----------------------------------------------------
   -- Exceptions.
   -----------------------------------------------------

   -- An operation was issued on an uninitialized list.
   -- Raised by Search, Insert, Remove, Replace, Get_Element,
   --  Size_Of.
   --
   Uninitialized_List: exception;

   -- There is no more space available for new nodes in the list.
   --  Raised by Initialize, Insert.
   --
   Storage_Exhausted: exception;

   -- The list is malformed (maintains unconsistent information).
   --  Raised by Get_Elements, Get_Keys.
   --
   Bad_List: exception;

private

   type List_Element is record
      The_Key1:    Key1;
      The_Key2:    Key2;
      The_Element: Element;
   end record;

   function Image (A_List_Element: in List_Element) return String;

   type List_Element_A is access List_Element;

   function Image (A_List_Element: in List_Element_A) return String;

   package Key1_Lists is new Keyed_Lists_Generic
     (Key           => Key1,
      Element       => List_Element_A,
      Capacity      => Capacity,
      Image_Key     => Image1,
      Image_Element => Image);

   package Key2_Lists is new Keyed_Lists_Generic
     (Key           => Key2,
      Element       => List_Element_A,
      Capacity      => Capacity,
      Image_Key     => Image2,
      Image_Element => Image);

   type Keyed_List is record
      List1: Key1_Lists.Keyed_List;
      List2: Key2_Lists.Keyed_List;
   end record;

end Double_Keyed_Lists_Generic;
