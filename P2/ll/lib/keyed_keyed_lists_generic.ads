-- $Id: keyed_keyed_lists_generic.ads,v 1.2 1998/01/19 12:46:21 jgb Exp $
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

--
-- Generic pacakge, providing an abstract data type which can
--  be used as a keyed list of keyed lists.
--  Key1: key of list of keyed lists.
--  Key2: key of list accessed by Key1.
--

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

package Keyed_Keyed_Lists_Generic is

   pragma Elaborate_Body;

   -------------------------------------------------------
   --
   -- Keyed Lists and auxiliary types.
   --
   -------------------------------------------------------

   -- Type for keyed lists (ADT).
   --
   type List is private;

   type List_A is access List;

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
   procedure Reset (A_List: in out List);

   -- Search a key in the list.
   -- May raise Uninitialized_List.
   --
   procedure Search (A_List: in  List;
                     A_Key1: in  Key1;
                     A_Key2: in  Key2;
                     Found:  out Boolean);

   -- Inserts an element according to the specified keys.
   --  If an element with same keys is already stored,
   --  Done returns as False, and element isn't inserted.
   --  Insertion is done according to key order.
   -- May raise Uninitialized_List, Storage_Exhausted.
   --
   procedure Insert (A_List:     in out List;
                     A_Key1:     in     Key1;
                     A_Key2:     in     Key2;
                     An_Element: in     Element;
                     Done:       out    Boolean);

   -- If found, removes the element associated with the pair of
   --  keys from a list.
   -- May raise Uninitialized_List.
   --
   procedure Remove (A_List: in out List;
                     A_Key1: in     Key1;
                     A_Key2: in     Key2;
                     Found:  out    Boolean);

   procedure Remove (A_List:          in out List;
                     A_Key1:          in     Key1;
                     A_Key2:          in     Key2;
                     Found:           out    Boolean;
                     Removed_Element: out    Element);

   procedure Replace (A_List:          in out List;
                      A_Key1:          in     Key1;
                      A_Key2:          in     Key2;
                      New_Element:     in     Element;
                      Found:           out    Boolean);

   -- If found, get the element associated with the given keys.
   -- May raise Uninitialized_List.
   --
   procedure Get_Element (A_List:     in  List;
                          A_Key1:     in  Key1;
                          A_Key2:     in  Key2;
                          An_Element: out Element;
                          Found:      out Boolean);

   -- Get the length of the sublist associated with Key1.
   --
   function Sublist_Length (A_List: in  List;
                            A_Key1: in Key1)
                            return Natural;

   -------------------------------------------------------
   -- Subprograms for helping when debugging.
   -------------------------------------------------------

   -- Get a printable string from a keyed list.
   --
   function Image (A_List: in List) return String;


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

   -- XXX: Caution!!!
   -- This should be a child package, but I've put it here
   -- because of a bug in Gnat-10p.
   --
   package Protect is

      -- Type for keyed-keyed lists (ADT).
      --
      -- !!!: List must be limited since in its implementation it
      --      is a protected object.
      --
      type List is limited private;

      -------------------------------------------------------
      -- Subprograms on keyed lists.
      -------------------------------------------------------

      -- Resets a keyed list.
      --
      procedure Reset (A_List: in out List);

      -- Search a key in the list.
      -- May raise Uninitialized_List.
      --
      procedure Search (A_List: in out List;
                        A_Key1: in     Key1;
                        A_Key2: in     Key2;
                        Found:  out    Boolean);

      -- Inserts an element according to the specified keys.
      --  If an element with same keys is already stored,
      --  Done returns as False, and element isn't inserted.
      --  Insertion is done according to key order.
      -- May raise Uninitialized_List, Storage_Exhausted.
      --
      procedure Insert (A_List:     in out List;
                        A_Key1:     in     Key1;
                        A_Key2:     in     Key2;
                        An_Element: in     Element;
                        Done:       out    Boolean);

      -- If found, removes the element associated with the pair of
      --  keys from a list.
      -- May raise Uninitialized_List.
      --
      procedure Remove (A_List: in out List;
                        A_Key1: in     Key1;
                        A_Key2: in     Key2;
                        Found:  out    Boolean);

      procedure Remove (A_List:          in out List;
                        A_Key1:          in     Key1;
                        A_Key2:          in     Key2;
                        Found:           out    Boolean;
                        Removed_Element: out    Element);

      procedure Replace (A_List:          in out List;
                         A_Key1:          in     Key1;
                         A_Key2:          in     Key2;
                         New_Element:     in     Element;
                         Found:           out    Boolean);

      -- If found, get the element associated with the given keys.
      -- May raise Uninitialized_List.
      --
      procedure Get_Element (A_List:     in out List;
                             A_Key1:     in     Key1;
                             A_Key2:     in     Key2;
                             An_Element: out    Element;
                             Found:      out    Boolean);

      -- Get a printable string from a keyed list.
      --
      function Sublist_Length (A_List: in List;
                               A_Key1: in Key1)
                               return Natural;

      -------------------------------------------------------
      -- Subprograms for helping when debugging.
      -------------------------------------------------------

      -- Get a printable string from a keyed list.
      --
      function Image (A_List: in List) return String;

   private

      -- Protected type for implementing the keyed lists.
      --
      protected type List is

        procedure Reset;

        procedure Search (A_Key1: in  Key1;
                          A_Key2: in  Key2;
                          Found:  out Boolean);

        procedure Insert (A_Key1:     in     Key1;
                          A_Key2:     in     Key2;
                          An_Element: in     Element;
                          Done:       out    Boolean);

        procedure Remove (A_Key1: in     Key1;
                          A_Key2: in     Key2;
                          Found:  out    Boolean);

        procedure Remove (A_Key1:          in     Key1;
                          A_Key2:          in     Key2;
                          Found:           out    Boolean;
                          Removed_Element: out    Element);

        procedure Replace (A_Key1:          in     Key1;
                           A_Key2:          in     Key2;
                           New_Element:     in     Element;
                           Found:           out    Boolean);

        procedure Get_Element (A_Key1:     in  Key1;
                               A_Key2:     in  Key2;
                               An_Element: out Element;
                               Found:      out Boolean);

        function Sublist_Length (A_Key1: in Key1) return Natural;

        function Image return String;

      private
        The_List: Keyed_Keyed_Lists_Generic.List;
      end List;

   end Protect;


private

   package Lists_Of_Elements is new Keyed_Lists_Generic
     (Key           => Key2,
      Element       => Element,
      Capacity      => Capacity,
      Image_Key     => Image2,
      Image_Element => Image);

   package Lists_Of_Lists is new Keyed_Lists_Generic
     (Key           => Key1,
      Element       => Lists_Of_Elements.Keyed_List_A,
      Capacity      => Capacity,
      Image_Key     => Image1,
      Image_Element => Lists_Of_Elements.Image);

   type List is record
      List_Of_Lists: Lists_Of_Lists.Keyed_List;
   end record;

end Keyed_Keyed_Lists_Generic;
