-- $Id: lists_generic.ads,v 1.12 1998/01/19 12:46:25 jgb Exp $
--

-- This package provides single linked lists as ADTs.
--  It is heavily based on the one provided by M. Feldman in
--  his book `Ada 95 Software Construction and Data Structures'.
--

-- List is controlled.
with Ada.Finalization;
with Ada.Streams;

generic
   -- Type of elements to be stored in the list.
   type Element is private;
   -- Function for getting a printable string from an element.
   --  (Could be used for debugging).
   with function Image (An_Element: in Element) return String;

package Lists_Generic is

   pragma Elaborate_Body;

   -------------------------------------------------------
   -- Renaming.
   -------------------------------------------------------

   package Final renames Ada.Finalization;

   -------------------------------------------------------
   --
   -- Reference list.
   --
   -------------------------------------------------------

   -- Type for references (accesses) to nodes of the list.
   --
   type Reference is private;

   -- Null reference.
   Null_Reference: constant Reference;


   -------------------------------------------------------
   --
   -- List type and primitive subprograms.
   --
   -------------------------------------------------------

   -- ADT for building single linked lists.
   -- !!!: It is controlled since we want to redefine Adjust so that
   --      asignation will copy all the list, not just the record
   --      with the pointers.
   --
   type List is new Final.Controlled with private;

   Null_List: constant List;

   -- Initializes a list, or resets it to empty if was already
   --  initizalized.
   -- May raise Storage_Exhausted.
   --
   procedure Initialize (A_List: in out List);

   -- For redefining asignation, so that it copies lists.
   -- Exceptions: Storage_Exhausted.
   --
   procedure Adjust (A_List: in out List);

   -- Free a list.
   --
   procedure Finalize (A_List: in out List);

   -- Access to a list.
   --
   type List_A is access all List;

   -------------------------------------------------------
   -- Subprograms for managing lists.
   -------------------------------------------------------

   -- Reset a list.
   --
   procedure Reset (A_List: in out List);

   -- Get a new list by only copying the reference (after
   --  copying, both lists are in fact the same one).
   --  Use with caution!!!
   --
--   procedure Copy_Ref (To:   in out List;
--                     From: in     List);

   -------------------------------------------------------
   -- Subprograms for modifying nodes of a list.
   -------------------------------------------------------

   -- Insert an element as head of the list.
   -- May raise Storage_Exhausted.
   --
   procedure Insert_Head (A_List:     in out List;
                          An_Element: in     Element);


   -- Insert an element as tail of the list.
   -- May raise Storage_Exhausted.
   --
   procedure Insert_Tail (A_List:     in out List;
                          An_Element: in     Element);


   -- Inserts an element after a reference. If the reference
   --  is Null_Reference, insert the element as tail of the list.
   -- May raise Storage_Exhausted.
   --
   procedure Insert (A_List:      in out List;
                     An_Element:  in     Element;
                     A_Reference: in     Reference);


   -- Replace the element in node referenced by An_Element.
   -- May raise Out_Of_List.
   --
   procedure Replace (A_List:      in out List;
                      An_Element:  in     Element;
                      A_Reference: in     Reference);


   -- Remove the referenced node.
   -- May raise Out_Of_List, Empty_List.
   --
   procedure Remove (A_List:      in out List;
                     A_Reference: in     Reference);


   -------------------------------------------------------
   -- Subprograms for selecting nodes of a list.
   -------------------------------------------------------

   -- Reference to the first element of a list.
   --
   function First (A_List: in List)
                   return Reference;


   -- Reference to the last element of a list.
   --
   function Last (A_List: in List)
                  return Reference;


   -- Get the element stored by referenced node.
   -- May raise Empty_List, Out_Of_List.
   --
   function Get_Element (A_List:      in List;
                         A_Reference: in Reference)
                         return Element;


   -------------------------------------------------------
   -- Subprograms for iterating a list.
   -------------------------------------------------------

   -- Gets the a reference to the next node in the list.
   --  If the reference was to the last node, gets a
   --  Null_Reference.
   -- May raise Empty_List, Out_Of_List.
   --
   procedure Forward (A_List:      in     List;
                      A_Reference: in out Reference);


   -- Gets the a reference to the previous node in the list.
   --  If the reference was to the first node, gets a
   --  Null_Reference.
   -- May raise Empty_List, Out_Of_List.
   --
   procedure Rewind (A_List:      in     List;
                     A_Reference: in out Reference);


   -------------------------------------------------------
   -- Subprograms for getting info of a list.
   -------------------------------------------------------

   -- Is list empty?
   --
   function Is_Empty (A_List: in List)
                      return Boolean;


   -- Length (number of elements) of a list.
   --
   function Length (A_List: in List)
                    return Natural;


   -- Is referenced node the first in the list?
   --
   function Is_First (A_List: in List;
                      A_Reference: in Reference)
                      return Boolean;


   -- Is referenced node the last in the list?
   --
   function Is_Last (A_List: in List;
                     A_Reference: in Reference)
                     return Boolean;


   -- Is referenced node out of the list? (for being used in
   --  cooperation with iterators, could also mean is the
   --  referenced node before the beggining or after the end
   --  of the list?
   --
   function Is_Out (A_List:      in List;
                    A_Reference: in Reference)
                    return Boolean;


   -------------------------------------------------------
   -- Subprograms for helping when debugging.
   -------------------------------------------------------

   -- Get a printable string from part of a list (from the
   --  reference on).
   function Image (A_List: in List;
                   A_Reference: in Reference)
                   return String;


   -- Get a printable string from a List.
   function Image (A_List: in List) return String;


   -- Get a printable string from a list access.
   function Image (A_List: in List_A) return String;

   -------------------------------------------------------
   -- Exceptions.
   -------------------------------------------------------

   -- There is no more space available for new nodes in the list.
   --  Raised by Initialize, Copy, Isert_Head, Insert_Tail, Insert.
   Storage_Exhausted: exception;

   -- A node referencing out of a list tried to access a list.
   --  Raised by Replace, Remove, Get_Element, Forward, Rewind.
   Out_Of_List:       exception;

   -- List is empty, and a operation valid only on a non-empty list
   --  was issued.
   --  Raised by Remove, Get_Element, Forward, Rewind.
   Empty_List:        exception;


private

   type Node;
   type Reference is access Node;

   Null_Reference: constant Reference := null;

   type Node is record
      Data:    Element;
      Next:    Reference;
   end record;

   type List is new Final.Controlled with record
      Head: Reference;
      Tail: Reference;
   end record;

   procedure Write
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     in     List);
   for List'Write use Write;

   procedure Read
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     out    List);
   for List'Read use Read;

   Null_List: constant List :=
     (Final.Controlled with
      Head => null,
      Tail => null);

end Lists_Generic;
