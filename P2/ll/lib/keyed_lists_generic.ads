-- $Id: keyed_lists_generic.ads,v 1.16 1998/01/19 12:46:23 jgb Exp $
--

-- Keyed list. Each element in the list has an associated key, which
--  can be used for inserting an element, looking for it, removing it,
--  etc. Elements will be maintained in key order. For this implementation,
--  generic lists will be used.
--  It is heavily based on the ADT provided by M. Feldman in
--  his book `Ada 95 Software Construction and Data Structures'.
--


-- Keyed lists will be controlled.
with Ada.Finalization;

-- We'll implement keyed lists with lists.
--  (see private section)
with Lists_Generic;

generic

   -- Key associated with an element.
   type Key     is private;
   -- Elements to be stored.
   type Element is private;
   -- Capacity of the keyed list.
   Capacity: in Positive;
   -- Comparation between keys (list will be maintained ordered).
   with function ">=" (Left, Right: in Key) return Boolean is <>;
   with function "=" (Left, Right: in Key) return Boolean is <>;
   -- For getting a string from a keyed list.
--   with function Image (A_Key: in Key;
--                      An_Element: in Element)
--                      return String is <>;
   with function Image_Key (A_Key: in Key) return String;
   with function Image_Element (An_Element: in Element) return String;

package Keyed_Lists_Generic is

   pragma Elaborate_Body;

   -------------------------------------------------------
   -- Renaming.
   -------------------------------------------------------

   package Final renames Ada.Finalization;


   -------------------------------------------------------
   --
   -- Keyed Lists and auxiliary types.
   --
   -------------------------------------------------------

   -- Type for keyed lists (ADT).
   --
   type Keyed_List is new Final.Controlled with private;

   -- Overridding of initialization.
   --
   procedure Initialize (A_Keyed_List: in out Keyed_List);

   -- Overridding of finalization.
   --
   procedure Finalize (A_Keyed_List: in out Keyed_List);

   type Keyed_List_A is access Keyed_List;

   function Is_Null (A_Keyed_List_A: in Keyed_List_A) return Boolean;

   function Image (A_Keyed_List_A: in Keyed_List_A) return String;

   --
   -- Some types for handling contents of the lists.
   --
   type Keys_Array     is array (Natural range <>) of Key;
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
   procedure Search (A_Keyed_List: in  Keyed_List;
                     A_Key:        in  Key;
                     Found:        out Boolean);


   -- Inserts an element and its associated key. If key is already
   --  stored, Done returns as False, and element isn't inserted.
   --  Insertion is done according to key order.
   -- May raise Uninitialized_List, Storage_Exhausted.
   --
   procedure Insert (A_Keyed_List: in out Keyed_List;
                     A_Key:        in     Key;
                     An_Element:   in     Element;
                     Done:         out    Boolean);


   -- If found, removes a key (and its associated element) from a list.
   -- May raise Uninitialized_List.
   --
   procedure Remove (A_Keyed_List: in out Keyed_List;
                     A_Key:        in     Key;
                     Found:        out    Boolean);

   -- Sometimes we want the removed element (for instance, to free it
   --  if it is an access).
   --
   procedure Remove (A_Keyed_List:    in out Keyed_List;
                     A_Key:           in     Key;
                     Found:           out    Boolean;
                     Removed_Element: out    Element);

   -- And sometimes we want even the removed key (for instance, if
   --  it is an access pointing to some object that we want to
   --  deallocate...
   --
   procedure Remove (A_Keyed_List:    in out Keyed_List;
                     A_Key:           in     Key;
                     Found:           out    Boolean;
                     Removed_Element: out    Element;
                     Removed_Key:     out    Key);

   -- If found, replaces the element associated with given key by the
   --  the new element.
   -- May raise Uninitialized_List.
   --
   procedure Replace (A_Keyed_List: in out Keyed_List;
                      A_Key:        in     Key;
                      New_Element:  in     Element;
                      Found:        out    Boolean);


   -- If found, get the element associated with the given key.
   -- May raise Uninitialized_List.
   --
   procedure Get_Element (A_Keyed_List: in  Keyed_List;
                          A_Key:        in  Key;
                          An_Element:   out Element;
                          Found:        out Boolean);

   -- Get the key and the element stored at the head of the list.
   --
   procedure Get_Head (A_Keyed_List: in  Keyed_List;
                       A_Key:        out Key;
                       An_Element:   out Element;
                       Found:        out Boolean);

   -- Returns the list of elements stored in the list, if any.
   -- May raise Uninitialized_List, Bad_List.
   --
   function Get_Elements (A_Keyed_List: in Keyed_List)
                          return Elements_Array;


   -- Returns the list of keys stored in the list, if any.
   -- May raise Uninitialized_List, Bad_List.
   --
   function Get_Keys (A_Keyed_List: in Keyed_List)
                      return Keys_Array;


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


   package Protect is

      -- Type for keyed lists (ADT).
      --
      -- !!!: Keyed_List must be limited since in its implementation it
      --      is a protected object.
      --
      type Keyed_List is limited private;

      -------------------------------------------------------
      -- Subprograms on keyed lists.
      -------------------------------------------------------

      -- Before using a list, it must be initialized.
      --
      procedure Initialize (A_Keyed_List: in out Keyed_List);


      -- Search a key in the list.
      -- May raise Uninitialized_List.
      --
      procedure Search (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        Found:        out    Boolean);


      -- Inserts an element and its associated key. If key is already
      --  stored, Done returns as False, and element isn't inserted.
      --  Insertion is done according to key order.
      -- May raise Uninitialized_List.
                                                  --
      procedure Insert (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        An_Element:   in     Element;
                        Done:         out    Boolean);


      -- If found, removes a key (and its associated element) from a list.
      -- May raise Uninitialized_List.
      --
      procedure Remove (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        Found:        out    Boolean);

      -- And sometimes we want even the removed key (for instance, if
      --  it is an access pointing to some object that we want to
      --  deallocate...
      --
      procedure Remove (A_Keyed_List:    in out Keyed_List;
                        A_Key:           in     Key;
                        Found:           out    Boolean;
                        Removed_Element: out    Element;
                        Removed_Key:     out    Key);

      -- If found, replaces the element associated with given key by the
      --  the new element.
      -- May raise Uninitialized_List.
      --
      procedure Replace (A_Keyed_List: in out Keyed_List;
                         A_Key:        in     Key;
                         New_Element:  in     Element;
                         Found:        out    Boolean);


      -- If found, get the element associated with the given key.
      -- May raise Uninitialized_List.
      --
      procedure Get_Element (A_Keyed_List: in out Keyed_List;
                             A_Key:        in     Key;
                             An_Element:   out    Element;
                             Found:        out    Boolean);


      -- Returns the list of elements stored in the list, if any.
      -- May raise Uninitialized_List, Bad_List.
      --
      function Get_Elements (A_Keyed_List: in Keyed_List)
                             return Elements_Array;


      -- Returns the list of keys stored in the list, if any.
      -- May raise Uninitialized_List, Bad_List.
      --
      function Get_Keys (A_Keyed_List: in Keyed_List)
                         return Keys_Array;


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

   private

      -- Protected type for implementing the keyed lists.
      --
      protected type Keyed_List is
        procedure Initialize;
        procedure Search (A_Key: in  Key;
                          Found: out Boolean);
        procedure Insert (A_Key:      in  Key;
                          An_Element: in  Element;
                          Done:       out Boolean);
        procedure Remove (A_Key: in  Key;
                          Found: out Boolean);
        procedure Remove (A_Key:           in     Key;
                          Found:           out    Boolean;
                          Removed_Element: out    Element;
                          Removed_Key:     out    Key);
        procedure Replace (A_Key:       in  Key;
                           New_Element: in  Element;
                           Found:       out Boolean);
        procedure Get_Element (A_Key:      in  Key;
                               An_Element: out Element;
                               Found:      out Boolean);
        function Get_Elements return Elements_Array;
        function Get_Keys return Keys_Array;
        function Size_Of return Natural;
        function Image return String;
      private
         The_List: Keyed_Lists_Generic.Keyed_List;
      end Keyed_List;

   end Protect;

private

   subtype Index is Positive range 1 .. Capacity;
   subtype Size  is Natural  range 0 .. Capacity;

   type List_Element is
      record
         The_Key:     Key;
         The_Element: Element;
      end record;

   function Image (A_List_Element: in List_Element) return String;

   package Lists is new Lists_Generic (Element => List_Element,
                                       Image   => Image);

   type Keyed_List is new Final.Controlled with record
      Current_Size: Size;
      Data:         Lists.List;
   end record;

end Keyed_Lists_Generic;
