-- $Id: protected_stores_keyed_generic.ads,v 1.5 1998/01/19 12:46:28 jgb Exp $
--

-- For private part (Store is controlled).
with Ada.Finalization;

-- For private part.
with Protected_Stores_Generic;

-- For private part.
with Protected_Stores_Generic.Sub;

-- For private part.
with Keyed_Lists_Generic;

-- For private part.
-- XXX: Gnat-3.10p bug
--with Keyed_Lists_Generic.Protect;

generic
   -- Size of the stores (maximun number of elements in a store,
   --  for a given key).
   Size: Natural;
   -- Maximun number of keys.
   Key_No: Natural;
   -- Type of elements to be stored.
   type Element is private;
   -- Function for getting a printable string from an element.
   --  (Could be used for debugging).
   with function Image_Element (An_Element: in Element) return String is <>;
   -- Type of keys.
   --
   type Key is private;
   with function "=" (Left, Right: in Key) return Boolean is <>;
   with function ">=" (Left, Right: in Key) return Boolean is <>;
   -- Function for getting a printable string from an element.
   --  (Could be used for debugging).
   with function Image_Key (A_Key: in Key) return String is <>;

package Protected_Stores_Keyed_Generic is

   pragma Elaborate_Body;

   -- Store of application data, ordered by key.
   --
   type Store is limited private;

   -- Overridding of Initialize for controlled type.
   --  Can also be used for `emptying' a store.
   --
   procedure Initialize (A_Store: in out Store);


   -- May raise Key_Not_Found.
   --
   procedure Put (A_Store: in out Store;
                  A_Key:   in     Key;
                  Item:    in     Element);

   -- May raise Key_Not_Found.
   --
   procedure Get (A_Store: in out Store;
                  A_Key:   in     Key;
                  Item:    out    Element);

   -- May raise Key_Not_Found.
   --
   procedure Get (A_Store: in out Store;
                  A_Key:   in     Key;
                  Item:    out    Element;
                  Timeout: in     Duration);

   procedure New_Key (A_Store: in out Store;
                      A_Key:   in     Key);

   -- May raise Key_Not_Found.
   --
   procedure Free_Key (A_Store: in out Store;
                       A_Key:   in     Key);

   -- May raise Storage_Exhausted.
   --
   function Image (A_Store: in Store) return String;

   -------------------------------------------------------------------
   -- Exceptions.
   -------------------------------------------------------------------

   -- Information about the key was not found in the Store.
   -- Raised by Put, Get, Free_Key.
   --
   Key_Not_Found: exception;

   -- No more keys may be `open'.
   -- Raised by New_Key.
   --
   Storage_Exhausted: exception;

   -- Storage for a key is full.
   -- Raised by Put.
   --
   Store_Full: exception;

   -- Timeout expired while waiting for data.
   -- Raised by Get (with timeout).
   --
   Timeout_Expired: exception;

private

   ----------------------------------------------------------
   -- Renaming.
   ----------------------------------------------------------

   package Final renames Ada.Finalization;

   -- Storage for data associated to a key will be managed
   --  by using Protected_Stores_Generic.Sub.
   --
   package Storage is new Protected_Stores_Generic
     (Size    => Size,
      Element => Element,
      Image   => Image_Element);

   package Storage_Sub is new Storage.Sub;


   type Store_Ref is access Storage_Sub.Store;

   -- Each key will have an associated Key_Record, holding
   --  a counter (number of New_Key operations minus number of Free_Key
   --  operations on that key) and an access to the store where
   --  elements for that key will actually be stored.
   --
   type Key_Record is
      record
         -- Times New_Channel was issued - times Free_Channel was issued.
         --  Should at least be 1 is record exists in list.
         Count:     Natural := 0;
         -- Store for data of a channel.
         The_Store: Store_Ref;
      end record;


   -- Image, needed for instantiating Store_Lists_Unprotected.
   --
   function Image (A_Key_Record: in Key_Record)
                   return String;

   -- Instantiation of a package providing keyed lists.
   --
   package Store_Lists_Unprotected is new Keyed_Lists_Generic
     (Key           => Key,
      Element       => Key_Record,
      Capacity      => Key_No,
      ">="          => ">=",
      "="           => "=",
      Image_Key     => Image_Key,
      Image_Element => Image);

   -- This child package provides keyed lists as a protected type.
   --
   -- Gnat-3.10p bug
   -- package Store_Lists is new Store_Lists_Unprotected.Protect;
   package Store_Lists renames Store_Lists_Unprotected.Protect;

   -- Store is controlled.
--   type Store is new Final.Controlled with record
   type Store is record
      The_List: Store_Lists.Keyed_List;
   end record;

end Protected_Stores_Keyed_Generic;
