-- $Id: protected_stores_generic.ads,v 1.5 1998/01/19 12:46:27 jgb Exp $
--

-- This package provides a protected type for storing elements.
--  Basic subprograms for accessing stores will be Put and Get.
--

-- Used inprivate part, for defining the Store.
with Lists_Generic;

generic
   -- Size of the stores (maximum number of elements in a store).
   Size: Natural;
   -- Type of elements to be stored.
   type Element is private;
   -- Function for getting a printable string from an element.
   --  (Could be used for debugging).
   with function Image (An_Element: in Element) return String is <>;

package Protected_Stores_Generic is

   pragma Elaborate_Body;

   -- Instantiation for getting a List type, which will be used
   --  for storage in private section of the protected type.
   --
   package Store_Lists is new Lists_Generic
     (Element => Element,
      Image   => Image);


   -- Protected type Store.
   --
   protected type Store is

      -- Puts an element in the store, raises Store_Full if
      --  store is full.
      -- May raise Store_Full.
      --
      procedure Put (An_Element: in Element);

      -- Gets an element in the store, blocks if store is empty.
      --
      entry Get (An_Element: out Element);

      -- Gets a printable string from the store, useful for
      --  debugging.
      --
      function Image return String;

   private
      -- Here is where elements are actually stored.
      Storage: Store_Lists.List;
      -- Busy elements.
      Busy:    Natural range 0 .. Size := 0;
   end Store;

   --------------------------------------------------------------
   -- Exceptions.
   --------------------------------------------------------------

   -- The store is full.
   --  Raised by Put.
   --
   Store_Full: exception;

end Protected_Stores_Generic;
