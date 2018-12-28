-- $Id: protected_stores_generic-sub.ads,v 1.1 1996/11/14 23:36:49 jgb Exp $
--

-- Subprograms interface for Protected_Stores_Generic.
--

generic
package Protected_Stores_Generic.Sub is

   -- Type provided for storing items.
   --
   type Store is limited private;

   -- Simple Put for storing an item.
   --  May raise Store_Ful.
   --
   procedure Put (A_Store: in out Store;
		  Item:    in     Element);

   -- Simple Get for getting a stored message (blocking).
   --
   procedure Get (A_Store: in out Store;
		  Item:    out    Element);

   -- Timed version of Get.
   -- May raise Timeout_Expired, if nothing is got before timeout
   --  expires.
   --
   procedure Get (A_Store: in out Store;
		  Item:    out    Element;
		  Timeout: in     Duration);

   -- Gets a printable string, useful for debugging.
   --
   function Image (A_Store: in Store) return String;

   ---------------------------------------------------------------------
   -- Exceptions.
   ---------------------------------------------------------------------

   -- A timed operation expired without being succesfully completed.
   --  Raised by Get (timed version).
   --
   Timeout_Expired: exception;
   -- A Store is full.
   --  Raised by Put.
   --
   Store_Full: exception;

private

   package Stores renames Protected_Stores_Generic;

   type Store is
      record
	 The_Store: Stores.Store;
      end record;

end Protected_Stores_Generic.Sub;
