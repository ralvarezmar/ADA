-- $Id: protected_stores_generic-sub.adb,v 1.1 1996/11/14 23:36:48 jgb Exp $
--

package body Protected_Stores_Generic.Sub is

   -------------------------------------------------------
   -- Put.
   -------------------------------------------------------

   procedure Put (A_Store: in out Store;
		  Item:    in     Element) is

   begin
      A_Store.The_Store.Put (Item);
   exception
      when Stores.Store_Full =>
	 raise Store_Full;
   end Put;


   -------------------------------------------------------
   -- Get (simple).
   -------------------------------------------------------

   procedure Get (A_Store: in out Store;
		  Item:    out    Element) is

   begin
      A_Store.The_Store.Get (Item);
   end Get;


   -------------------------------------------------------
   -- Get (timed).
   -------------------------------------------------------

   procedure Get (A_Store: in out Store;
		  Item:    out    Element;
		  Timeout: in     Duration) is

   begin
      select
	 A_Store.The_Store.Get (Item);
      or
	 delay Timeout;
	 raise Timeout_Expired;
      end select;
   end Get;


   -------------------------------------------------------
   -- Image.
   -------------------------------------------------------

   function Image (A_Store: in Store) return String is

   begin
      return A_Store.The_Store.Image;
   end Image;
   
end Protected_Stores_Generic.Sub;
