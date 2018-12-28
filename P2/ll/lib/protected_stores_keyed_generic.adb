-- $Id: protected_stores_keyed_generic.adb,v 1.3 1997/05/13 18:48:29 jgb Exp $
--

with Debugging;

package body Protected_Stores_Keyed_Generic is

   ----------------------------------------------------------
   -- Debugging.
   ----------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Protected_Stores_Keyed_Generic";


   -------------------------------------------------------------------
   -- Image (for Key and Key_Record).
   -------------------------------------------------------------------

   function Image (A_Key_Record: in Key_Record)
		   return String is

   begin
      return ("(Count: " & Positive'Image (A_Key_Record.Count) &
	      ", Store: " & Storage_Sub.Image (A_Key_Record.The_Store.all) &
	      ")");
   end Image;


   -------------------------------------------------------------------
   -- Initialize.
   -------------------------------------------------------------------

   procedure Initialize (A_Store: in out Store) is

   begin
      Store_Lists.Initialize (A_Store.The_List);
   end;


   -------------------------------------------------------------------
   -- Put.
   -------------------------------------------------------------------

   procedure Put (A_Store: in out Store;
		  A_Key:   in     Key;
		  Item:    in     Element) is

      -- Information about the Key.
      A_Key_Record: Key_Record;
      -- Was the Key found.
      Found: Boolean;
   begin
      -- Let's look for info record for the Key.
      Store_Lists.Get_Element (A_Store.The_List,
			       A_Key,
			       A_Key_Record,
			       Found);
      if not Found then
	 raise Key_Not_Found;
      end if;
      -- Let's store the data.
      Storage_Sub.Put (A_Key_Record.The_Store.all, Item);
   exception
      when Storage_Sub.Store_Full =>
	 raise Store_Full;
   end Put;

   -------------------------------------------------------------------
   -- Get.
   -------------------------------------------------------------------

   procedure Get (A_Store: in out Store;
		  A_Key:   in     Key;
		  Item:    out    Element) is

      -- Information about the Key.
      A_Key_Record: Key_Record;
      -- Was the Key found.
      Found: Boolean;
   begin
      -- Let's look for info record for the Key.
      Store_Lists.Get_Element (A_Store.The_List,
			       A_Key,
			       A_Key_Record,
			       Found);
      if not Found then
	 raise Key_Not_Found;
      end if;
      -- Let's hung waiting for data.
      Storage_Sub.Get(A_Key_Record.The_Store.all, Item); 
      Debugging.Put_Line (Name & " (Get)", Debug, 
			  "A_Store: " & Image (A_Store));
   end Get;

   -------------------------------------------------------------------
   -- Get (with timeout).
   -------------------------------------------------------------------

   procedure Get (A_Store: in out Store;
		  A_Key:   in     Key;
		  Item:    out    Element;
		  Timeout: in     Duration) is

      -- Information about the Key.
      A_Key_Record: Key_Record;
      -- Was the Key found.
      Found: Boolean;
   begin
      -- Let's look for info record for the Key.
      Store_Lists.Get_Element (A_Store.The_List,
			       A_Key,
			       A_Key_Record,
			       Found);
      if not Found then
	 raise Key_Not_Found;
      end if;
      -- Let's hung waiting for data.
      Storage_Sub.Get(A_Key_Record.The_Store.all, Item, Timeout);
   exception
      when Storage_Sub.Timeout_Expired =>
	 -- Storage_Sub.Get timeout expired.
	 raise Timeout_Expired;
   end Get;

   -------------------------------------------------------------------
   -- New_Key.
   -------------------------------------------------------------------

   procedure New_Key (A_Store: in out Store;
		      A_Key:   in     Key) is

      -- Information about the Key.
      A_Key_Record: Key_Record;
      -- Was the operation done?.
      Done: Boolean;
   begin
      -- Get the record for the specified Key (if exists).
      Store_Lists.Get_Element (A_Store.The_List,
			       A_Key,
			       A_Key_Record,
			       Done);
      -- If there is no such record, create it.
      if not Done then
	 A_Key_Record.The_Store := new Storage_Sub.Store;
      end if;
      A_Key_Record.Count := A_Key_Record.Count + 1;
      -- If record is new, insert it. If not, just update.
      if not Done then
	 Store_Lists.Insert (A_Store.The_List,
			     A_Key,
			     A_Key_Record,
			     Done);
	 if not Done then
	    raise Storage_Exhausted;
	 end if;
      else
	 Store_Lists.Replace (A_Store.The_List,
			      A_Key,
			      A_Key_Record,
			      Done);
      end if;
   end New_Key;

   -------------------------------------------------------------------
   -- Free_Key.
   -------------------------------------------------------------------

   procedure Free_Key (A_Store: in out Store;
		       A_Key:   in     Key) is

      -- Information about the Key.
      A_Key_Record: Key_Record;
      -- Was the operation done?.
      Done: Boolean;
   begin
      -- Get the record for the specified Key (if exists).
      Store_Lists.Get_Element (A_Store.The_List,
			       A_Key,
			       A_Key_Record,
			       Done);
      if not Done then
	 -- If record not found, exception.
	 raise Key_Not_Found;
      end if;
      if A_Key_Record.Count <= 1 then
	 -- Record must be removed.
	 Store_Lists.Remove (A_Store.The_List,
			     A_Key,
			     Done);
      else
	 -- Just decrement the counter.
	 A_Key_Record.Count := A_Key_Record.Count - 1;
	 Store_Lists.Replace (A_Store.The_List,
			      A_Key,
			      A_Key_Record,
			      Done);
      end if;
   end Free_Key;

   -------------------------------------------------------------------
   -- Image.
   -------------------------------------------------------------------

   function Image (A_Store: in Store) return String is

   begin
      return Store_Lists.Image (A_Store.The_List);
   end Image;

end Protected_Stores_Keyed_Generic;
