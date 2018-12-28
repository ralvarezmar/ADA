-- $Id: lists_generic.adb,v 1.11 1997/08/22 10:47:21 jgb Exp $
--


-- For managing dinamic storage.
with Unchecked_Deallocation;
-- For Image for List_A.
with Misc_Util_Accesses;

-- Used by Image.
with Debugging;

package body Lists_Generic is


   -------------------------------------------------------
   -- Free (internal use only).
   --  Give back dymanic storage used for a node, and return
   --  Null_Reference.
   --  Specification is:
   --  procedure Free (A_Reference: in out Reference);
   -------------------------------------------------------

   procedure Free is new
     Unchecked_Deallocation (Object => Node,
                             Name   => Reference);


   -------------------------------------------------------
   -- Alloc (internal use only).
   --  Allocate dynamic storage for a node. Returns a reference
   --  to that new dynamic node.
   -- Exceptions: Storage_Exhausted.
   -------------------------------------------------------

   function Alloc (An_Element: in Element;
                   Next:       in Reference)
                   return Reference is

      Tmp_Reference: Reference;
   begin
      Tmp_Reference := new Node'(Data    => An_Element,
                                 Next    => Next);
      return Tmp_Reference;
   exception
      when Storage_Error =>
         raise Storage_Exhausted;
   end Alloc;


   -------------------------------------------------------
   -- Initialize.
   -------------------------------------------------------

   procedure Initialize (A_List: in out List) is

   begin
      A_List.Head := null;
      A_List.Tail := null;
   end Initialize;


   -------------------------------------------------------
   -- Adjust.
   --
   -- !!!: We cannot use here "A_List := New_List" because that
   --      calls Adjust and enters infinite recursion.
   -------------------------------------------------------

   procedure Adjust (A_List: in out List) is

      Old_List: List;
      Current: Reference;
   begin
      -- Let's copy head and tail to new location.
      Old_List.Head := A_List.Head;
      Old_List.Tail := A_List.Tail;
      -- Let's "initialize" A_List.
      A_List.Head := Null_Reference;
      A_List.Tail := Null_Reference;
      -- Copy the list, node by node, inserting them at the tail
      --  of the destination list.
      Current := First (Old_List);
      while not Is_Out (Old_List, Current) loop
         Insert_Tail (A_List, Get_Element (Old_List, Current));
         Forward (Old_List, Current);
      end loop;
      -- !!!: Old_List is going to be out of scope, so Finalize is
      --      going to be called for it. So, we need to make it point
      --      to a null list. Otherwise, the `original' list for
      --      which this Adjust is taking place will disappear.
      Old_List.Head := null;
      Old_List.Tail := null;
   end Adjust;


   -------------------------------------------------------
   -- Finalize.
   -------------------------------------------------------

   procedure Finalize (A_List: in out List) is

   begin
      -- Run the list, freeing all the nodes.
      while A_List.Head /= Null_Reference loop
         -- We first get a reference to next node, and then
         --  free this one.
         Remove (A_List, A_List.Head);
      end loop;
   end Finalize;


   -------------------------------------------------------
   -- Reset.
   -------------------------------------------------------

   procedure Reset (A_List: in out List) is

   begin
      Finalize (A_List);
   end Reset;


   -------------------------------------------------------
   -- Copy_Ref.
   -------------------------------------------------------

--   procedure Copy_Ref (To:   in out List;
--                     From: in     List) is

--   begin
--      To.Head := From.Head;
--      To.Tail := From.Tail;
--   end Copy_Ref;

   -------------------------------------------------------
   -- Insert_Head.
   -------------------------------------------------------

   procedure Insert_Head (A_List:     in out List;
                          An_Element: in     Element) is

   begin
      A_List.Head := Alloc (An_Element, A_List.Head);
      if A_List.Tail = Null_Reference then
         -- The list was empty, update tail.
         A_List.Tail := A_List.Head;
      end if;
   end Insert_Head;


   -------------------------------------------------------
   -- Insert_Tail.
   -------------------------------------------------------

   procedure Insert_Tail (A_List:     in out List;
                          An_Element: in     Element) is

      -- Let's allocate the new node.
      Tmp_Reference: Reference :=
        Alloc (An_Element, Null_Reference);
   begin
      if A_List.Head = Null_Reference then
         -- List was empty, node is tail and head.
         A_List.Head := Tmp_Reference;
      else
         -- List wasn't empty, insert node after the tail.
         A_List.Tail.Next := Tmp_Reference;
      end if;
      A_List.Tail := Tmp_Reference;
   end Insert_Tail;


   -------------------------------------------------------
   -- Insert.
   -------------------------------------------------------

   procedure Insert (A_List:      in out List;
                     An_Element:  in     Element;
                     A_Reference: in     Reference) is

   begin
      if A_Reference = Null_Reference then
         -- Null reference, add to tail.
         Insert_Tail (A_List, An_Element);
      else
         -- Insert after reference.
         if Is_Last (A_List, A_Reference) then
            Insert_Tail (A_List, An_Element);
         else
            A_Reference.Next :=
              Alloc (An_Element, A_Reference.Next);
         end if;
      end if;
   end Insert;


   -------------------------------------------------------
   -- Replace.
   -------------------------------------------------------

   procedure Replace (A_List:      in out List;
                      An_Element:  in     Element;
                      A_Reference: in     Reference) is

   begin
      if A_Reference = Null_Reference then
         raise Out_Of_List;
      else
         A_Reference.Data := An_Element;
      end if;
   end Replace;


   -------------------------------------------------------
   -- Remove.
   -------------------------------------------------------

   procedure Remove (A_List:      in out List;
                     A_Reference: in     Reference) is

      Previous: Reference;
      Current:  Reference := A_Reference;
   begin
      if Is_Empty (A_List) then
         raise Empty_List;
      elsif Is_Out (A_List, Current) then
         raise Out_Of_List;
      elsif Is_First (A_List, Current) then
         -- Going to remove first element.
         A_List.Head := Current.Next;
         if A_List.Head = Null_Reference then
            -- Now list is empty.
            A_List.Tail := Null_Reference;
         end if;
      else
         -- `normal' case.
         Previous := Current;
         Rewind (A_List, Previous);
         Previous.Next := Current.Next;
         if Is_Last (A_List, Current) then
            -- Going to remove last node.
            A_List.Tail := Previous;
         end if;
      end if;
      Free (Current);
   end Remove;


   -------------------------------------------------------
   -- First.
   -------------------------------------------------------

   function First (A_List: in List)
                   return Reference is

   begin
      return A_List.Head;
   end First;


   -------------------------------------------------------
   -- Last.
   -------------------------------------------------------

   function Last (A_List: in List)
                  return Reference is

   begin
      return A_List.Tail;
   end Last;


   -------------------------------------------------------
   -- Get_Element.
   -------------------------------------------------------

   function Get_Element (A_List:      in List;
                         A_Reference: in Reference)
                         return Element is

   begin
      if Is_Empty (A_List) then
         raise Empty_List;
      elsif Is_Out (A_List, A_Reference) then
         raise Out_Of_List;
      else
         return A_Reference.Data;
      end if;
   end Get_Element;


   -------------------------------------------------------
   -- Forward.
   -------------------------------------------------------

   procedure Forward (A_List:      in     List;
                      A_Reference: in out Reference) is

   begin
      if Is_Empty (A_List) then
         raise Empty_List;
      elsif Is_Out (A_List, A_Reference) then
         raise Out_Of_List;
      else
         A_Reference := A_Reference.Next;
      end if;
   end Forward;


   -------------------------------------------------------
   -- Rewind.
   -------------------------------------------------------

   procedure Rewind (A_List:      in     List;
                     A_Reference: in out Reference) is

      -- Current begins pointing to the head, and will be
      --  used to run the list.
      Current: Reference := A_List.Head;
   begin
      if Is_Empty (A_List) then
         raise Empty_List;
      elsif Is_Out (A_List, A_Reference) then
         raise Out_Of_List;
      elsif Is_First (A_List, A_Reference) then
        A_Reference := Null_Reference;
      else
         -- Look for A_Reference in the list.
         while (Current /= Null_Reference) and then
               (Current.Next /= A_Reference) loop
            Current := Current.Next;
         end loop;
         if Current = Null_Reference then
            -- Reference not found
            raise Out_Of_List;
         else
            A_Reference := Current;
         end if;
      end if;
   end Rewind;


   -------------------------------------------------------
   -- Is_Empty.
   -------------------------------------------------------

   function Is_Empty (A_List: in List)
                      return Boolean is

   begin
      return A_List.Head = Null_Reference;
   end Is_Empty;


   -------------------------------------------------------
   -- Length.
   -------------------------------------------------------

   function Length (A_List: in List)
                    return Natural is

      A_Ref: Reference := First (A_List);
      The_Length: Natural := 0;
   begin
      while A_Ref /= Null_Reference loop
         The_Length := The_Length + 1;
         Forward (A_List, A_Ref);
      end loop;
      return The_Length;
   end Length;


   -------------------------------------------------------
   -- Is_First.
   -------------------------------------------------------

   function Is_First (A_List: in List;
                      A_Reference: in Reference)
                      return Boolean is

   begin
      return (A_List.Head /= Null_Reference)
        and (A_Reference = A_List.Head);
   end Is_First;


   -------------------------------------------------------
   -- Is_Last.
   -------------------------------------------------------

   function Is_Last (A_List: in List;
                     A_Reference: in Reference)
                     return Boolean is

   begin
      return (A_List.Tail /= Null_Reference)
        and (A_Reference = A_List.Tail);
   end Is_Last;


   -------------------------------------------------------
   -- Is_Out.
   -------------------------------------------------------

   function Is_Out (A_List:      in List;
                    A_Reference: in Reference)
                    return Boolean is

   begin
      return (A_Reference = Null_Reference);
   end Is_Out;


   -------------------------------------------------------
   -- Image.
   --  Uses Debugging package for building the resulting string
   --  by adding the image of each element in the list to it,
   --  from the reference on.
   -------------------------------------------------------

   function Image (A_List: in List;
                   A_Reference: in Reference)
                   return String is

      Current:       Reference              := A_Reference;
      A_String:      Debugging.Debug_String;
      First_Element: Boolean                := True;
   begin
      Debugging.Add_To (A_String, "(");
      while not Is_Out (A_List, Current) loop
         if First_Element then
            First_Element := False;
         else
            Debugging.Add_To (A_String, ", ");
         end if;
         Debugging.Add_To (A_String,
                           Image (Get_Element (A_List, Current)));
         Forward (A_List, Current);
      end loop;
      Debugging.Add_To (A_String, ")");
      return Debugging.To_String (A_String);
   end Image;


   -------------------------------------------------------
   -- Image.
   -------------------------------------------------------

   function Image (A_List: in List) return String is

   begin
      return Image (A_List, First (A_List));
   end Image;

   -------------------------------------------------------
   -- Write (List).
   -------------------------------------------------------

   procedure Write
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     in     List) is

      A_Ref: Reference := First (Item);
   begin
      Natural'Write (A_Stream, Length (Item));
      while A_Ref /= Null_Reference loop
         Element'Output (A_Stream, Get_Element (Item, A_Ref));
         Forward (Item, A_Ref);
      end loop;
   end Write;

   -------------------------------------------------------
   -- Read (List).
   -------------------------------------------------------

   procedure Read
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     out    List) is

      List_Length: Natural;
   begin
      Natural'Read (A_Stream, List_Length);
      for Count in 1 .. List_Length loop
         Insert_Tail (Item, Element'Input (A_Stream));
      end loop;
   end Read;

   -------------------------------------------------------
   -- Image (List_A).
   -------------------------------------------------------

   function Image (A_List: in List_A) return String is

      function Image_A is new Misc_Util_Accesses.Image_A
        (Object       => List,
         Name         => List_A,
         Image_Object => Image);

   begin
      return Image_A (A_List);
   end Image;

end Lists_Generic;
