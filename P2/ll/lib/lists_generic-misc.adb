-- $Id: lists_generic-misc.adb,v 1.4 1997/08/25 08:36:51 jgb Exp $
--

-- Miscellaneous operations on Lists, which doesn't need modifications
--  in List type.

package body Lists_Generic.Misc is

   -----------------------------------------------------------------
   -- Length
   -----------------------------------------------------------------

   function Length (A_List: in List) return Natural is

      The_Length: Natural := 1;
      A_Ref: Reference;
   begin
      if Is_Empty (A_List) then
         The_Length := 0;
      else
         A_Ref := First (A_List);
         while not Is_Last (A_List, A_Ref) loop
            The_Length := The_Length + 1;
            Forward (A_List, A_Ref);
         end loop;
      end if;
      return The_Length;
   end Length;

   -----------------------------------------------------------------
   -- Get_Elements
   -----------------------------------------------------------------

   function Get_Elements (A_List: in List) return Element_Array is

      The_Array: Element_Array (1 .. Length (A_List));
      A_Ref: Reference;
   begin
      if not Is_Empty (A_List) then
         A_Ref := First (A_List);
         for Count in The_Array'Range loop
            The_Array (Count) := Get_Element (A_List, A_Ref);
            Forward (A_List, A_Ref);
         end loop;
      end if;
      return The_Array;
   end Get_Elements;

   -----------------------------------------------------------------
   -- Make_List
   -----------------------------------------------------------------

   function Make_List (Elements: in Element_Array) return List is

      The_List: List;
   begin
      for Count in Elements'Range loop
         Insert_Tail (The_List, Elements (Count));
      end loop;
      return The_List;
   end Make_List;

   -----------------------------------------------------------------
   -- Is_In
   -----------------------------------------------------------------

   function Is_In (A_List:     in List;
                   An_Element: in Element) return Boolean is

      A_Ref: Reference;
      Current_Element: Element;
      Found:  Boolean := False;
   begin
      if not Is_Empty (A_List) then
         A_Ref := First (A_List);
      else
         return False;
      end if;
      while not Found loop
         Current_Element := Get_Element (A_List, A_Ref);
         if Current_Element = An_Element then
            -- Found, return True.
            Found := True;
         else
            -- Still not found.
            if Is_Last (A_List, A_Ref) then
               return False;
            else
               Forward (A_List, A_Ref);
            end if;
         end if;
      end loop;
      return Found;
   end Is_In;

   -----------------------------------------------------------------
   -- Insert_Ordered
   -- Doesn't assume an ordered list, just insert before the
   --  first element which is larger or equal than it.
   --  If this is the only procedure used for insertion, the
   --  resulting list will be ordered, from smaller to larger.
   -----------------------------------------------------------------

   procedure Insert_Ordered (A_List:     in out List;
                             An_Element: in     Element) is

      -- Temp references.
      --
      Tmp_Previous: Reference;
      Tmp_Current:  Reference;
      Done: Boolean := False;
   begin
      if not Is_Empty (A_List) then
         -- List isn't empty, let's look for my place.
         Tmp_Current := First (A_List);
         while not Done loop
            if Get_Element (A_List, Tmp_Current) >= An_Element then
               -- This is larger or equal, let's insert before it.
               if Is_First (A_List, Tmp_Current) then
                  Insert_Head (A_List, An_Element);
               else
                  Insert (A_List, An_Element, Tmp_Previous);
               end if;
               Done := True;
            else
               -- Still not the place, let's update Tmp_Previous and
               --  Tmp_Current.
               if Is_Last (A_List,Tmp_Current) then
                  -- Warning: this is the last element. No update
                  --  of Tmp_Previous, we must insert the element.
                  Insert_Tail (A_List, An_Element);
                  Done := True;
               else
                  Tmp_Previous := Tmp_Current;
                  Forward (A_List,Tmp_Current);
               end if;
            end if;
         end loop;
      else
         -- Empty list, just insert the member.
         Insert_Tail (A_List, An_Element);
      end if;
   end Insert_Ordered;

   -----------------------------------------------------------------
   -- Remove (Element).
   --  Removes first ocurence of An_Element in A_List.
   -----------------------------------------------------------------

   procedure Remove (A_List:     in out List;
                     An_Element: in Element) is

      Finished: Boolean := False;
      A_Ref: Reference;
   begin
      if not Is_Empty (A_List) then
         A_Ref := First (A_List);
         while not Finished loop
            if Get_Element (A_List, A_Ref) = An_Element then
               Remove (A_List, A_Ref);
               Finished := True;
            else
               if Is_Last (A_List, A_Ref) then
                  Finished := True;
               else
                  Forward (A_List, A_Ref);
               end if;
            end if;
         end loop;
      end if;
   end Remove;

   -----------------------------------------------------------------
   -- Remove_All
   -----------------------------------------------------------------

   procedure Remove_All (A_List: in out List) is

      A_Ref: Reference;
   begin
      while not Is_Empty (A_List) loop
         A_Ref := First (A_List);
         Remove (A_List, A_Ref);
      end loop;
   end Remove_All;

   -----------------------------------------------------------------
   -- Merge_Tail
   -----------------------------------------------------------------

   procedure Merge_Tail (A_List:   in out List;
                         To_Merge: in List;
                         Equal:    out Boolean) is

      Finished: Boolean := False;
      A_Ref: Reference;
      Current_Element: Element;
   begin
      Equal := True;
      if not Is_Empty (To_Merge) then
         A_Ref := First (To_Merge);
         while not Finished loop
            Current_Element := Get_Element (To_Merge, A_Ref);
            if not Is_In (A_List, Current_Element) then
               -- Not in list, let's add to the tail of the list.
               Insert_Tail (A_List, Current_Element);
               Equal := False;
            end if;
            if Is_Last (To_Merge, A_Ref) then
               Finished := True;
            else
               Forward (To_Merge, A_Ref);
            end if;
         end loop;
      end if;
   end Merge_Tail;

   -----------------------------------------------------------------
   -- Merge_Ordered
   -----------------------------------------------------------------

   procedure Merge_Ordered (A_List:   in out List;
                            To_Merge: in List;
                            Equal:    out Boolean) is

      Finished: Boolean := False;
      A_Ref: Reference;
      Current_Element: Element;
   begin
      Equal := True;
      if not Is_Empty (To_Merge) then
         A_Ref := First (To_Merge);
         while not Finished loop
            Current_Element := Get_Element (To_Merge, A_Ref);
            if not Is_In (A_List, Current_Element) then
               -- Not in list, let's add it to the list.
               Insert_Ordered (A_List, Current_Element);
               Equal := False;
            end if;
            if Is_Last (To_Merge, A_Ref) then
               Finished := True;
            else
               Forward (To_Merge, A_Ref);
            end if;
         end loop;
      end if;
   end Merge_Ordered;

   -----------------------------------------------------------------
   -- Merge_Ordered
   -----------------------------------------------------------------

   procedure Merge_Ordered (A_List:   in out List;
                            To_Merge: in     List;
                            Added:    out    List;
                            Equal:    out    Boolean) is

      Finished: Boolean := False;
      A_Ref: Reference;
      Current_Element: Element;
   begin
      Equal := True;
      if not Is_Empty (To_Merge) then
         A_Ref := First (To_Merge);
         while not Finished loop
            Current_Element := Get_Element (To_Merge, A_Ref);
            if not Is_In (A_List, Current_Element) then
               -- Not in list, let's add it to the list.
               Insert_Ordered (A_List, Current_Element);
               Insert_Ordered (Added, Current_Element);
               Equal := False;
            end if;
            if Is_Last (To_Merge, A_Ref) then
               Finished := True;
            else
               Forward (To_Merge, A_Ref);
            end if;
         end loop;
      end if;
   end Merge_Ordered;

   -----------------------------------------------------------------
   -- Get_In_List
   -----------------------------------------------------------------

   procedure Get_In_List (In_List:  in out List;
                          The_List: in     List;
                          To_Check: in     List) is

      Finished: Boolean := False;
      Check_Ref: Reference;
      Current_Element: Element;
   begin
      Remove_All (In_List);
      if not Is_Empty (To_Check) then
         Check_Ref := First (To_Check);
         while not Finished loop
            Current_Element := Get_Element (To_Check, Check_Ref);
            if Is_In (The_List, Current_Element) then
               Insert_Tail (In_List, Current_Element);
            end if;
            if Is_Last (To_Check, Check_Ref) then
               Finished := True;
            else
               Forward (To_Check, Check_Ref);
            end if;
         end loop;
      end if;
   end Get_In_List;

end Lists_Generic.Misc;
