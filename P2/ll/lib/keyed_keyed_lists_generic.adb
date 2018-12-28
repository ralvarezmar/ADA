-- $Id: keyed_keyed_lists_generic.adb,v 1.2 1998/01/19 12:46:20 jgb Exp $
--

package body Keyed_Keyed_Lists_Generic is

   ---------------------------------------------------------------------
   -- Reset
   -- XXX: All lists should be enptied before resetting the list
   --      of lists.
   ---------------------------------------------------------------------

   procedure Reset (A_List: in out List) is

   begin
      Lists_Of_Lists.Reset (A_List.List_Of_Lists);
   end Reset;

   ---------------------------------------------------------------------
   -- Search
   ---------------------------------------------------------------------

   procedure Search (A_List: in  List;
                     A_Key1: in  Key1;
                     A_Key2: in  Key2;
                     Found:  out Boolean) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found then
         if Key1_List /= null then
            Lists_Of_Elements.Search (Key1_List.all, A_Key2, Found);
         else
            Found := False;
         end if;
      end if;
   end Search;

   ---------------------------------------------------------------------
   -- Insert
   ---------------------------------------------------------------------

   procedure Insert (A_List:     in out List;
                     A_Key1:     in     Key1;
                     A_Key2:     in     Key2;
                     An_Element: in     Element;
                     Done:       out    Boolean) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A := null;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Done);
      if not Done then
         -- List for Key1 doesn't exist, insert a null.
         Lists_Of_Lists.Insert (A_List.List_Of_Lists, A_Key1,
                                Key1_List, Done);
      end if;
      if Key1_List = null then
         -- Key1 points to a null access: hang there a
         --  keyed list for new element.
         Key1_List := new Lists_Of_Elements.Keyed_List;
         Lists_Of_Lists.Replace (A_List.List_Of_Lists, A_Key1,
                                 Key1_List, Done);
      end if;
      -- Now Key1_List should point to the proper list, let's insert.
      Lists_Of_Elements.Insert (Key1_List.all, A_Key2,
                                An_Element, Done);
   end Insert;

   ---------------------------------------------------------------------
   -- Remove
   ---------------------------------------------------------------------

   procedure Remove (A_List: in out List;
                     A_Key1: in     Key1;
                     A_Key2: in     Key2;
                     Found:  out    Boolean) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found then
         if Key1_List /= null then
            Lists_Of_Elements.Remove (Key1_List.all, A_Key2, Found);
         else
            Found := False;
         end if;
      end if;
   end Remove;

   ---------------------------------------------------------------------
   -- Remove (returning removed element)
   ---------------------------------------------------------------------

   procedure Remove (A_List:          in out List;
                     A_Key1:          in     Key1;
                     A_Key2:          in     Key2;
                     Found:           out    Boolean;
                     Removed_Element: out    Element) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found then
         if Key1_List /= null then
            Lists_Of_Elements.Remove (Key1_List.all, A_Key2, Found,
                                      Removed_Element);
         else
            Found := False;
         end if;
      end if;
   end Remove;

   ---------------------------------------------------------------------
   -- Replace.
   ---------------------------------------------------------------------

   procedure Replace (A_List:          in out List;
                      A_Key1:          in     Key1;
                      A_Key2:          in     Key2;
                      New_Element:     in     Element;
                      Found:           out    Boolean) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found then
         if Key1_List /= null then
            Lists_Of_Elements.Replace (Key1_List.all, A_Key2,
                                       New_Element, Found);
         else
            Found := False;
         end if;
      end if;
   end Replace;

   ---------------------------------------------------------------------
   -- Get_Element
   ---------------------------------------------------------------------

   procedure Get_Element (A_List:     in  List;
                          A_Key1:     in  Key1;
                          A_Key2:     in  Key2;
                          An_Element: out Element;
                          Found:      out Boolean) is

      use type Lists_Of_Elements.Keyed_List_A;

      Key1_List: Lists_Of_Elements.Keyed_List_A;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found then
         if Key1_List /= null then
            Lists_Of_Elements.Get_Element (Key1_List.all, A_Key2,
                                           An_Element, Found);
         else
            Found := False;
         end if;
      end if;
   end Get_Element;

   ---------------------------------------------------------------------
   -- Sublist_Length
   ---------------------------------------------------------------------

   function Sublist_Length (A_List: in  List;
                            A_Key1: in Key1)
                            return Natural is

      Key1_List: Lists_Of_Elements.Keyed_List_A;
      Found:     Boolean;
   begin
      Lists_Of_Lists.Get_Element (A_List.List_Of_Lists, A_Key1,
                                  Key1_List, Found);
      if Found and then not Lists_Of_Elements.Is_Null (Key1_List) then
         return Lists_Of_Elements.Size_Of (Key1_List.all);
      else
         return 0;
      end if;
   end Sublist_Length;

   ---------------------------------------------------------------------
   -- Image
   ---------------------------------------------------------------------

   function Image (A_List: in List) return String is

   begin
      return Lists_Of_Lists.Image (A_List.List_Of_Lists);
   end Image;


   package body Protect is

      ----------------------------------------------------------------
      -- Reset.
      ----------------------------------------------------------------

      procedure Reset (A_List: in out List) is

      begin
         A_List.Reset;
      end Reset;

      ----------------------------------------------------------------
      -- Search.
      --
      -- XXX: Parameter A_Keyed_List is "in out" instead of "in"
      --      because "prefix of protected procedure or entry call
      --      must be variable" (gnat-3.09 dixit).
      ----------------------------------------------------------------

      ----------------------------------------------------------------

      procedure Search (A_List: in out List;
                        A_Key1: in     Key1;
                        A_Key2: in     Key2;
                        Found:  out    Boolean) is

      begin
         A_List.Search (A_Key1, A_Key2, Found);
      end Search;

      ----------------------------------------------------------------
      -- Insert.
      ----------------------------------------------------------------

      procedure Insert (A_List:     in out List;
                        A_Key1:     in     Key1;
                        A_Key2:     in     Key2;
                        An_Element: in     Element;
                        Done:       out    Boolean) is

      begin
         A_List.Insert (A_Key1, A_Key2, An_Element, Done);
      end Insert;

      ----------------------------------------------------------------
      -- Remove.
      ----------------------------------------------------------------

      procedure Remove (A_List: in out List;
                        A_Key1: in     Key1;
                        A_Key2: in     Key2;
                        Found:  out    Boolean) is

      begin
         A_List.Remove (A_Key1, A_Key2, Found);
      end Remove;

      ----------------------------------------------------------------
      --  Remove.
      ----------------------------------------------------------------

      procedure Remove (A_List:          in out List;
                        A_Key1:          in     Key1;
                        A_Key2:          in     Key2;
                        Found:           out    Boolean;
                        Removed_Element: out    Element) is

      begin
         A_List.Remove (A_Key1, A_Key2, Found, Removed_Element);
      end Remove;

      ----------------------------------------------------------------
      -- Replace.
      ----------------------------------------------------------------

      procedure Replace (A_List:          in out List;
                         A_Key1:          in     Key1;
                         A_Key2:          in     Key2;
                         New_Element:     in     Element;
                         Found:           out    Boolean) is

      begin
         A_List.Replace (A_Key1, A_Key2, New_Element, Found);
      end Replace;

      ----------------------------------------------------------------
      -- Get_Element.
      --
      -- XXX: Parameter A_Keyed_List is "in out" instead of "in"
      --      because "prefix of protected procedure or entry call
      --      must be variable" (gnat-3.09 dixit).
      ----------------------------------------------------------------

      procedure Get_Element (A_List:     in out List;
                             A_Key1:     in     Key1;
                             A_Key2:     in     Key2;
                             An_Element: out    Element;
                             Found:      out    Boolean) is

      begin
         A_List.Get_Element (A_Key1, A_Key2, An_Element, Found);
      end Get_Element;

      ----------------------------------------------------------------
      -- Sublist_Length.
      ----------------------------------------------------------------

      function Sublist_Length (A_List: in List;
                               A_Key1: in Key1)
                               return Natural is

      begin
         return A_List.Sublist_Length (A_Key1);
      end Sublist_Length;

      ----------------------------------------------------------------
      -- Image.
      ----------------------------------------------------------------

      function Image (A_List: in List) return String is

      begin
         return A_List.Image;
      end Image;

      -- Protected type for implementing the keyed lists.
      --
      protected body List is

        procedure Reset is

        begin
        Reset (The_List);
        end Reset;

        procedure Search (A_Key1: in  Key1;
                          A_Key2: in  Key2;
                          Found:  out Boolean) is

        begin
           Search (The_List, A_Key1, A_Key2, Found);
        end Search;

        procedure Insert (A_Key1:     in     Key1;
                          A_Key2:     in     Key2;
                          An_Element: in     Element;
                          Done:       out    Boolean) is

        begin
           Insert (The_List, A_Key1, A_Key2, An_Element, Done);
        end Insert;

        procedure Remove (A_Key1: in     Key1;
                          A_Key2: in     Key2;
                          Found:  out    Boolean) is

        begin
           Remove (The_List, A_Key1, A_Key2, Found);
        end Remove;

        procedure Remove (A_Key1:          in     Key1;
                          A_Key2:          in     Key2;
                          Found:           out    Boolean;
                          Removed_Element: out    Element) is

        begin
           Remove (The_List, A_Key1, A_Key2, Found, Removed_Element);
        end Remove;

        procedure Replace (A_Key1:          in     Key1;
                           A_Key2:          in     Key2;
                           New_Element:     in     Element;
                           Found:           out    Boolean) is

        begin
           Replace (The_List, A_Key1, A_Key2, New_Element, Found);
        end Replace;

        procedure Get_Element (A_Key1:     in  Key1;
                               A_Key2:     in  Key2;
                               An_Element: out Element;
                               Found:      out Boolean) is

        begin
           Get_Element (The_List, A_Key1, A_Key2, An_Element, Found);
        end Get_Element;

        function Sublist_Length (A_Key1: in Key1) return Natural is

        begin
           return Sublist_Length (The_List, A_Key1);
        end Sublist_Length;

        function Image return String is

        begin
           return Image (The_List);
        end Image;

      end List;

   end Protect;

end Keyed_Keyed_Lists_Generic;
