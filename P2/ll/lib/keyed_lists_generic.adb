-- $Id: keyed_lists_generic.adb,v 1.14 1998/01/19 12:46:22 jgb Exp $
--

-- For implementing the keyed list, we will use lists with
--  a dummy node. Procedure Initialize will insert the dummy
--  node in the lists, and the other subprograms will have
--  it into account. The list will be maintained ordered by
--  keys, lesser first.
--
-- This implementation is heavily based on the ADT provided by
--  M. Feldman in his book `Ada 95 Software Construction and Data
--  Structures'.
--

with Misc_Util_Accesses;
with Debugging;

package body Keyed_Lists_Generic is

   ----------------------------------------------------------
   -- Debugging.
   ----------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Keyed_Lists_Generic";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   -----------------------------------------------------
   -- Is_Null (Keyed_List_A).
   -----------------------------------------------------

   function Is_Null (A_Keyed_List_A: in Keyed_List_A) return Boolean is

      function Is_Null_A is new Misc_Util_Accesses.Is_Null_A
        (Object => Keyed_List, Name => Keyed_List_A);
   begin
      return Is_Null_A (A_Keyed_List_A);
   end Is_Null;


   -----------------------------------------------------
   -- Image (Keyed_List_A).
   -----------------------------------------------------

   function Image (A_Keyed_List_A: in Keyed_List_A) return String is

      function Image_A is new Misc_Util_Accesses.Image_A
        (Object       => Keyed_List,
         Name         => Keyed_List_A,
         Image_Object => Image);
   begin
      return (Image_A (A_Keyed_List_A));
   end Image;

   -----------------------------------------------------
   -- Locate (internal use only).
   --  Locates a node given its key, returning a both a reference
   --  to it and to its previous node. More exactly, Previous will
   --  point to the last node with key < A_Key, and Current
   --  will point to the first node with key >= A_Key.
   --  If list is empty, both Current and Previous will point
   --  to dummy node.
   --  If fails to locate the node, Found is returned as False.
   -----------------------------------------------------

   procedure Locate (A_Keyed_List: in  Keyed_List;
                     A_Key:        in  Key;
                     Previous:     out Lists.Reference;
                     Current:      out Lists.Reference;
                     Found:        out Boolean) is

      -- Temp references.
      --
      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
      -- Have we found something bigger or equal than the key? (not yet).
      --  Warning, this is not like Found, which means we have
      --  found something *equal*.
      --
      Tmp_Found:        Boolean := False;
      -- The list element we are dealing with in each iteration.
      Tmp_List_Element: List_Element;

   begin
      if Lists.Is_Empty (A_Keyed_List.Data) then
         -- No dummy node, list uninitialized. Shouldnt's happen,
         --  locate is an internal subprogram.
         raise Uninitialized_List;
      end if;
      -- Let's begin by pointing Tmp_Current to dummy node.
      Tmp_Current := Lists.First (A_Keyed_List.Data);
      Tmp_Previous := Tmp_Current;
      if Lists.Is_Last (A_Keyed_List.Data, Tmp_Current) then
         -- Only dummy node, keyed list is empty.
         Found    := False;
      else
         -- Search for the first node with key >= A_Key.
         Lists.Forward (A_Keyed_List.Data, Tmp_Current);
         while (not Tmp_Found) and then
           (not Lists.Is_Out (A_Keyed_List.Data, Tmp_Current)) loop
            Tmp_List_Element := Lists.Get_Element (A_Keyed_List.Data,
                                                   Tmp_Current);
            if Tmp_List_Element.The_Key >= A_Key then
               Tmp_Found := True;
            else
               Tmp_Previous := Tmp_Current;
               Lists.Forward (A_Keyed_List.Data, Tmp_Current);
            end if;
         end loop;
         -- Found only if something >= was found, and that's
         --  exactly equal.
         Found := (Tmp_Found and then (Tmp_List_Element.The_Key = A_Key));
      end if;
      Previous := Tmp_Previous;
      Current  := Tmp_Current;
   end Locate;


   -----------------------------------------------------
   -- Initialize.
   --  Creates the dummy node, and intitializes the underlying
   --  List, just in case...
   -----------------------------------------------------

   procedure Initialize (A_Keyed_List: in out Keyed_List) is

      -- Contents of dummy node are irrelevant.
      Dummy: List_Element;

      -- Dummy is uninitialized, but I kow it, causes no harm,
      --  and I don't like the warning message to be printed out.
      --
      pragma Warnings (Off, Dummy);

   begin
      Lists.Insert_Tail (A_Keyed_List.Data, Dummy);
      A_Keyed_List.Current_Size := 0;
   exception
      when Lists.Storage_Exhausted =>
         Put_Line ("Lists.Storage_Exhausted raised", " (Initialize)");
         raise Storage_Exhausted;
   end Initialize;


   -----------------------------------------------------
   -- Finalize.
   --  Everithing needed is done by finalization of
   --  A_Keyed_List.Data.
   -----------------------------------------------------

   procedure Finalize (A_Keyed_List: in out Keyed_List) is

   begin
      null;
   end Finalize;


   -----------------------------------------------------
   -- Reset.
   -----------------------------------------------------

   procedure Reset (A_Keyed_List: in out Keyed_List) is

   begin
      Lists.Reset (A_Keyed_List.Data);
      Initialize (A_Keyed_List);
   end Reset;


   -----------------------------------------------------
   -- Search.
   --  May raise Uninitialized_List (because of invocation
   --  of Locate).
   -----------------------------------------------------

   procedure Search (A_Keyed_List: in  Keyed_List;
                     A_Key:        in  Key;
                     Found:        out Boolean) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
   end Search;


   -----------------------------------------------------
   -- Insert.
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Insert (A_Keyed_List: in out Keyed_List;
                     A_Key:        in     Key;
                     An_Element:   in     Element;
                     Done:         out    Boolean) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
      Found:        Boolean;
   begin
      Put_Line ("A_Keyed_List: " & Image (A_Keyed_List),
                " (Insert)");
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if not Found then
         if A_Keyed_List.Current_Size >= Capacity then
            -- List full
            Put_Line ("A_Keyed_List.Current_Size (" &
                      Natural'Image (A_Keyed_List.Current_Size) &
                      ") >= Capacity (" &
                      Natural'Image (Capacity) & ")",
                      " (Insert)");
            raise Storage_Exhausted;
         else
            Lists.Insert (A_Keyed_List.Data,
                          (The_Key => A_Key, The_Element => An_Element),
                          Tmp_Previous);
            A_Keyed_List.Current_Size := A_Keyed_List.Current_Size + 1;
            Done := True;
         end if;
      else
         Done := False;
      end if;
      Put_Line ("A_Keyed_List: " & Image (A_Keyed_List),
                " (Insert)");
   exception
      when Lists.Storage_Exhausted =>
         Put_Line ("Lists.Storage_Exhausted raised", " (Insert)");
         raise Storage_Exhausted;
   end Insert;


   -----------------------------------------------------
   -- Remove.
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Remove (A_Keyed_List: in out Keyed_List;
                     A_Key:        in     Key;
                     Found:        out    Boolean) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if Found then
         Lists.Remove (A_Keyed_List.Data, Tmp_Current);
         A_Keyed_List.Current_Size := A_Keyed_List.Current_Size - 1;
      end if;
   end Remove;

   -----------------------------------------------------
   -- Remove (returning removed Element).
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Remove (A_Keyed_List:    in out Keyed_List;
                     A_Key:           in     Key;
                     Found:           out    Boolean;
                     Removed_Element: out    Element) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
      Tmp_List_Element: List_Element;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if Found then
         Tmp_List_Element :=
           Lists.Get_Element (A_Keyed_List.Data, Tmp_Current);
         Lists.Remove (A_Keyed_List.Data, Tmp_Current);
         A_Keyed_List.Current_Size := A_Keyed_List.Current_Size - 1;
         Removed_Element := Tmp_List_Element.The_Element;
      end if;
   end Remove;

   -----------------------------------------------------
   -- Remove (returning both removed Element and Key).
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Remove (A_Keyed_List:    in out Keyed_List;
                     A_Key:           in     Key;
                     Found:           out    Boolean;
                     Removed_Element: out    Element;
                     Removed_Key:     out    Key) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
      Tmp_List_Element: List_Element;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if Found then
         Tmp_List_Element :=
           Lists.Get_Element (A_Keyed_List.Data, Tmp_Current);
         Lists.Remove (A_Keyed_List.Data, Tmp_Current);
         A_Keyed_List.Current_Size := A_Keyed_List.Current_Size - 1;
         Removed_Element := Tmp_List_Element.The_Element;
         Removed_Key := Tmp_List_Element.The_Key;
      end if;
   end Remove;

   -----------------------------------------------------
   -- Replace.
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Replace (A_Keyed_List: in out Keyed_List;
                      A_Key:        in     Key;
                      New_Element:  in     Element;
                      Found:        out    Boolean) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if Found then
         Lists.Replace (A_Keyed_List.Data,
                        (The_Key => A_Key, The_Element => New_Element),
                        Tmp_Current);
      end if;
   end Replace;


   -----------------------------------------------------
   -- Get_Element.
   --  May raise Uninitialized_List (because of invocation of Locate).
   -----------------------------------------------------

   procedure Get_Element (A_Keyed_List: in  Keyed_List;
                          A_Key:        in  Key;
                          An_Element:   out Element;
                          Found:        out Boolean) is

      Tmp_Previous: Lists.Reference;
      Tmp_Current:  Lists.Reference;
      Tmp_List_Element: List_Element;
   begin
      Locate (A_Keyed_List, A_Key, Tmp_Previous, Tmp_Current, Found);
      if Found then
         Tmp_List_Element := Lists.Get_Element (A_Keyed_List.Data,
                                                Tmp_Current);
         An_Element := Tmp_List_Element.The_Element;
      end if;
   end Get_Element;

   -----------------------------------------------------
   -- Get_Head.
   -----------------------------------------------------

   procedure Get_Head (A_Keyed_List: in  Keyed_List;
                       A_Key:        out Key;
                       An_Element:   out Element;
                       Found:        out Boolean) is

      First_Reference: Lists.Reference;
      First_Element:   List_Element;
   begin
      if Size_Of (A_Keyed_List) < 1 then
         Found := False;
      else
         -- First element is dummy node, so skip it.
         First_Reference := Lists.First (A_Keyed_List.Data);
         Lists.Forward (A_Keyed_List.Data, First_Reference);
         First_Element := Lists.Get_Element (A_Keyed_List.Data,
                                             First_Reference);
         An_Element := First_Element.The_Element;
         A_Key := First_Element.The_Key;
         Found := True;
      end if;
   end Get_Head;

   -----------------------------------------------------
   -- Get_Elements.
   --  May raise Uninitialized_List, Bad_List.
   -----------------------------------------------------

   function Get_Elements (A_Keyed_List: in Keyed_List)
                          return Elements_Array is

      Current:            Lists.Reference;
      Length:             Natural := Size_Of (A_Keyed_List);
      A_List_Element:     List_Element;
      The_Elements_Array: Elements_Array (1 .. Length);
   begin
      -- Let's begin by pointing Tmp_Current to dummy node.
      Current := Lists.First (A_Keyed_List.Data);
      -- Now let's loop to fill the array.
      for Count in 1 .. Length loop
         Lists.Forward (A_Keyed_List.Data, Current);
         if Lists.Is_Out (A_Keyed_List.Data, Current) then
            -- Lenght of list isn't correct (this should be
            --  part of the list).
            raise Bad_List;
         else
            -- Let's store the element in the array, and let's
            --  go on.
            A_List_Element :=
              Lists.Get_Element (A_Keyed_List.Data, Current);
            The_Elements_Array (Count) := A_List_Element.The_Element;
         end if;
      end loop;
      if not Lists.Is_Last (A_Keyed_List.Data, Current) then
         -- Lenght of list isn't correct (this should be
         --  the last node of the list).
         raise Bad_List;
      end if;
      return The_Elements_Array;
   end Get_Elements;


   -----------------------------------------------------
   -- Get_Keys.
   --  May raise Uninitialized_List, Bad_List.
   -----------------------------------------------------

   function Get_Keys (A_Keyed_List: in Keyed_List)
                          return Keys_Array is

      Current:            Lists.Reference;
      Length:             Natural := Size_Of (A_Keyed_List);
      A_List_Element:     List_Element;
      The_Elements_Array: Keys_Array (1 .. Length);
   begin
      -- Let's begin by pointing Tmp_Current to dummy node.
      Current := Lists.First (A_Keyed_List.Data);
      for Count in 1 .. Length loop
         Lists.Forward (A_Keyed_List.Data, Current);
         if Lists.Is_Out (A_Keyed_List.Data, Current) then
            -- Lenght of list isn't correct (this should be
            --  part of the list).
            raise Bad_List;
         else
            -- Let's store the element in the array, and let's
            --  go on.
            A_List_Element :=
              Lists.Get_Element (A_Keyed_List.Data, Current);
            The_Elements_Array (Count) := A_List_Element.The_Key;
         end if;
      end loop;
      return The_Elements_Array;
   end Get_Keys;


   -----------------------------------------------------
   -- Size_Of.
   -----------------------------------------------------

   function Size_Of (A_Keyed_List: in Keyed_List) return Natural is

   begin
      if Lists.Is_Empty (A_Keyed_List.Data) then
         -- No dummy node, list uninitialized. Shouldnt's happen,
         --  locate is an internal subprogram.
         raise Uninitialized_List;
      end if;
      return A_Keyed_List.Current_Size;
   end Size_Of;


   -----------------------------------------------------
   -- Image (for Keyed_List).
   -----------------------------------------------------

   function Image (A_Keyed_List: in Keyed_List) return String is

      Current: Lists.Reference := Lists.First (A_Keyed_List.Data);
   begin
      if Lists.Is_Empty (A_Keyed_List.Data) then
         -- No dummy node, list uninitialized. Shouldnt's happen,
         --  locate is an internal subprogram.
         return "(Uninitialized List)";
      else
         Lists.Forward (A_Keyed_List.Data, Current);
         return ("(Size: " & Size'Image (A_Keyed_List.Current_Size) &
                 ", Contents: " &
                 Lists.Image (A_Keyed_List.Data, Current) &
                 ")");
      end if;
   end Image;


   -----------------------------------------------------
   -- Image (for List_Element).
   -----------------------------------------------------

   function Image (A_List_Element: in List_Element) return String is

   begin
      return "(Key: " & Image_Key (A_List_Element.The_Key) &
         ", Element: " & Image_Element (A_List_Element.The_Element) &
         ")";
   end Image;


   package body Protect is

      ----------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------

      procedure Initialize (A_Keyed_List: in out Keyed_List) is

      begin
      A_Keyed_List.Initialize;
      end Initialize;

      ----------------------------------------------------------------
      -- Search
      --
      -- XXX: Parameter A_Keyed_List is "in out" instead of "in"
      --      because "prefix of protected procedure or entry call
      --      must be variable" (gnat-3.05 dixit).
      ----------------------------------------------------------------

      procedure Search (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        Found:        out    Boolean) is

      begin
         A_Keyed_List.Search (A_Key, Found);
      end Search;

      ----------------------------------------------------------------
      -- Insert
      ----------------------------------------------------------------

      procedure Insert (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        An_Element:   in     Element;
                        Done:         out    Boolean) is

      begin
         A_Keyed_List.Insert (A_Key, An_Element, Done);
      end Insert;

      ----------------------------------------------------------------
      -- Remove
      ----------------------------------------------------------------

      procedure Remove (A_Keyed_List: in out Keyed_List;
                        A_Key:        in     Key;
                        Found:        out    Boolean) is

      begin
         A_Keyed_List.Remove (A_Key, Found);
      end Remove;

      ----------------------------------------------------------------
      -- Remove
      ----------------------------------------------------------------

      procedure Remove (A_Keyed_List:    in out Keyed_List;
                        A_Key:           in     Key;
                        Found:           out    Boolean;
                        Removed_Element: out    Element;
                        Removed_Key:     out    Key) is

      begin
         A_Keyed_List.Remove (A_Key, Found, Removed_Element, Removed_Key);
      end Remove;

      ----------------------------------------------------------------
      -- Replace
      ----------------------------------------------------------------

      procedure Replace (A_Keyed_List: in out Keyed_List;
                         A_Key:        in     Key;
                         New_Element:  in     Element;
                         Found:        out    Boolean) is

      begin
         A_Keyed_List.Replace (A_Key, New_Element, Found);
      end Replace;

      ----------------------------------------------------------------
      -- Get_Element
      ----------------------------------------------------------------

      procedure Get_Element (A_Keyed_List: in out Keyed_List;
                             A_Key:        in     Key;
                             An_Element:   out    Element;
                             Found:        out    Boolean) is

      begin
         A_Keyed_List.Get_Element (A_Key, An_Element, Found);
      end Get_Element;

      ----------------------------------------------------------------
      -- Get_Elements
      ----------------------------------------------------------------

      function Get_Elements (A_Keyed_List: in Keyed_List)
                             return Elements_Array is

      begin
         return A_Keyed_List.Get_Elements;
      end Get_Elements;

      ----------------------------------------------------------------
      -- Get_Keys
      ----------------------------------------------------------------

      function Get_Keys (A_Keyed_List: in Keyed_List)
                         return Keys_Array is

      begin
         return A_Keyed_List.Get_Keys;
      end Get_Keys;

      ----------------------------------------------------------------
      -- Size_Of
      ----------------------------------------------------------------

      function Size_Of (A_Keyed_List: in Keyed_List) return Natural is

      begin
         return A_Keyed_List.Size_Of;
      end Size_Of;

      ----------------------------------------------------------------
      -- Image
      ----------------------------------------------------------------

      function Image (A_Keyed_List: in Keyed_List) return String is

      begin
         return A_Keyed_List.Image;
      end Image;


      protected body Keyed_List is
        procedure Initialize is

        begin
        Initialize (The_List);
        end Initialize;

        procedure Search (A_Key:        in  Key;
                          Found:        out Boolean) is

        begin
           Search (The_List, A_Key, Found);
        end Search;

        procedure Insert (A_Key:        in     Key;
                          An_Element:   in     Element;
                          Done:         out    Boolean) is

        begin
           Insert (The_List, A_Key, An_Element, Done);
        end Insert;

        procedure Remove (A_Key:        in     Key;
                          Found:        out    Boolean) is

        begin
           Remove (The_List, A_Key, Found);
        end Remove;

        procedure Remove (A_Key:           in     Key;
                          Found:           out    Boolean;
                          Removed_Element: out    Element;
                          Removed_Key:     out    Key) is

        begin
           Remove (The_List, A_Key, Found, Removed_Element, Removed_Key);
        end Remove;

        procedure Replace (A_Key:        in     Key;
                           New_Element:  in     Element;
                           Found:        out    Boolean) is

        begin
           Replace (The_List, A_Key, New_Element, Found);
        end Replace;

        procedure Get_Element (A_Key:        in     Key;
                               An_Element:   out    Element;
                               Found:        out    Boolean) is

        begin
           Get_Element (The_List, A_Key, An_Element, Found);
        end Get_Element;

        function Get_Elements return Elements_Array is

        begin
           return Get_Elements (The_List);
        end Get_Elements;

        function Get_Keys return Keys_Array is

        begin
           return Get_Keys (The_List);
        end Get_Keys;

        function Size_Of return Natural is

        begin
           return Size_Of (The_List);
        end Size_Of;

        function Image return String is

        begin
           return Image (The_List);
        end Image;

      end Keyed_List;

   end Protect;

end Keyed_Lists_Generic;
