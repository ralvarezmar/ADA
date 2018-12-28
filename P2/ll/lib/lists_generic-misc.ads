-- $Id: lists_generic-misc.ads,v 1.5 1998/01/19 12:46:24 jgb Exp $
--

generic
   with function ">=" (Left, Right: in Element) return Boolean is <>;
   with function "=" (Left, Right: in Element) return Boolean is <>;

package Lists_Generic.Misc is

   pragma Elaborate_Body;

   type Element_Array is array (Natural range <>) of Element;

   -- Current length fo a list (number of elements)
   --
   function Length (A_List: in List) return Natural;

   -- Get all the elements currently in a list.
   --
   function Get_Elements (A_List: in List) return Element_Array;

   function Make_List (Elements: in Element_Array) return List;

   function Is_In (A_List:     in List;
                   An_Element: in Element) return Boolean;

   procedure Insert_Ordered (A_List:     in out List;
                             An_Element: in     Element);

   procedure Remove (A_List:     in out List;
                     An_Element: in Element);

   procedure Remove_All (A_List: in out List);

   -- Merge two lists (adding at the tail)
   --
   procedure Merge_Tail (A_List:   in out List;
                         To_Merge: in List;
                         Equal:    out Boolean);

   -- Merge two lists (ordered)
   --
   procedure Merge_Ordered (A_List:   in out List;
                            To_Merge: in     List;
                            Equal:    out    Boolean);

   -- Merge two lists (ordered)
   --
   procedure Merge_Ordered (A_List:   in out List;
                            To_Merge: in     List;
                            Added:    out    List;
                            Equal:    out    Boolean);

   -- Get a list (In_List) with the elements of To_Check which
   --  are in The_List.
   --
   procedure Get_In_List (In_List:  in out List;
                          The_List: in     List;
                          To_Check: in     List);

end Lists_Generic.Misc;
