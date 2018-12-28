-- $Id: protected_stores_generic.adb,v 1.5 1998/01/19 12:46:26 jgb Exp $
--

with Debugging;

package body Protected_Stores_Generic is

   ----------------------------------------------------------
   -- Debugging.
   ----------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Protected_Stores_Generic";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   -------------------------------------------------------------
   -- Protected type Store.
   -------------------------------------------------------------

   protected body Store is


     -----------------------------------------------------------
     -- Put.
     --  Raises exception if storage is full.
     --
     -- XXX: Maybe other politics could be inforced when
     --      buffer is full.
     -----------------------------------------------------------

     procedure Put (An_Element: in Element) is

        begin
           if Store_Lists.Length (Storage) < Size then
              Store_Lists.Insert_Head (Storage, An_Element);
              Put_Line ("Size = " &
                        Natural'Image (Store_Lists.Length (Storage)),
                        " (Put finishing)");
           else
              Put_Line ("Size = " &
                        Natural'Image (Store_Lists.Length (Storage)),
                        " (Put raised Store_Full)");
              raise Store_Full;
           end if;
        end Put;


     -----------------------------------------------------------
     -- Get.
     -----------------------------------------------------------

     entry Get (An_Element: out Element)
       when Store_Lists.Length (Storage) > 0 is

           Node_Ref: Store_Lists.Reference :=
             Store_Lists.Last (Storage);
        begin
           An_Element := Store_Lists.Get_Element (Storage, Node_Ref);
           Store_Lists.Remove (Storage, Node_Ref);
           Put_Line ("Size = " &
                     Natural'Image (Store_Lists.Length (Storage)),
                     " (Get finishing)");
        end Get;


     -----------------------------------------------------------
     -- Image.
     -----------------------------------------------------------

     function Image return String is

     begin
        return Store_Lists.Image (Storage);
     end Image;

   end Store;

end Protected_Stores_Generic;
