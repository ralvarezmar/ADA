with Ada.Real_Time;
generic
   type Key_Type is private;
   type Value_Type is private;
   with function "=" (K1, K2: Key_Type) return Boolean;
   
   Max : Natural; 
package Maps_G is

   type Map is limited private;

   procedure Get (M       : Map;
                  Key     : out  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean);


   Full_Map : exception; 
   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type);

   procedure Delete (M      : in out Map;
                   --  Key     : in  Key_Type;
                     Success : out Boolean);


   function Map_Length (M : Map) return Natural;

   --
   -- Cursor Interface for iterating over Map elements
   --
  -- type Cursor is limited private;
   function First (M: Map) return Key_Type;
   function Last (M: Map) return Key_Type;


   type Element_Type is record
      Key:   Key_Type;
      Value: Value_Type;
   end record;
   No_Element: exception;

   -- Raises No_Element if Has_Element(C) = False;

private

   type Cell;
   type Cell_A is access Cell;
   type Cell is record
      Key   : Key_Type;
      Value : Value_Type;
      Next  : Cell_A;
      Prev:  Cell_A;
   end record;

   type Map is record
      P_First : Cell_A;
      P_Last: Cell_A;
      Length  : Natural := 0;
   end record;

 
end Maps_G;
