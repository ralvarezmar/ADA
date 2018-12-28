--
--  TAD genérico de una tabla de símbolos (map) implementada como un hash
--

generic
   type Key_Type is private;
   type Value_Type is private;
   with function "=" (K1, K2: Key_Type) return Boolean;
   type Hash_Range is mod <>;
   with function Hash (K: Key_Type) return Hash_Range;
package Hash_Maps_G is

   type Map is limited private;

   Full_Map : exception;

   procedure Get (M       : in out Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean);


   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type);

   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean);


   function Map_Length (M : Map) return Natural;

   --
   -- Cursor Interface for iterating over Map elements
   --
   type Cursor is limited private;
   function First (M: Map) return Cursor;
   function Last (M: Map) return Cursor;
   procedure Next (C: in out Cursor);
   procedure Prev (C: in out Cursor);
   function Has_Element (C: Cursor) return Boolean;
   type Element_Type is record
      Key:   Key_Type;
      Value: Value_Type;
   end record;
   No_Element: exception;

   -- Raises No_Element if Has_Element(C) = False;
   function Element (C: Cursor) return Element_Type;

private
  type Content is (Full, Empty, Delete);

  type Cell is record
      Key   : Key_Type;
      Value : Value_Type;
      Contenido: Content := Empty;
   end record;

   type Map is array(Hash_Range) of Cell;


   type Cursor is record
      M         : Map;
      Element : Integer;
   end record;

end Hash_Maps_G;
