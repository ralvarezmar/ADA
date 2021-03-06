with Ada.Text_IO;
With Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with maps_g;

procedure prueba_maps_g is
  package ASU  renames Ada.Strings.Unbounded;
  package ATIO renames Ada.Text_IO;

package Maps is new Maps_G (Key_Type   => Natural,
                               Value_Type => Natural,
                               "="        => "=",
				Max => 50);



   procedure Print_Map (M : Maps.Map) is
      C: Natural := Maps.First(M);
   begin
      Ada.Text_IO.Put_Line ("Map");
      Ada.Text_IO.Put_Line ("===");
      Ada.Text_IO.Put_Line(Natural'Image(Maps.Map_Length(M)));
      Ada.Text_IO.Put_Line(Natural'Image(Maps.First(M)));
   end Print_Map;

procedure Do_Put (M: in out Maps.Map; K: Natural; V: Natural) is
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Putting" & Natural'Image(K));
      Maps.Put (M, K, V);
      Print_Map(M);
   exception
      when Maps.Full_Map =>
         Ada.Text_IO.Put_Line("Full_Map");
   end Do_Put;

   procedure Do_Get (M: in out Maps.Map) is
      V: Natural;
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Getting first");
      Maps.Get (M, V, Success);
      if Success then
         Ada.Text_IO.Put_Line("Value:" & Natural'Image(V));
         Print_Map(M);
      else
         Ada.Text_IO.Put_Line("Element not found!");
      end if;
   end Do_Get;

   procedure Do_Delete (M: in out Maps.Map) is
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Deleting first");
      Maps.Delete (M, Success);

   end Do_Delete;


   A_Map : Maps.Map;
begin

   -- First puts
   Do_Put (A_Map, 10, 10);
   
   Do_Delete (A_Map);
   
   Do_Get (A_Map);
   Do_Put (A_Map, 20, 20);
   Do_Put (A_Map, 11, 11);

   Do_Put (A_Map, 30, 30);
   Do_Put (A_Map, 21, 21);
   Do_Put (A_Map, 15, 15);
   Do_Put (A_Map, 16, 16);
   
   Do_Get (A_Map);
   Do_Put (A_Map, 40, 40);
   Do_Put (A_Map, 25, 25);
   Do_Put (A_Map, 50, 50);

   -- Now deletes
   Do_Delete (A_Map);
   Do_Delete (A_Map);

   -- Now gets
   Do_Get (A_Map);

end prueba_maps_g;
