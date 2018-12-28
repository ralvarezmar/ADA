with Lower_Layer_UDP;
with Maps_G;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

package Server_handler is

package LLU renames Lower_Layer_UDP;
use type Ada.Calendar.Time;
package ASU renames Ada.Strings.Unbounded;

type Value is record
      Client_EP: LLU.End_Point_Type;
      Hora : Ada.Calendar.Time;
   end record;

Maximos_Clientes: Natural := Integer'Value(Ada.Command_Line.Argument(2));

package Maps is new Maps_G (Key_Type   => ASU.Unbounded_String,
                               Value_Type => Value,
                               "="        => ASU."=",
				Max => Maximos_Clientes);

package Maps_Old is new Maps_G (Key_Type   => ASU.Unbounded_String,
                               Value_Type => Ada.Calendar.Time,
                               "="        => ASU."=",
				Max => 50);

Clients_Active:Maps.Map;
Clients_Olds:Maps_Old.Map;


	procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type);

	

end Server_handler;
