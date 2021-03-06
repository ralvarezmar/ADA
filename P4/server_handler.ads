with Lower_Layer_UDP;
with Hash_Maps_G;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Maps_G;
with Ada.Calendar;
with Ada.Real_Time;

with Ada.Command_Line;
package Server_handler is

package LLU renames Lower_Layer_UDP;
package ASU renames Ada.Strings.Unbounded;
use type Ada.Real_Time.Time;

type Seq_N_T is mod Integer'Last;

 type Mess_ID is record
	EP_D: LLU.End_Point_Type;
	EP_O: LLU.End_Point_Type;
	Num : Seq_N_T;
end record;

type client_info_type is record 
      Client_EP: LLU.End_Point_Type;
      Hora : Ada.Calendar.Time;
      Last_Msg: Seq_N_T := 0;
      Msg_Enviado: Seq_N_T := 0;	--Ultimo numero de secuencia enviado por el server
end record;

type Mensaje is record
	Nick: ASU.Unbounded_String;
	Comentario: ASU.Unbounded_String;
	N_Rts: Integer := 1;	
end record;

Max_Delay: Integer := Integer'Value(Ada.Command_Line.Argument(4));
Plazo_Retransmision: Ada.Real_Time.Time_Span:= Ada.Real_Time.To_Time_Span(2 * Duration(Max_Delay) / 1000);

Max_Rts: Integer := 10 + (Integer'Value(Ada.Command_Line.Argument(5))/10) * (Integer'Value(Ada.Command_Line.Argument(5))/10)/10;

Max: Integer := Integer'Value(Ada.Command_Line.Argument(2));
Maximos_Clientes: constant := 50;

function Comparar(Variable1:Mess_ID;Variable2:Mess_ID) return Boolean;

type Hash_Range is mod Maximos_Clientes;

function Hash_Client(S:ASU.Unbounded_String) return Hash_Range;

type Hash_Range_Message is mod 1000;

function Hash_Message(K:Mess_ID) return Hash_Range_Message;

type Hash_Range_Old is mod 100;

function Hash_Client_Old(S:ASU.Unbounded_String) return Hash_Range_Old;

procedure Handler (From    : in     LLU.End_Point_Type;
                    To      : in     LLU.End_Point_Type;
                    P_Buffer: access LLU.Buffer_Type);


--Lista de mensajes



package Maps_Message is new Hash_Maps_G (Key_Type => Mess_ID,
                               Value_Type => Mensaje,
                               "="        => Comparar, 
				Hash_Range => Hash_Range_Message,
                                 Hash => Hash_Message);

Pending_Msgs:Maps_Message.Map;

--Retransmisiones
package Maps_Rts is new Maps_G (Key_Type   => Ada.Real_Time.Time,
                              	Value_Type => Mess_ID,
                              	"="        => "=", 
				Max => 1000);

Retransmission_Times : Maps_Rts.Map;

--Listas de clientes

package Maps is new Hash_Maps_G (Key_Type   => ASU.Unbounded_String,
                                 Value_Type => client_info_type,
                                 "="        => ASU."=",
				 Hash_Range => Hash_Range,
                                 Hash => Hash_Client);
Clients_Active:Maps.Map;


package Maps_Old is new Hash_Maps_G (Key_Type   => ASU.Unbounded_String,
                               Value_Type => Ada.Calendar.Time,
                               "="        => ASU."=",
				Hash_Range => Hash_Range_Old,
                                 Hash => Hash_Client_Old);


Clients_Olds:Maps_Old.Map;


end Server_handler;
