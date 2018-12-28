with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with chat_messages;
with Ada.Calendar;
with Ada.Real_Time;
with Hash_Maps_G;
with Maps_G;

package Client_handler is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ATI renames Ada.Text_IO;
   
   use type Ada.Real_Time.Time;
   type Seq_N_T is mod Integer'Last;


type Mess_ID is record --Mess_ID
	EP_D: LLU.End_Point_Type;
	EP_O: LLU.End_Point_Type;
	Num : Seq_N_T := 0;
end record;

type Mensaje is record
	Tipo: chat_messages.Message_Type;
	Nick: ASU.Unbounded_String;
	Comentario: ASU.Unbounded_String;	
	N_Rts: Integer := 1;
end record;

Max_Rts: Integer := 10 + ((Integer'Value(Ada.Command_Line.Argument(5))/100)*(Integer'Value(Ada.Command_Line.Argument(5)))/100)/10;
Max_Delay:Integer;
Plazo_Retransmision: Ada.Real_Time.Time_Span :=Ada.Real_Time.To_Time_Span(2 * Duration(Max_Delay) / 1000);
Last_Sec: Seq_N_T:=0;
type Hash_Range is mod 100;

function Comparar(Variable1:Mess_ID;Variable2:Mess_ID) return Boolean;

function Hash_EP(K:Mess_ID) return Hash_Range;

procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type);



package Maps_Message is new Hash_Maps_G (Key_Type   => Mess_ID,
                               Value_Type => Mensaje,
                                "="        => Comparar,
				Hash_Range => Hash_Range,
                                 Hash => Hash_EP);

Pending_Msgs: Maps_Message.Map;

package Maps_Rts is new Maps_G (Key_Type   => Ada.Real_Time.Time,
                              	Value_Type => Mess_ID,
                              	"="        => "=",
				Max => 100);

Retransmission_Times : Maps_Rts.Map;


end Client_handler;
