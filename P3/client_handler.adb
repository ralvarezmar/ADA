with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Client_Handler;
with chat_messages;

package body Client_handler is

  
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;

procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is
     	

	Nick	: ASU.Unbounded_String;
	Text	: ASU.Unbounded_String;
	Tipo: chat_messages.Message_Type;

   begin
		Tipo:= chat_messages.Message_Type'Input(P_Buffer);
		if Tipo=chat_messages.Server then
			Nick:= ASU.Unbounded_String'Input(P_Buffer);
			Text := ASU.Unbounded_String'Input(P_Buffer);			
			ATI.New_Line;	
			ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Text));	
			ATI.Put(">> ");				
		end if;
 
end Handler; 
end Client_Handler;
