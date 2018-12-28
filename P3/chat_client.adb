with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with chat_messages;
with Client_handler;

procedure chat_client is
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	
	Server_EP : LLU.End_Point_Type;
	Client_EP_Receive: LLU.End_Point_Type;
	Client_EP_Handler: LLU.End_Point_Type;
	Buffer	: aliased LLU.Buffer_Type(1024);

	Ver_Servidor: ASU.Unbounded_String;
	Port	: Integer;
	Request	: ASU.Unbounded_String;
	Nick	: ASU.Unbounded_String;
	Text	: ASU.Unbounded_String;
	Tipo : chat_messages.Message_Type;


procedure Mandar_Mensajes is
	begin
		LLU.Reset(Buffer);
		ATI.Put(">> ");
		Request := ASU.To_Unbounded_String(ATI.Get_Line);				
		--Mando el mensaje
		if Request /= ".quit" then 
			chat_messages.Message_Type'Output(Buffer'access, chat_messages.Writer);
			LLU.End_Point_Type'Output(Buffer'Access,Client_EP_Handler);				
			ASU.Unbounded_String'Output(Buffer'Access, Nick);
			ASU.Unbounded_String'Output(Buffer'Access, Request); --Guarda el mensaje en el buffer
			LLU.Send(Server_EP, Buffer'Access); --Muestra el contenido del buffer	
		end if;	

end Mandar_Mensajes;

	Expired: Boolean:=False;
begin
	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(Ada.Command_Line.Argument(1)));
	Port		:= Integer'Value(Ada.Command_Line.Argument(2));
	Nick		:= ASU.To_Unbounded_String(Ada.Command_Line.Argument(3));
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);	
	LLU.Bind_Any(Client_EP_Handler,Client_handler.Handler'access);
	LLU.Bind_Any(Client_EP_Receive);
	chat_messages.Message_Type'Output(Buffer'access, chat_messages.Init); --mensaje init
	LLU.End_Point_Type'Output(Buffer'Access,Client_EP_Receive); --cliente handler
	LLU.End_Point_Type'Output(Buffer'Access,Client_EP_Handler); --cliente handler
	ASU.Unbounded_String'Output(Buffer'Access, Nick); --Nombre del cliente		
	LLU.Send(Server_EP,Buffer'Access);
	LLU.Reset(Buffer);
	--Recibir welcome
	LLU.Receive(Client_EP_Receive,Buffer'Access, 5.0, Expired);
	if Expired then --Plazo 
		ATI.Put_Line("Server unreachable");
	else
		Tipo:= chat_messages.Message_Type'Input(Buffer'Access);--Mensaje Welcome
		if Tipo = chat_messages.Welcome then
			if Boolean'Input(Buffer'access)  then
				ATI.Put_Line("Mini-Chat v2.0: Welcome " & ASU.To_String(Nick));
				loop 
					Mandar_Mensajes;
				exit when Request=".quit";
				end loop;
				chat_messages.Message_Type'Output(Buffer'access, chat_messages.Logout);
				LLU.End_Point_Type'Output(Buffer'Access,Client_EP_Handler);
				ASU.Unbounded_String'Output(Buffer'Access, Nick);
				LLU.Send(Server_EP,Buffer'access);
			else 
				ATI.Put_Line("Mini-Chat v2.0: IGNORED new user " & ASU.To_String(Nick) & ", nick already used");
			end if;
		else 
			ATI.Put_Line("Recibido mensaje erroneo");
		end if;
	end if;
	LLU.Finalize;
end chat_client; 
