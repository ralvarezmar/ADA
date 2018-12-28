with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with chat_messages;
with client_collections;

procedure chat_client is
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	
	Server_EP : LLU.End_Point_Type;
	Client_EP: LLU.End_Point_Type;
	Buffer	: aliased LLU.Buffer_Type(1024);

	Ver_Servidor: ASU.Unbounded_String;
	Port	: Integer;
	Request	: ASU.Unbounded_String;
	Nick	: ASU.Unbounded_String;
	Text	: ASU.Unbounded_String;
	Servidor: chat_messages.Message_Type;
	Collection_Writer:client_collections.Collection_Type;
	Collection_Reader:client_collections.Collection_Type;
begin
	
	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(Ada.Command_Line.Argument(1)));
	Port		:= Integer'Value(Ada.Command_Line.Argument(2));
	Nick		:= ASU.To_Unbounded_String(Ada.Command_Line.Argument(3));
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);	
	LLU.Bind_Any(Client_EP);
	chat_messages.Message_Type'Output(Buffer'access, chat_messages.Init); --mensaje init
	LLU.End_Point_Type'Output(Buffer'Access,Client_EP); --cliente
	ASU.Unbounded_String'Output(Buffer'Access, Nick); --Nombre del cliente	
	
	LLU.Send(Server_EP,Buffer'Access);
	LLU.Reset(Buffer);
	if Ada.Command_Line.Argument(3)="reader" then  --lector 
		loop
				LLU.Reset(Buffer);
				LLU.Receive(Client_EP,Buffer'Access);	--Recibe el mensaje
				Servidor:= chat_messages.Message_Type'Input(Buffer'Access);
				Nick:= ASU.Unbounded_String'Input(Buffer'Access);
				Text := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Text));
				
		end loop;
	else				--escritor
		loop 
			chat_messages.Message_Type'Output(Buffer'access, chat_messages.Writer);
			ATI.Put(ASU.To_String(Nick) & ": ");
			LLU.End_Point_Type'Output(Buffer'Access,Client_EP);
			Request := ASU.To_Unbounded_String(ATI.Get_Line);
			ASU.Unbounded_String'Output(Buffer'Access, Request); --Guarda el mensaje en el buffer
			LLU.Send(Server_EP, Buffer'Access); --Muestra el contenido del buffer		
			LLU.Reset(Buffer);	
		exit when Request=".quit";		
		end loop;
	end if; 
	LLU.Finalize;

end chat_client; 
