with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with chat_messages;
with client_collections;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Handler;

procedure chat_server is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	Collection_Writer:client_collections.Collection_Type;
	Collection_Reader:client_collections.Collection_Type;
	Server_EP : LLU.End_Point_Type;
	Client_EP : LLU.End_Point_Type;
	Buffer	: aliased LLU.Buffer_Type(1024);
	Ver_Servidor: ASU.Unbounded_String;
	Port	:  Integer;
	Comentario : ASU.Unbounded_String;
	Nick	: ASU.Unbounded_String;
	Client	:chat_messages.Message_Type;
begin
	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(LLU.Get_Host_Name)); 
	Port		:= Integer'Value(Ada.Command_Line.Argument(1));
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);
	LLU.Bind (Server_EP);
	loop
		LLU.Reset(Buffer);
		LLU.Receive(Server_EP,Buffer'Access);
		Client	:= chat_messages.Message_Type'Input(Buffer'access);
		Client_EP	:= LLU.End_Point_Type'Input(Buffer'Access);
		if Client=chat_messages.Init then  --Mensaje INIT 
			Nick:=ASU.Unbounded_String'Input(Buffer'Access);
			if Nick /= "reader" then 	--escritor		
				begin				
					client_collections.Add_Client(Collection_Writer, Client_EP,Nick,True);
					ATI.Put_Line("INIT received from " & ASU.To_String(Nick));
					LLU.Reset(Buffer);
					chat_messages.Message_Type'Output(Buffer'access, chat_messages.Server); --mandar a lectores el init
					ASU.Unbounded_String'Output(Buffer'Access, ASU.To_Unbounded_String("Server"));
					ASU.Unbounded_String'Output(Buffer'Access, (Nick & " joins the chat"));
					--ATI.Put_Line("Antes de enviar");
					client_collections.Send_To_All(Collection_Reader,Buffer'Access);
					
					exception						
						when client_collections.Client_Collection_Error=>
							ATI.Put_Line("INIT received from " & ASU.To_String(Nick) & ". IGNORED, nick already used"); 
				end;
			else -- añadir lector
				client_collections.Add_Client(Collection_Reader, Client_EP,Nick,False);				
			end if;
			--ATI.Put_Line(client_collections.Collection_Image(Collection_Reader));
			--ATI.Put_Line(client_collections.Collection_Image(Collection_Writer));
		elsif Client=chat_messages.Writer then --mensaje WRITER
			begin			
				Nick:= client_collections.Search_Client(Collection_Writer,Client_EP);			
				ATI.Put("WRITER received from " & ASU.To_String(Nick) & " : ");	
				Comentario := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put_Line(ASU.To_String(Comentario));
				LLU.Reset(Buffer);
				chat_messages.Message_Type'Output(Buffer'access, chat_messages.Server); --mandar a lectores el mensaje
				ASU.Unbounded_String'Output(Buffer'Access, Nick);
				ASU.Unbounded_String'Output(Buffer'Access, Comentario);
				--ATI.Put_Line("Antes de enviar");
				client_collections.Send_To_All(Collection_Reader,Buffer'Access);
				--ATI.Put_Line("Despues de enviar");				
				exception						
						when client_collections.Client_Collection_Error=>
							ATI.Put("WRITER received from unknow client. IGNORED");
			end;			
		end if;
	end loop;
	exception
		when ex: others =>
			ATI.Put_Line("Fallo");
end chat_server;


