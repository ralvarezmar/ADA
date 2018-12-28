with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Server_handler;
with chat_messages;
with maps_g;
with Ada.Command_Line;

package body Server_Handler is	
  	
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	
procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is

Success : Boolean;

procedure Send_To_All(Map: in Maps.Map;
			 Client_EP_Handler: in LLU.End_Point_Type;
			P_Buffer: access LLU.Buffer_Type)  is

Cursor: Maps.Cursor:= Maps.First(Clients_Active);

	begin 
		while Maps.Has_Element(Cursor) loop 
			if Maps.Element(Cursor).Value.Client_EP /= Client_EP_Handler then				
				LLU.Send(Maps.Element(Cursor).Value.Client_EP,P_Buffer);
			end if;
		Maps.Next(Cursor);
		end loop;
end Send_To_All;


procedure Expulsar(P_Buffer: access LLU.Buffer_Type) is 
Cursor: Maps.Cursor:= Maps.First(Server_Handler.Clients_Active);
Hora: Ada.Calendar.Time;
Aux : Maps.Element_Type;
begin
	Hora:= Server_Handler.Maps.Element(Cursor).Value.Hora;
	while Maps.Has_Element(Cursor) loop
		if Hora>=Server_Handler.Maps.Element(Cursor).Value.Hora then
			Aux.Key:=Server_Handler.Maps.Element(Cursor).Key;
			Aux.Value:=Server_Handler.Maps.Element(Cursor).Value;
		end if;			 
		Maps.Next(Cursor);
	end loop;
	ATI.New_Line;
	ATI.Put_Line("Expulsado " & ASU.To_String(Aux.Key)); 
	--mandar mensaje de baneo
	LLU.Reset(P_Buffer.all);
	chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); 
	ASU.Unbounded_String'Output(P_Buffer, Aux.Key);
	ASU.Unbounded_String'Output(P_Buffer, ASU.To_Unbounded_String(" banned for being idle too long"));
	LLU.Send(Aux.Value.Client_EP,P_Buffer); --Envio al cliente baneado
	Send_To_All(Clients_Active, Aux.Value.Client_EP,P_Buffer);	
	Maps.Delete(Server_Handler.Clients_Active, Aux.Key, Success);
	Maps_Old.Put(Server_Handler.Clients_Olds, Aux.Key, Aux.Value.Hora);--Elevar excepcion para clientes viejos		
end Expulsar;





procedure Mensaje_Logout (Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type)  is --Probar Logout
	Encontrado:Boolean :=False;
	Nick: ASU.Unbounded_String;
	Client_EP_Handler: LLU.End_Point_Type;
	Client_Info : Server_Handler.Value;
	begin
		Client_EP_Handler :=LLU.End_Point_Type'Input(P_Buffer);
		Nick:=ASU.Unbounded_String'Input(P_Buffer);
		--Send to all(Mensaje Logout)
		Maps.Get(Clients_Active, Nick, Client_Info, Success);
		if Success then 
			begin
			ATI.Put_Line("LOGOUT received from " & ASU.To_String(Nick));
			LLU.Reset(P_Buffer.all);
			chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); --mandar mensaje
			ASU.Unbounded_String'Output(P_Buffer, Nick);
			ASU.Unbounded_String'Output(P_Buffer, ASU.To_Unbounded_String(" leaves the chat"));
			Send_To_All(Clients_Active, Client_EP_Handler,P_Buffer);
			Maps.Delete(Clients_Active, Nick, Success);--Borrar cliente de la lista			
			Maps_Old.Put(Clients_Olds, Nick, Client_Info.Hora); --Añado el cliente borrado a una nueva lista	
			exception
				when Maps_Old.Full_Map => 
					ATI.Put("No se permiten mas clientes");
			end;	
		else 
			ATI.Put_Line("Mensaje erróneo recibido");
		end if;
end Mensaje_Logout;	


procedure Mensaje_Init(Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type) is 
	Nick: ASU.Unbounded_String;
	Client_EP_Handler: LLU.End_Point_Type;
	Client_EP_Receive: LLU.End_Point_Type;
	Client_Info : Server_Handler.Value;
	begin
		Client_EP_Receive :=	LLU.End_Point_Type'Input(P_Buffer); 
		Client_EP_Handler :=	LLU.End_Point_Type'Input(P_Buffer);
		Nick:=ASU.Unbounded_String'Input(P_Buffer);	
		ATI.Put("INIT received from " & ASU.To_String(Nick));
		Maps.Get(Clients_Active, Nick, Client_Info, Success);
		
		if not Success then --el cliente no está en la lista y lo admite 
			begin
			Client_Info.Client_EP := Client_EP_Handler; 
			Client_Info.Hora := Ada.Calendar.Clock; 
			
			Maps.Put(Clients_Active, Nick, Client_Info); --Añado a la lista
			ATI.Put_Line(": ACCEPTED");
			exception
				when Maps.Full_Map => 		
					Expulsar(P_Buffer);
					Maps.Put(Clients_Active, Nick, Client_Info);
					ATI.Put_Line("INIT received from " & ASU.To_String(Nick) & ": ACCEPTED");	
			end;								
			LLU.Reset(P_Buffer.all);	
			chat_messages.Message_Type'Output(P_Buffer,chat_messages.Welcome);--Mensaje Welcome
			Boolean'Output(P_Buffer,True);
			LLU.Send(Client_EP_Receive,P_Buffer);			
				
		else --el cliente está en la lista 
			LLU.Reset(P_Buffer.all);
			chat_messages.Message_Type'Output(P_Buffer,chat_messages.Welcome);--Mensaje Welcome
			Boolean'Output(P_Buffer,False);--No lo acoge 
			LLU.Send(Client_EP_Receive,P_Buffer);	
			ATI.Put_Line(": IGNORED. Nick already used");
		end if;	
		--Send to all(Mensaje Welcome)
		LLU.Reset(P_Buffer.all);
		Chat_messages.Message_Type'Output(P_Buffer,chat_messages.Server);		
		ASU.Unbounded_String'Output(P_Buffer,Nick);
		ASU.Unbounded_String'Output(P_Buffer,ASU.To_Unbounded_String(" joined the chat"));	
		Send_To_All(Clients_Active,Client_EP_Handler,P_Buffer);
end Mensaje_Init;


procedure Mensaje_Writer (Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type) is 

Client_EP_Handler: LLU.End_Point_Type;
Comentario : ASU.Unbounded_String;
Nick	: ASU.Unbounded_String;
Client_Info : Server_Handler.Value;
	begin
		Client_EP_Handler :=LLU.End_Point_Type'Input(P_Buffer); --Recibe el mensaje
		Nick:=ASU.Unbounded_String'Input(P_Buffer);
		Comentario:=ASU.Unbounded_String'Input(P_Buffer);		
		--Send to all(Mensaje writer)
		LLU.Reset(P_Buffer.all);
		Maps.Get(Clients_Active, Nick, Client_Info, Success);
		Client_Info.Hora := Ada.Calendar.Clock; --Actualizo la hora 
		if Success then 
			Maps.Put(Clients_Active, Nick, Client_Info);
			ATI.Put_Line("WRITER received from " & ASU.To_String(Nick) & ":" & ASU.To_String(Comentario));
			chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); --mandar mensaje
			ASU.Unbounded_String'Output(P_Buffer, Nick);
			ASU.Unbounded_String'Output(P_Buffer, Comentario);
			Send_To_All(Clients_Active,Client_EP_Handler,P_Buffer);
		else 	
			ATI.Put_Line("Mensaje erróneo recibido");			
		end if; 
end Mensaje_Writer;


Cursor: Maps.Cursor:= Maps.First(Clients_Active);
Client	:chat_messages.Message_Type;

begin
	Client := chat_messages.Message_Type'Input(P_Buffer);
	if Client=chat_messages.Init then --Recibe un mensaje Init
		Mensaje_Init(Clients_Active,P_Buffer);
	elsif Client=chat_messages.Writer then --Recibe un mensaje Writer
		Mensaje_Writer(Clients_Active,P_Buffer);
	elsif Client=chat_messages.Logout then --Recibe mensaje logout
		Mensaje_Logout(Clients_Active, P_Buffer);	
	end if;
end Handler;
end Server_Handler;
