with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Server_handler;
with chat_messages;
with Hash_Maps_G;
with Maps_G;
with Ada.Real_Time;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with procedures;
with protected_ops;
with Ada.Exceptions;

package body Server_Handler is	
  	
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	use type Ada.Calendar.Time;
	use type Ada.Real_Time.Time;


-------------------------FUNCIONES HASH----------------------------

function Hash_Client(S:ASU.Unbounded_String) return Hash_Range is
Hash: Hash_Range :=0;
begin
for i in 1..ASU.Length(S) loop
	Hash := Hash_Range'Mod(character'pos(ASU.To_String(S)(i)) + Integer(Hash));
end loop;
return Hash;
end Hash_Client;


function Hash_Client_Old(S:ASU.Unbounded_String) return Hash_Range_Old is
Hash: Hash_Range_Old:=0;
begin
for i in 1..ASU.Length(S) loop
	Hash := Hash_Range_Old'Mod(character'pos(ASU.To_String(S)(i)) + Integer(Hash));
end loop;
return Hash;
end Hash_Client_Old;



function Hash_Message(K:Mess_ID) return Hash_Range_Message is 
EP: ASU.Unbounded_String;
Hash: Hash_Range_Message:=0;
begin
	
	EP := ASU.To_Unbounded_String(LLU.Image(K.EP_D) & LLU.Image(K.EP_O));
	for i in 1..ASU.Length(EP) loop
		Hash := Hash_Range_Message'Mod(character'pos(ASU.To_String(EP)(i)) + Integer(Hash));
	end loop;
	Hash := Hash_Range_Message'Mod(Integer(Hash) + Integer(K.Num));
return Hash;
end Hash_Message;

----------------FUNCION COMPARAR-----------------------------------------
function Comparar(Variable1:Mess_ID; Variable2:Mess_ID) return Boolean is
begin 
if Variable1.EP_D = Variable2.EP_D and then Variable1.EP_O = Variable2.EP_O 
								and then Variable1.Num = Variable2.Num then
	return True;
else
	return False; 
end if;

end Comparar;
----------------------------------------------------------SERVER--------------------------------
Success : Boolean;

procedure Send_To_All(Map: in Maps.Map;  
			Client_EP_Handler: in LLU.End_Point_Type;
			P_Buffer: access LLU.Buffer_Type;			
			Message: Mensaje;
			Server_EP: in LLU.End_Point_Type)  is
Cursor: Maps.Cursor:= Maps.First(Clients_Active);
Info_Msg: Mess_ID;
Hora: Ada.Calendar.Time:= Ada.Calendar.Clock;
Client_Info: client_info_type;
	begin 
		while Maps.Has_Element(Cursor) loop  
			if Maps.Element(Cursor).Value.Client_EP /= Client_EP_Handler then
				Maps.Get(Clients_Active, Maps.Element(Cursor).Key, Client_Info, Success);
				LLU.Reset(P_Buffer.all);
				Chat_messages.Message_Type'Output(P_Buffer,chat_messages.Server); --Tipo
				LLU.End_Point_Type'Output(P_Buffer, Server_EP);	 -- server EP
				Client_Info.Msg_Enviado := Client_Info.Msg_Enviado + 1; --Sumo 1 al ultimo mensaje enviado al cliente
				Seq_N_T'Output(P_Buffer, Client_Info.Msg_Enviado); --Numero secuencia
				ASU.Unbounded_String'Output(P_Buffer, Message.Nick);	--Nick
				ASU.Unbounded_String'Output(P_Buffer,Message.Comentario);	--Comentario
				LLU.Send(Maps.Element(Cursor).Value.Client_EP,P_Buffer);
				--ATI.Put_Line("Mando mensaje SERVER a " & ASU.To_String(Maps.Element(Cursor).Key));
				--ATI.Put_Line(ASU.To_String(Message.Nick) & " " & ASU.To_String(Message.Comentario));
				--Guardar Mensaje mandado				
				Info_Msg.EP_D := Maps.Element(Cursor).Value.Client_EP;
				Info_Msg.EP_O := Server_EP;
				Info_Msg.Num := Client_Info.Msg_Enviado;	--CAMBIAR
				Client_Info.Client_EP := Info_Msg.EP_D;	
				--Client_Info.Last_Msg := Client_Info.Last_Msg; --CAMBIAR
				Maps.Put(Clients_Active, Maps.Element(Cursor).Key, Client_Info); --Actualizo los datos del cliente
				Maps_Message.Put(Pending_Msgs, Info_Msg, Message); --Añado mensaje a la lista
				Hora:= Ada.Calendar.Clock;
				--Maps.Put(Clients_Active, Message.Nick, Client_Info); --Añado a la lista para actualizar el numero de secuencia
				Maps_Rts.Put(Retransmission_Times,Ada.Real_Time.Clock + Server_Handler.Plazo_Retransmision,Info_Msg); --Añado a la lista de retransmision			
				if Server_Handler.Maps_Rts.Map_Length(Server_Handler.Retransmission_Times) >= 1 then
					Protected_Ops.Program_Timer_Procedure(Procedures.Retransmission_Server'access, Ada.Real_Time.Clock + 						Server_Handler.Plazo_Retransmision);
				end if;
			end if;
		Maps.Next(Cursor);
		end loop;
end Send_To_All;


procedure Expulsar(P_Buffer: access LLU.Buffer_Type;
		    Client_EP: in LLU.End_Point_Type;
		   Server_EP: in LLU.End_Point_Type) is 
Cursor: Maps.Cursor:= Maps.First(Clients_Active);
Hora: Ada.Calendar.Time;
Aux : Maps.Element_Type;
Aux_Msg: Maps_Message.Element_Type;
begin
	Hora:= Maps.Element(Cursor).Value.Hora;
	while Maps.Has_Element(Cursor) loop
		if Hora>=Maps.Element(Cursor).Value.Hora then
			Aux.Key:=Maps.Element(Cursor).Key;
			Aux.Value:=Maps.Element(Cursor).Value;
		end if;			 
		Maps.Next(Cursor);
	end loop;
	Maps.Get(Clients_Active, Aux.Key, Aux.Value, Success);
	ATI.New_Line;
	ATI.Put_Line("Expulsado " & ASU.To_String(Aux.Key));
	Aux.Value.Msg_Enviado := Aux.Value.Msg_Enviado + 1;
	--mandar mensaje de baneo
	LLU.Reset(P_Buffer.all);
	chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); 
	--Añadir el server EP
	LLU.End_Point_Type'Output(P_Buffer, Server_EP);	
	Seq_N_T'Output(P_Buffer, Aux.Value.Msg_Enviado);
	ASU.Unbounded_String'Output(P_Buffer, Aux.Key);
	ASU.Unbounded_String'Output(P_Buffer, ASU.To_Unbounded_String(" banned for being idle too long"));
	--Guardo el mensaje enviado
	Aux_Msg.Value.Nick := Aux.Key;
	Aux_Msg.Value.Comentario := ASU.To_Unbounded_String(" banned for being idle too long");	
	Aux_Msg.Key.EP_D := Client_EP;
	Aux_Msg.Key.EP_O := Server_EP;
	Aux_Msg.Key.Num := Aux.Value.Msg_Enviado; 
	Maps_Message.Put(Pending_Msgs, Aux_Msg.Key, Aux_Msg.Value);--Añado el mensaje enviado
	--------------
	--Ada.Text_IO.Put_Line("EP cliente baneado: " & LLU.Image(Aux.Value.Client_EP)); 
	LLU.Send(Aux.Value.Client_EP,P_Buffer); --Envio al cliente baneado
	Delay 5.0;
	Send_To_All(Clients_Active, Aux.Value.Client_EP,P_Buffer, Aux_Msg.Value,Server_EP);	
	Maps.Delete(Clients_Active, Aux.Key, Success);
	Maps_Old.Put(Clients_Olds, Aux.Key, Aux.Value.Hora);
end Expulsar;

---------------------------------INIT-------------------------
procedure Mensaje_Init(Lista: in out Maps.Map;
			P_Buffer: access LLU.Buffer_Type;
			Server_EP: in LLU.End_Point_Type) is 
	Client_EP_Handler: LLU.End_Point_Type;
	Client_EP_Receive: LLU.End_Point_Type;
	Client_Info : Server_Handler.client_info_type;
	Nick: ASU.Unbounded_String;
	Message_Info: Server_Handler.Mensaje;
	begin
		Client_EP_Receive :=	LLU.End_Point_Type'Input(P_Buffer); 
		Client_EP_Handler :=	LLU.End_Point_Type'Input(P_Buffer);
		Message_Info.Nick:=ASU.Unbounded_String'Input(P_Buffer);		
		Maps.Get(Lista, Message_Info.Nick, Client_Info, Success); 
		if not Success or else Client_Info.Client_EP = Client_EP_Handler then --el cliente no está en la lista y lo admite 
			begin
			if not Success then
				ATI.Put("INIT received from " & ASU.To_String(Message_Info.Nick));					   
				ATI.Put_Line(": ACCEPTED");
			end if; 
			Client_Info.Client_EP := Client_EP_Handler;			
			Client_Info.Hora := Ada.Calendar.Clock;
			Client_Info.Last_Msg := 0;
			Maps.Put(Lista, Message_Info.Nick, Client_Info); --Añado a la lista
			if Server_Handler.Maps.Map_Length(Lista) > Server_Handler.Max then
				 Expulsar(P_Buffer, Client_EP_Handler, Server_EP);
			end if;
			exception
				when Maps.Full_Map => 		
					Expulsar(P_Buffer, Client_EP_Handler, Server_EP);
					Maps.Put(Lista, Message_Info.Nick, Client_Info);
					ATI.Put_Line("INIT received from " & ASU.To_String(Message_Info.Nick) & ": ACCEPTED");	
			end;								
			LLU.Reset(P_Buffer.all);
			chat_messages.Message_Type'Output(P_Buffer,chat_messages.Welcome);--Mensaje Welcome
			Boolean'Output(P_Buffer,True);
			LLU.Send(Client_EP_Receive,P_Buffer);
			Message_Info.Comentario := ASU.To_Unbounded_String(" joined the chat");		

		else --el cliente está en la lista 
			LLU.Reset(P_Buffer.all);
			if Client_EP_Handler=Client_Info.Client_EP then
				chat_messages.Message_Type'Output(P_Buffer,chat_messages.Welcome);--Mensaje Welcome
				Boolean'Output(P_Buffer,True);
				ATI.Put("INIT received from " & ASU.To_String(Message_Info.Nick));					   
				ATI.Put_Line(": ACCEPTED");
			else
				chat_messages.Message_Type'Output(P_Buffer,chat_messages.Welcome);--Mensaje Welcome
				Boolean'Output(P_Buffer,False);--No lo acoge 
				ATI.Put("INIT received from " & ASU.To_String(Message_Info.Nick));
				ATI.Put_Line(": IGNORED. Nick already used");
				Message_Info.Comentario := ASU.To_Unbounded_String(" Ignored. Nick already used");
				--Send_To_All(Lista,Client_EP_Handler,P_Buffer,Message_Info,Server_EP); --Send to all(Mensaje Welcome)
			end if;
			LLU.Send(Client_EP_Receive,P_Buffer);
		end if;
		--LLU.Reset(P_Buffer.all);
		--Chat_messages.Message_Type'Output(P_Buffer,chat_messages.Server);
		--LLU.End_Point_Type'Output(P_Buffer, Server_EP);	 -- server EP
		--Seq_N_T'Output(P_Buffer, Seq_N);		
		--ASU.Unbounded_String'Output(P_Buffer, Message_Info.Nick);
		--ASU.Unbounded_String'Output(P_Buffer,Message_Info.Comentario);	
		if not Success or (Success and Client_EP_Handler/=Client_Info.Client_EP) then 
			Send_To_All(Lista,Client_EP_Handler,P_Buffer,Message_Info,Server_EP); --Send to all(Mensaje Welcome)
		end if;	
end Mensaje_Init;


-----------------------------ACK---------------------------
procedure Mensaje_ACK(P_Buffer: access LLU.Buffer_Type;
			Server_EP: in LLU.End_Point_Type) is
Ack_R : Mess_ID;
Success: Boolean;
Message_Info : Mensaje;
begin
	Ack_R.EP_D := LLU.End_Point_Type'Input(P_Buffer); --REVISAR
	Ack_R.Num:= Seq_N_T'input(P_Buffer);
	Ack_R.EP_O := Server_EP;
	--ATI.Put_Line("EP destino: " & LLU.Image(Ack_R.EP_D));
	--ATI.Put_Line("EP origen: " & LLU.Image(Ack_R.EP_O));
	--ATI.Put_Line("Numero de secuencia: " & Seq_N_T'Image(Ack_R.Num));
	Maps_Message.Get(Pending_Msgs, Ack_R, Message_Info, Success);
	if Success then
		Maps_Message.Delete(Pending_Msgs, Ack_R, Success);
	end if;
end; 

procedure Enviar_ACK (Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type;
			Client_Info: in client_info_type;
			Server_EP: in LLU.End_Point_Type;
			Num_Seq: in Seq_N_T) is 
begin	
	LLU.Reset(P_Buffer.all);
	chat_messages.Message_Type'Output(P_Buffer, chat_messages.ACK);
	LLU.End_Point_Type'Output(P_Buffer,Server_EP); --EP_H_ACKer
	--ATI.Put_Line("EP enviado en ACK: " & LLU.Image(Server_EP));
	--ATI.Put_Line("Numero de secuencia enviado en ACK: " & Seq_N_T'Image(Num_Seq));
	Seq_N_T'Output(P_Buffer, Num_Seq);
	--ATI.Put_Line("Buffer: " & LLU.Image(P_Buffer.all));
	LLU.Send(Client_Info.Client_EP,P_Buffer);
	--ATI.Put_Line("ACK enviado a: " & LLU.Image(Client_Info.Client_EP));

end Enviar_ACK;
----------------------------WRITER--------------------------------
procedure Mensaje_Writer (Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type;
			Cient_Info: in out client_info_type;
			Server_EP: in LLU.End_Point_Type) is 

Client_EP_Handler: LLU.End_Point_Type;
Client_Info : client_info_type;
Aux_Msg: Maps_Message.Element_Type;
Seq_Recibido: Seq_N_T;
	begin
		Client_EP_Handler :=LLU.End_Point_Type'Input(P_Buffer); --Recibe el mensaje
		Seq_Recibido:= Seq_N_T'Input(P_Buffer);
		Aux_Msg.Value.Nick:=ASU.Unbounded_String'Input(P_Buffer);
		Aux_Msg.Value.Comentario:=ASU.Unbounded_String'Input(P_Buffer);
		--Send to all(Mensaje writer)
		LLU.Reset(P_Buffer.all);
		--ATI.Put_Line("Numero de secuencia: " & Seq_N_T'Image(Seq_Recibido));
		Maps.Get(Clients_Active, Aux_Msg.Value.Nick, Client_Info, Success);
		--ATI.Put_Line("Numero de secuencia esperado: " & Seq_N_T'Image(Client_Info.Last_Msg + 1));
		Client_Info.Hora := Ada.Calendar.Clock; --Actualizo la hora 
		if Success then 
			if seq_Recibido >= Client_Info.Last_Msg + 1  then
				if Client_Info.Last_Msg + 1 = seq_Recibido then		
					ATI.Put_Line("WRITER received from " & ASU.To_String(Aux_Msg.Value.Nick) & ":" & ASU.To_String(Aux_Msg.Value.Comentario));
					--chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); --mandar mensaje
					--LLU.End_Point_Type'Output(P_Buffer, Server_EP);	
					--Seq_N:= Seq_N + 1;
					Client_Info.Last_Msg := Client_Info.Last_Msg+1; --Actualizo el ultimo mensaje del cliente
					Client_Info.Hora := Ada.Calendar.Clock; -- Actualizo la hora
					--Añadir a lista de mensajes
					Aux_Msg.Key.EP_D := Client_EP_Handler;
					Aux_Msg.Key.EP_O := Server_EP;
					Aux_Msg.Key.Num := Seq_Recibido; 
					Maps.Put(Clients_Active, Aux_Msg.Value.Nick, Client_Info);
					Maps_Message.Put(Pending_Msgs,Aux_Msg.Key,Aux_Msg.Value); 
					Maps.Put(Clients_Active, Aux_Msg.Value.Nick, Client_Info);
					Enviar_ACK(Clients_Active,P_Buffer,Client_Info,Server_EP,Seq_Recibido); 
					Send_To_All(Clients_Active, Client_EP_Handler, P_Buffer, Aux_Msg.Value,Server_EP); --Mandar a todos los clientes
				end if;
			else
				Enviar_ACK(Clients_Active,P_Buffer,Client_Info,Server_EP,Seq_Recibido);
			end if;
		else 	
			ATI.Put_Line("Mensaje erróneo recibido");
		end if;
end Mensaje_Writer;


--------------------------LOGOUT----------------------------------
procedure Mensaje_Logout (Map: in Maps.Map;
			P_Buffer: access LLU.Buffer_Type;
			Client_Info: out client_info_type;
			EP_Server: in LLU.End_Point_Type)  is 
	Encontrado:Boolean :=False;
	Client_EP_Handler: LLU.End_Point_Type;
	N_Seq : Seq_N_T;
	Message_Info: Mensaje;
	begin
		Client_EP_Handler :=LLU.End_Point_Type'Input(P_Buffer);
		N_Seq := Seq_N_T'Input(P_Buffer);
		Message_Info.Nick:=ASU.Unbounded_String'Input(P_Buffer);
		--Send to all(Mensaje Logout)
		Maps.Get(Clients_Active, Message_Info.Nick, Client_Info, Success);
		--Client_Info.Last_Msg := N_Seq;
		if Success then 
			begin
			if N_Seq >= Client_Info.Last_Msg + 1  then
				if N_Seq = Client_Info.Last_Msg + 1 then			
					Client_Info.Last_Msg := Client_Info.Last_Msg + 1;
					ATI.Put_Line("LOGOUT received from " & ASU.To_String(Message_Info.Nick));
					Enviar_ACK(Clients_Active,P_Buffer,Client_Info,EP_Server,N_Seq);	
					LLU.Reset(P_Buffer.all);
					chat_messages.Message_Type'Output(P_Buffer, chat_messages.Server); --mandar mensaje
					LLU.End_Point_Type'Output(P_Buffer, EP_Server);			
					Seq_N_T'Output(P_Buffer, Client_Info.Msg_Enviado);
					Message_Info.Comentario := ASU.To_Unbounded_String(" leaves the chat");	
					ASU.Unbounded_String'Output(P_Buffer, Message_Info.Nick);
					ASU.Unbounded_String'Output(P_Buffer, Message_Info.Comentario);
					Send_To_All(Clients_Active, Client_EP_Handler,P_Buffer, Message_Info, EP_Server);
					Maps.Delete(Clients_Active, Message_Info.Nick, Success);--Borrar cliente de la lista			
					Maps_Old.Put(Clients_Olds, Message_Info.Nick, Client_Info.Hora); --Añado el cliente borrado a una nueva lista	
				end if;
			else				
				Enviar_ACK(Clients_Active,P_Buffer,Client_Info,EP_Server,N_Seq);
			end if;	
			exception
				when Maps_Old.Full_Map => 
					ATI.Put("No se permiten mas clientes");
			end;
		--else 
		--	ATI.Put_Line("Mensaje erróneo recibido");
		end if;
end Mensaje_Logout;	



procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is

Client	:chat_messages.Message_Type;
Client_Info: client_info_type;
begin
	Client := chat_messages.Message_Type'Input(P_Buffer);
	if Client=chat_messages.Init then --Recibe un mensaje Init
		--ATI.Put_Line("Recibido INIT");
		Mensaje_Init(Clients_Active,P_Buffer,To);
	elsif Client=chat_messages.Writer then --Recibe un mensaje Writer
		--ATI.Put_Line("Recibido WRITER");
		Mensaje_Writer(Clients_Active,P_Buffer,Client_Info,To);
	elsif Client=chat_messages.Logout then --Recibe mensaje logout
		--ATI.Put_Line("Recibido LOGOUT");
		Mensaje_Logout(Clients_Active, P_Buffer,Client_Info,To);	
	elsif Client=chat_messages.ACK then
		Mensaje_ACK(P_Buffer,To);
	end if;
exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista en el handler: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Handler;
end Server_Handler;
