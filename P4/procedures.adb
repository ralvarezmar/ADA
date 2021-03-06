with Ada.Text_IO;
with Protected_Ops;
with Ada.Real_Time;
with chat_messages;
with procedures;
with server_handler;
with client_handler;
with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Ada.Exceptions;

--  -I/lib/ll/lib
package body Procedures is 
   use type ASU.Unbounded_String;
   use type Ada.Real_Time.Time;
   use type chat_messages.Message_Type;
function Time_Image (T:Ada.Calendar.Time) return String is
begin 
	return Gnat.Calendar.Time_IO.Image(T,"%d-%b-%y %T.%i");
end Time_Image;

function Cortar_IP(Cliente: LLU.End_Point_Type) return ASU.Unbounded_String is
Total: ASU.Unbounded_String;
IP:ASU.Unbounded_String;
Port:ASU.Unbounded_String;
begin 
	Total:= ASU.To_Unbounded_String(LLU.Image(Cliente));	
	IP:=ASU.Tail(Total,ASU.Index(Total," ")-1);
	IP:=ASU.Head(IP,ASU.Index(IP,",")-1);
	IP:=ASU.Tail(IP, ASU.Length(IP) - ASU.Index(IP,":"));
	Port:=ASU.Tail(Total,ASU.Length(Total) - ASU.Index(Total," ")-1);	
	Port:=ASU.Tail(Port,ASU.Length(Port) - ASU.Index(Port,":"));
	Port:=ASU.Tail(Port,ASU.Length(Port) - ASU.Index(Port,":"));
	Total:= "(" & IP & ":" & Port & ")";
return Total;
end Cortar_IP;


procedure Print_Msgs_server is 
Cursor: Server_Handler.Maps_Message.Cursor := Server_Handler.Maps_Message.First(Server_Handler.Pending_Msgs);
begin
	Ada.Text_IO.Put_Line("Lista de mensajes");
	while Server_Handler.Maps_Message.Has_Element(Cursor) loop
        	Ada.Text_IO.Put("EP destino: " & ASU.To_String(Cortar_IP(Server_Handler.Maps_Message.Element(Cursor).Key.EP_D)));
		Ada.Text_IO.Put("EP origen: " & ASU.To_String(Cortar_IP(Server_Handler.Maps_Message.Element(Cursor).Key.EP_O)));
		Ada.Text_IO.Put_Line(" " & ASU.To_String(Server_Handler.Maps_Message.Element(Cursor).Value.Nick) & " " & ASU.To_String(Server_Handler.Maps_Message.Element(Cursor).Value.Comentario)); 
		Server_Handler.Maps_Message.Next(Cursor);
	end loop;

end Print_Msgs_server;

procedure Print_Msgs_client is 
Cursor: client_Handler.Maps_Message.Cursor := client_Handler.Maps_Message.First(client_Handler.Pending_Msgs);
begin
	Ada.Text_IO.Put_Line("Lista de mensajes");
	while client_Handler.Maps_Message.Has_Element(Cursor) loop
        	Ada.Text_IO.Put("EP destino: " & ASU.To_String(Cortar_IP(Client_Handler.Maps_Message.Element(Cursor).Key.EP_D))); 
		Ada.Text_IO.Put("EP origen: " & ASU.To_String(Cortar_IP(Client_Handler.Maps_Message.Element(Cursor).Key.EP_O)));
		Ada.Text_IO.Put_Line(ASU.To_String(client_Handler.Maps_Message.Element(Cursor).Value.Nick) & " " & ASU.To_String(client_Handler.Maps_Message.Element(Cursor).Value.Comentario)); 
		client_Handler.Maps_Message.Next(Cursor);
	end loop;

end Print_Msgs_client;

   procedure Retransmission_Server is
      Hora: Ada.Real_Time.Time;
      Encontrado : Boolean := False; 
      Info_Msg: Server_Handler.Mess_ID;
      Message : Server_Handler.Mensaje;
      Buffer	: aliased LLU.Buffer_Type(1024);
      Success : Boolean;
      Hora_Msg: Ada.Real_Time.Time;
    begin	
	Hora := Ada.Real_Time.Clock; 
	
	Server_Handler.Maps_Rts.Get(Server_Handler.Retransmission_Times, Hora_Msg, Info_Msg, Success);
	if Hora_Msg < Hora and Success then 
		server_handler.Maps_Message.Get(server_Handler.Pending_Msgs,Info_Msg,Message,Success);--Busco para obtener el mensaje
		if Success then 
			if Message.N_Rts < server_Handler.Max_Rts then --Envío mensaje
				--Print_Msgs_server;
				--Ada.Text_IO.Put_Line("Mensaje de: " & ASU.To_String(Message.Nick) & ": " & ASU.To_String(Message.Comentario));
				--Ada.Text_IO.Put_Line("EP destino: " & LLU.Image(Info_Msg.EP_D)); 
				--Ada.Text_IO.Put_Line("EP origen: " & LLU.Image(Info_Msg.EP_O));
				--Ada.Text_IO.Put_Line("Número de retransmisiones: " & Integer'Image(Message.N_Rts));
				LLU.Reset(Buffer);
				chat_messages.Message_Type'Output(Buffer'Access, chat_messages.Server);
				LLU.End_Point_Type'Output(Buffer'Access, Info_Msg.EP_O);
				Server_Handler.Seq_N_T'Output(Buffer'Access, Info_Msg.Num);
				ASU.Unbounded_String'Output(Buffer'Access, Message.Nick);
				ASU.Unbounded_String'Output(Buffer'Access, Message.Comentario);
				LLU.Send(Info_Msg.EP_D,Buffer'Access);
				server_handler.Maps_Rts.Delete(Server_Handler.Retransmission_Times,Success);
				Message.N_Rts := Message.N_Rts + 1; 
				Message.Nick := Message.Nick;
				Message.Comentario := Message.Comentario;
				Server_Handler.Maps_Message.Put(Server_Handler.Pending_Msgs, Info_Msg, Message);
				Hora := Ada.Real_Time.Clock;	
				Server_Handler.Maps_Rts.Put(Server_Handler.Retransmission_Times, Hora + Server_Handler.Plazo_Retransmision, Info_Msg); 				else
				server_handler.Maps_Message.Delete(Server_Handler.Pending_Msgs,Info_Msg,Success);
			end if;
		else  --Borro mensaje 
			Server_handler.Maps_Rts.Delete(Server_Handler.Retransmission_Times,Success);
		end if;
	Server_Handler.Maps_Rts.Get(Server_Handler.Retransmission_Times, Hora_Msg, Info_Msg, Success);
	end if;
	Hora := Ada.Real_Time.Clock;
	protected_ops.Program_Timer_Procedure(Procedures.Retransmission_Server'access, Hora + Server_Handler.Plazo_Retransmision);
   end Retransmission_Server;

procedure Retransmission_Client is
	Hora: Ada.Real_Time.Time;
      Encontrado : Boolean := False; 
      Info_Msg: Client_Handler.Mess_ID;
      Message : Client_Handler.Mensaje;
      Buffer	: aliased LLU.Buffer_Type(1024);
      Success : Boolean;
      Hora_Msg: Ada.Real_Time.Time;
    begin
	Hora:= Ada.Real_Time.Clock;
	Client_Handler.Maps_Rts.Get(Client_Handler.Retransmission_Times,Hora_Msg, Info_Msg, Success); 
	if Hora_Msg < Hora and Success then
		Client_handler.Maps_Message.Get(Client_Handler.Pending_Msgs,Info_Msg,Message,Success); --Busco para obtener el mensaje
		if Success then 			
			if Message.N_Rts < Client_Handler.Max_Rts then --Envío mensaje
				--Print_Msgs_client;
				--if Message.Tipo = chat_messages.Writer then
				--	Ada.Text_IO.Put_Line("Mensaje de " & ASU.To_String(Message.Nick) & ": " & ASU.To_String(Message.Comentario));
				--elsif Message.Tipo = chat_messages.Logout then
				--	Ada.Text_IO.Put_Line("Mensaje Logout de " & ASU.To_String(Message.Nick));
				--end if;
				--Ada.Text_IO.Put_Line("EP destino: " & LLU.Image(Info_Msg.EP_D));
				--Ada.Text_IO.Put_Line("EP origen: " & LLU.Image(Info_Msg.EP_O));
				--Ada.Text_IO.Put_Line("Número de retransmisiones: " & Integer'Image(Message.N_Rts));
				LLU.Reset(Buffer);
				chat_messages.Message_Type'Output(Buffer'Access, Message.Tipo);
				LLU.End_Point_Type'Output(Buffer'Access, Info_Msg.EP_O);
				Client_Handler.Seq_N_T'Output(Buffer'Access, Info_Msg.Num);
				ASU.Unbounded_String'Output(Buffer'Access, Message.Nick);
				if Message.Tipo = chat_messages.Writer then
					ASU.Unbounded_String'Output(Buffer'Access, Message.Comentario);
				end if;
				LLU.Send(Info_Msg.EP_D,Buffer'Access);
				client_handler.Maps_Rts.Delete(Client_Handler.Retransmission_Times,Success); --Borro la retransmision para hacerla mas tarde
				Message.N_Rts := Message.N_Rts + 1; 
				--Message.Tipo := Message.Tipo;
				--Message.Nick := Message.Nick;
				Client_Handler.Maps_Message.Put(Client_Handler.Pending_Msgs, Info_Msg, Message);
				Hora:= Ada.Real_Time.Clock;
				Client_Handler.Maps_Rts.Put(Client_Handler.Retransmission_Times, Hora_Msg + Client_Handler.Plazo_Retransmision, Info_Msg);
			else
				client_handler.Maps_Message.Delete(Client_Handler.Pending_Msgs,Info_Msg,Success);
			end if;
		else  --Borro mensaje 
			client_handler.Maps_Rts.Delete(Client_Handler.Retransmission_Times,Success);
		end if;
		client_Handler.Maps_Rts.Get(Client_Handler.Retransmission_Times,Hora_Msg, Info_Msg, Success);
	end if;	
	Hora := Ada.Real_Time.Clock;
	--Delay (Duration(2 * (Client_Handler.Max_Delay) / 1000));
	protected_ops.Program_Timer_Procedure(Procedures.Retransmission_Client'access,Hora + Client_Handler.Plazo_Retransmision);
exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista en retransmisiones: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end Retransmission_Client; 


--Imprime la lista de clientes activos

procedure Print_Active_Inv is
Cursor: Server_Handler.Maps.Cursor := Server_Handler.Maps.Last(Server_Handler.Clients_Active);
begin
	while Server_Handler.Maps.Has_Element(Cursor) loop
       		 Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put(ASU.To_String(Cortar_IP(Server_Handler.Maps.Element(Cursor).Value.Client_EP)) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps.Element(Cursor).Value.Hora));  
		Server_Handler.Maps.Prev(Cursor);
	end loop;
end Print_Active_Inv;

procedure Print_Active is --Mostrar clientes activos
Cursor: Server_Handler.Maps.Cursor := Server_Handler.Maps.First(Server_Handler.Clients_Active);
begin
	while Server_Handler.Maps.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put(ASU.To_String(Cortar_IP(Server_Handler.Maps.Element(Cursor).Value.Client_EP)) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps.Element(Cursor).Value.Hora)); 
		Server_Handler.Maps.Next(Cursor);
	end loop;
end Print_Active;

--Imprime la lista de clientes antiguos
procedure Print_Old is 
Cursor: Server_Handler.Maps_Old.Cursor := Server_Handler.Maps_Old.First(Server_Handler.Clients_Olds);
begin
	while Server_Handler.Maps_Old.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps_Old.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps_Old.Element(Cursor).Value)); 
		Server_Handler.Maps_Old.Next(Cursor);
	end loop;

end Print_Old;

procedure Print_Old_Inv is 
Cursor: Server_Handler.Maps_Old.Cursor := Server_Handler.Maps_Old.Last(Server_Handler.Clients_Olds);
begin
	while Server_Handler.Maps_Old.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps_Old.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps_Old.Element(Cursor).Value)); 
		Server_Handler.Maps_Old.Prev(Cursor);
	end loop;

end Print_Old_Inv;
end Procedures;
	

