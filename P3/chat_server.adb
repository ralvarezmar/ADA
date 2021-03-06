with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with chat_messages;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Server_handler;
with Maps_G;
with Ada.Command_Line;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;

procedure chat_server is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;

function Time_Image (T:Ada.Calendar.Time) return String is
begin 
	return Gnat.Calendar.Time_IO.Image(T,"%d-%b-%y %T.%i");
end Time_Image;

--Imprime la lista de clientes activos

function Cortar_IP(Cliente: LLU.End_Point_Type) return ASU.Unbounded_String is
Total: ASU.Unbounded_String;
IP:ASU.Unbounded_String;
Port:ASU.Unbounded_String;
begin 
	Total:= ASU.To_Unbounded_String(LLU.Image(Cliente));
	
	IP:=ASU.Tail(Total,ASU.Index(Total," ")-1);
	IP:=ASU.Head(IP,ASU.Index(IP,",")-1);
	IP:=ASU.Tail(IP, ASU.Length(IP) - ASU.Index(IP,":"));
	Port:=ASU.Tail(Total,ASU.Index(Total," ")-1);
	Port:=ASU.Tail(Port,ASU.Index(Port,":"));
	Total:= "(" & IP & ":" & Port & ")";
return Total;
end Cortar_IP;

procedure Print_Active_Inv(Clients: Server_Handler.Maps.Map) is
Cursor: Server_Handler.Maps.Cursor := Server_Handler.Maps.Last(Clients);
begin
	while Server_Handler.Maps.Has_Element(Cursor) loop
       		 Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put(ASU.To_String(Cortar_IP(Server_Handler.Maps.Element(Cursor).Value.Client_EP)) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps.Element(Cursor).Value.Hora));  
		Server_Handler.Maps.Prev(Cursor);
	end loop;
end Print_Active_Inv;

procedure Print_Active (Clients: Server_Handler.Maps.Map) is --Mostrar clientes activos
Cursor: Server_Handler.Maps.Cursor := Server_Handler.Maps.First(Clients);
begin
	while Server_Handler.Maps.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put(ASU.To_String(Cortar_IP(Server_Handler.Maps.Element(Cursor).Value.Client_EP)) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps.Element(Cursor).Value.Hora)); 
		Server_Handler.Maps.Next(Cursor);
	end loop;
end Print_Active;

--Imprime la lista de clientes antiguos
procedure Print_Old (Clients: Server_Handler.Maps_Old.Map) is 
Cursor: Server_Handler.Maps_Old.Cursor := Server_Handler.Maps_Old.First(Clients);
begin
	while Server_Handler.Maps_Old.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps_Old.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps_Old.Element(Cursor).Value)); 
		Server_Handler.Maps_Old.Next(Cursor);
	end loop;

end Print_Old;

procedure Print_Old_Inv (Clients: Server_Handler.Maps_Old.Map) is 
Cursor: Server_Handler.Maps_Old.Cursor := Server_Handler.Maps_Old.Last(Clients);
begin
	while Server_Handler.Maps_Old.Has_Element(Cursor) loop
        	Ada.Text_IO.Put(ASU.To_String(Server_Handler.Maps_Old.Element(Cursor).Key) & " ");
		Ada.Text_IO.Put_Line(Time_Image(Server_Handler.Maps_Old.Element(Cursor).Value)); 
		Server_Handler.Maps_Old.Prev(Cursor);
	end loop;

end Print_Old_Inv;

	Server_EP : LLU.End_Point_Type;	
	Ver_Servidor: ASU.Unbounded_String;
	Port	:  Integer;
	Comentario : ASU.Unbounded_String;
	Nick	: ASU.Unbounded_String;	
	C: 	character;
	Usage_Error	: exception;
begin


	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(LLU.Get_Host_Name)); 
	Port		:= Integer'Value(Ada.Command_Line.Argument(1));
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);
	LLU.Bind (Server_EP,Server_Handler.Handler'access);	

	if Ada.Command_Line.Argument_Count /= 2 then
		raise Usage_Error; 
	end if; 
	if  2<=Integer'Value(Ada.Command_Line.Argument(2)) and 50 > Integer'Value(Ada.Command_Line.Argument(2)) then 
	Ada.Text_IO.Put_Line ("Servidor arrancado"); 
	
		loop
		Ada.Text_IO.Get_Immediate (C);
			if C = 'l'  then
				--Clientes activos por orden
				Ada.Text_IO.Put_Line ("ACTIVE CLIENTS");
				Ada.Text_IO.Put_Line ("============");
				Print_Active(Server_Handler.Clients_Active);
			elsif C = 'L'then
				--Clientes activos por orden inverso
				Ada.Text_IO.Put_Line ("ACTIVE CLIENTS");
				Ada.Text_IO.Put_Line ("============");
				Print_Active_Inv(Server_Handler.Clients_Active);
			elsif C = 'O' then
				--Clientes antiguos inversamente al orden
				Ada.Text_IO.Put_Line ("OLD CLIENTS");
				Ada.Text_IO.Put_Line ("===========");
				Print_Old_Inv(Server_Handler.Clients_Olds);
			elsif C= 'o' then
				--Clientes antiguos(primero muestra el primero que salió)
				Ada.Text_IO.Put_Line ("OLD CLIENTS");
				Ada.Text_IO.Put_Line ("===========");
				Print_Old(Server_Handler.Clients_Olds);
			else
				Ada.Text_IO.Put_Line ("Pulse 'l' o 'L' para ver la lista de clientes."); 
				Ada.Text_IO.Put_Line ("Pulse 'o' u 'O' para mostrar clientes antiguos");
			end if;
		end loop;
	else 
		
		raise Usage_Error;		
	end if; 	
exception
  
	when Ex:Usage_Error =>
	Ada.Text_IO.Put_Line ("Ponga un número de clientes entre 2 y 50");
	LLU.Finalize;
	when Ex:others =>
        Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;
end chat_server;


