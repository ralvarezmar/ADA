with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with chat_messages;
with client_handler;
with Ada.Calendar;
with Ada.Real_Time;
with Hash_Maps_G;
with Maps_G;
with Protected_Ops;
with procedures;
with Ada.Exceptions;

procedure chat_client is
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;
	use type Ada.Real_Time.Time;
	use type client_handler.Seq_N_T;


Seq_N: client_Handler.Seq_N_T := 0;

procedure Mensaje_Init(Buffer: access LLU.Buffer_Type;
			 Client_EP_Receive: in LLU.End_Point_Type;
			Client_EP_Handler:in LLU.End_Point_Type;
			Server_EP :in LLU.End_Point_Type;
			Nick: in ASU.Unbounded_String) is
begin		
		LLU.Reset(Buffer.all);
		chat_messages.Message_Type'Output(Buffer, chat_messages.Init); 
		LLU.End_Point_Type'Output(Buffer,Client_EP_Receive);
		LLU.End_Point_Type'Output(Buffer,Client_EP_Handler); --cliente handler
		ASU.Unbounded_String'Output(Buffer, Nick); --Nombre del cliente		
		LLU.Send(Server_EP,Buffer);
end Mensaje_Init;


procedure Mensaje_Logout(Buffer: access LLU.Buffer_Type;
			 Client_EP_Handler:in LLU.End_Point_Type;
			 Server_EP :in LLU.End_Point_Type;			 
			 Nick: in ASU.Unbounded_String)	is
Info: Client_Handler.Mess_ID;
Value: Client_Handler.Mensaje;
Hora: Ada.Real_Time.Time := Ada.Real_Time.Clock;
begin
		LLU.Reset(Buffer.all);
		Seq_N:= Seq_N + 1;
		chat_messages.Message_Type'Output(Buffer, chat_messages.Logout);
		LLU.End_Point_Type'Output(Buffer,Client_EP_Handler);
		client_Handler.Seq_N_T'Output(Buffer, Seq_N);
		ASU.Unbounded_String'Output(Buffer, Nick);
		LLU.Send(Server_EP,Buffer);
		Info.EP_D := Server_EP;
		Info.EP_O := Client_EP_Handler;
		Info.Num := Seq_N; 
		Value.Nick := Nick;
		Value.Tipo := chat_messages.Logout;
		Hora := Ada.Real_Time.Clock;
		Client_Handler.Maps_Message.Put(Client_Handler.Pending_Msgs, Info, Value);
		Client_Handler.Maps_Rts.Put(Client_Handler.Retransmission_Times, Hora + Client_Handler.Plazo_Retransmision, Info);
		--ATI.Put_Line(LLU.Image(Server_EP));
--		ATI.Put_Line("Guardo el Mensaje");
		if Client_Handler.Maps_Rts.Map_Length(Client_Handler.Retransmission_Times) >= 1 then
			Protected_ops.Program_Timer_Procedure(Procedures.Retransmission_Client'access, Hora + Client_Handler.Plazo_Retransmision);
		end if;
end Mensaje_Logout;

	

procedure Mandar_Mensajes(Buffer: access LLU.Buffer_Type;
			 Client_EP_Handler:in LLU.End_Point_Type;
			 Server_EP :in LLU.End_Point_Type;			 
			 Nick: in ASU.Unbounded_String;
			 Request: out ASU.Unbounded_String) is
Clave: Client_Handler.Mess_ID;
Value: Client_Handler.Mensaje;
Hora: Ada.Real_Time.Time := Ada.Real_Time.Clock;
	begin
		ATI.Put(">> ");
		Request := ASU.To_Unbounded_String(ATI.Get_Line);				
		--Mando el mensaje
		if Request/=".quit" then
			Seq_N:= Seq_N + 1;
			LLU.Reset(Buffer.all);
			chat_messages.Message_Type'Output(Buffer, chat_messages.Writer);
			LLU.End_Point_Type'Output(Buffer,Client_EP_Handler);
			client_handler.Seq_N_T'Output(Buffer, Seq_N);				
			ASU.Unbounded_String'Output(Buffer, Nick);
			ASU.Unbounded_String'Output(Buffer, Request); --Guarda el mensaje en el buffer
			LLU.Send(Server_EP, Buffer); --Manda el contenido del buffer
			--Añado a la lista de mensajes pendientes
			Clave.EP_D := Server_EP;
			Clave.EP_O := Client_EP_Handler;			
			Clave.Num := Seq_N;  
			Value.Tipo := chat_messages.Writer;
			Value.Nick := Nick;
			Value.Comentario := Request;
			Hora := Ada.Real_Time.Clock;
			Client_Handler.Maps_Message.Put(Client_Handler.Pending_Msgs, Clave, Value);
			--Añado a la lista de Retransmisiones
			Client_Handler.Maps_Rts.Put(Client_Handler.Retransmission_Times, Hora + Client_Handler.Plazo_Retransmision, Clave);
			if Client_Handler.Maps_Rts.Map_Length(Client_Handler.Retransmission_Times) >= 1 then
				Protected_ops.Program_Timer_Procedure(Procedures.Retransmission_Client'access, 
													  Hora + Client_Handler.Plazo_Retransmision);
			end if;	
		end if;
end Mandar_Mensajes;


	Server_EP : LLU.End_Point_Type;
	Client_EP_Receive: LLU.End_Point_Type;
	Client_EP_Handler: LLU.End_Point_Type;
	Buffer	: aliased LLU.Buffer_Type(1024);
	Ver_Servidor: ASU.Unbounded_String;
	Port	: Integer;
	
	Nick	: ASU.Unbounded_String;
	Text	: ASU.Unbounded_String;
	Tipo : chat_messages.Message_Type;

	Expired: Boolean:=False;
	Success: Boolean := True;
	Min_Delay: Integer;
	Max_Delay: Integer;
	Fault_Pct: Integer;
	Max_Rts : Integer;
	i : Natural := 1;	
	Request	: ASU.Unbounded_String;
	Hora_Msg: Ada.Real_Time.Time;
	Info_Msg: client_handler.Mess_ID;
begin
	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(Ada.Command_Line.Argument(1)));
	Port		:= Integer'Value(Ada.Command_Line.Argument(2));
	Nick		:= ASU.To_Unbounded_String(Ada.Command_Line.Argument(3));
	Min_Delay:=Integer'Value(Ada.Command_Line.Argument(4));
	Max_Delay:=Integer'Value(Ada.Command_Line.Argument(5));
	Fault_Pct:=Integer'Value(Ada.Command_Line.Argument(6));
	Max_Rts := 10 + ((Fault_Pct/100) * (Fault_Pct/100))/10;
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);
	LLU.Bind_Any(Client_EP_Handler,Client_handler.Handler'access);
	LLU.Bind_Any(Client_EP_Receive);
	Client_Handler.Plazo_Retransmision := Ada.Real_Time.To_Time_Span(2 * Duration(Max_Delay) / 1000);
	--Simulación de fallos
	LLU.Set_Faults_Percent (Fault_Pct);
	LLU.Set_Random_Propagation_Delay(Min_Delay,Max_Delay);
	--mensaje init
	Mensaje_Init(Buffer'Access, Client_EP_Receive,Client_EP_Handler,Server_EP,Nick);
	--Recibir welcome
	LLU.Reset(Buffer);
	LLU.Receive(Client_EP_Receive, Buffer'Access, 2*Duration(Max_Delay) / 1000, Expired);
	if Expired then --Mandar init otra vez 
		while Expired and i<=Max_Rts loop
			--RETRANSMISION DEL INIT HASTA RECIBIR WELCOME O SUPERAR RETRANSMISIONES
			Mensaje_Init(Buffer'Access, Client_EP_Receive,Client_EP_Handler,Server_EP,Nick);
			LLU.Reset(Buffer);
  			LLU.Receive(Client_EP_Receive,Buffer'Access, 2*Duration(Max_Delay) /1000, Expired);
			i := i + 1;
		end loop;
	end if;
	if Expired then
		ATI.Put_Line("Server unreachable");		
	else
		Tipo:= chat_messages.Message_Type'Input(Buffer'Access);--Mensaje Welcome
		if Tipo = chat_messages.Welcome then
			if Boolean'Input(Buffer'access)  then
				ATI.Put_Line("Mini-Chat v2.0: Welcome " & ASU.To_String(Nick));
				loop
					Mandar_Mensajes(Buffer'Access,Client_EP_Handler,Server_EP,Nick,Request);
				exit when Request=".quit"; --Comparar con EP recibida en servidor 
				end loop;
				Mensaje_Logout(Buffer'Access,Client_EP_Handler,Server_EP,Nick); 				
			else 
				ATI.Put_Line("Mini-Chat v2.0: IGNORED new user " & ASU.To_String(Nick) & ", nick already used");
			end if;
		else 
			ATI.Put_Line("Recibido mensaje erroneo");
		end if;
	end if;


	Client_Handler.Maps_Rts.Get(Client_Handler.Retransmission_Times,Hora_Msg, Info_Msg, Success);
	
	while Success loop 
		Delay 1.0;
		Client_Handler.Maps_Rts.Get(Client_Handler.Retransmission_Times,Hora_Msg, Info_Msg, Success);
	end loop;
	LLU.Finalize;

exception
   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end chat_client; 
