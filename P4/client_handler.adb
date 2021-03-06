with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Client_Handler;
with chat_messages;
with Ada.Calendar;
with Hash_Maps_G;
with Maps_G;
with protected_ops;

package body Client_handler is

	use type LLU.End_Point_Type;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;

----------------FUNCION COMPARAR-----------------------------------------
function Comparar(Variable1:Mess_ID; Variable2:Mess_ID) return Boolean is
begin 
if Variable1.EP_D = Variable2.EP_D and Variable1.EP_O = Variable2.EP_O and Variable1.Num = Variable2.Num then
	return True;
else
	return False; 
end if;

end Comparar;
----------------------------FUNCION HASH-----------------------------------------
function Hash_EP(K:Mess_ID) return Hash_Range is 
EP: ASU.Unbounded_String;
Hash: Hash_Range:=0;
begin
	
	EP := ASU.To_Unbounded_String(LLU.Image(K.EP_D) & LLU.Image(K.EP_O));
	for i in 1..ASU.Length(EP) loop
		Hash := Hash_Range'Mod(character'pos(ASU.To_String(EP)(i)) + Integer(Hash));
	end loop;
	Hash := Hash_Range'Mod(Integer(Hash) + Integer(K.Num));
return Hash;
end Hash_EP;
--------------------------------------------------------------------


procedure Enviar_ACK (P_Buffer: access LLU.Buffer_Type;
			Client_EP: in LLU.End_Point_Type;
			Seq_N: in Seq_N_T;
			Server_EP: in LLU.End_Point_Type) is 
begin	
	LLU.Reset(P_Buffer.all);
	chat_messages.Message_Type'Output(P_Buffer, chat_messages.ACK);
	LLU.End_Point_Type'Output(P_Buffer,Client_EP); --EP_H_ACKer
	Seq_N_T'Output(P_Buffer, Seq_N);
	LLU.Send(Server_EP,P_Buffer);
end Enviar_ACK;

----------------------------HANDLER---------------------------------------
procedure Handler (From    : in     LLU.End_Point_Type;
                             To      : in     LLU.End_Point_Type;
                             P_Buffer: access LLU.Buffer_Type) is

	Nick	: ASU.Unbounded_String;
	Text	: ASU.Unbounded_String;
	Tipo: chat_messages.Message_Type;
	N_Sec : Seq_N_T;
	Cursor: Maps_Message.Cursor:= Maps_Message.First(Pending_Msgs);	
	Server_EP_Handler : LLU.End_Point_Type;
	Success: Boolean;
	Ack_R: Mess_ID;
	Message_Info: Mensaje;
	Max_Delay: Integer := Integer'Value(Ada.Command_Line.Argument(4));
   begin
		--Max_Rts 
		--Plazo_Retransmision := 
		Tipo:= chat_messages.Message_Type'Input(P_Buffer);
		if Tipo=chat_messages.Server then --Revisar
			Server_EP_Handler:=LLU.End_Point_Type'Input(P_Buffer);
			N_Sec:= Seq_N_T'Input(P_Buffer);
			Nick:= ASU.Unbounded_String'Input(P_Buffer);
			Text := ASU.Unbounded_String'Input(P_Buffer);
			--ATI.Put_Line("Numero de secuencia recibido: " & Seq_N_T'Image(N_Sec));
			--ATI.Put_Line("Numero de secuencia esperado: " & Seq_N_T'Image(Last_Sec + 1));			
			if N_Sec >= (Last_Sec + 1) then 
				if (Last_Sec + 1) = N_Sec then
					ATI.New_Line;
					ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Text));	
					ATI.Put(">> ");
					Last_Sec := Last_Sec+1;
					Enviar_ACK(P_Buffer,To,N_Sec,Server_EP_Handler);
				end if;
			else 
				Enviar_ACK(P_Buffer,To,N_Sec,Server_EP_Handler);
			end if;
		elsif Tipo=chat_messages.ACK then --Recibo ACK del server
			Ack_R.EP_O := To;
			ACK_R.EP_D := LLU.End_Point_Type'Input(P_Buffer);
			Ack_R.Num:= Seq_N_T'Input(P_Buffer);
			--ATI.Put_Line("EP destino: " & LLU.Image(Ack_R.EP_D));
			--ATI.Put_Line("EP origen: " & LLU.Image(Ack_R.EP_O));
			--ATI.Put_Line("Numero de secuencia: " & Seq_N_T'Image(ACK_R.Num));
			Maps_Message.Get(Pending_Msgs, Ack_R, Message_Info, Success);
			if Success then --Borro mensaje al recibirlo
				Maps_Message.Delete(Pending_Msgs, Ack_R, Success);
			end if;
		end if;
 
end Handler; 
end Client_Handler;
