with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with chat_messages;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Server_handler;
with Hash_Maps_G;
with Ada.Command_Line;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;
with Protected_Ops;
with procedures;

procedure chat_server is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ATI renames Ada.Text_IO;
	use type chat_messages.Message_Type;
	use type ASU.Unbounded_String;


	Server_EP : LLU.End_Point_Type;	
	Ver_Servidor: ASU.Unbounded_String;
	Comentario : ASU.Unbounded_String;
	Nick	: ASU.Unbounded_String;	
	C: 	character;	
	Port	:  Integer;
	Usage_Error	: exception;
	Min_Delay:Integer:=Integer'Value(Ada.Command_Line.Argument(3));
	Max_Delay:Integer:=Integer'Value(Ada.Command_Line.Argument(4));
	Fault_Pct:Integer:=Integer'Value(Ada.Command_Line.Argument(5));
begin


	Ver_Servidor	:= ASU.To_Unbounded_String (LLU.To_IP(LLU.Get_Host_Name)); 
	Port		:= Integer'Value(Ada.Command_Line.Argument(1));
	Server_EP 	:= LLU.Build(ASU.To_String(Ver_Servidor), Port);
	LLU.Bind (Server_EP,Server_Handler.Handler'access);
	Ada.Text_IO.Put_Line("Atado en EP: " & LLU.Image(Server_EP));


	LLU.Set_Faults_Percent (Fault_Pct);--Simulación de fallos
	LLU.Set_Random_Propagation_Delay(Min_Delay,Max_Delay);
	
	if Ada.Command_Line.Argument_Count /= 5 then
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
				Protected_Ops.Protected_Call(Procedures.Print_Active'access);
			elsif C = 'L'then
				--Clientes activos por orden inverso
				Ada.Text_IO.Put_Line ("ACTIVE CLIENTS");
				Ada.Text_IO.Put_Line ("============");
				Protected_Ops.Protected_Call(Procedures.Print_Active_Inv'access); --Pasar el procedimiento a los procedures
			elsif C = 'O' then
				--Clientes antiguos inversamente al orden
				Ada.Text_IO.Put_Line ("OLD CLIENTS");
				Ada.Text_IO.Put_Line ("===========");
				Protected_Ops.Protected_Call(Procedures.Print_Old_Inv'access);
			elsif C= 'o' then
				--Clientes antiguos(primero muestra el primero que salió)
				Ada.Text_IO.Put_Line ("OLD CLIENTS");
				Ada.Text_IO.Put_Line ("===========");
				Protected_Ops.Protected_Call(Procedures.Print_Old'access);
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


