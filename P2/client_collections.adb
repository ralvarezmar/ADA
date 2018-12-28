--Ruben Alvarez Martin

with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Command_Line;


package body client_collections is 

	use type LLU.End_Point_Type; 
	use type ASU.Unbounded_String;
	package ATI renames Ada.Text_IO;

procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A); 


function Unique_Client (Collection: in Collection_Type;
			Nick: in ASU.Unbounded_String)
			return Boolean is
P_Aux : Cell_A;
Unique_Client:Boolean;
begin 
	Unique_Client:=True;
	P_Aux := Collection.P_First;
	while P_Aux /= null and Unique_Client loop
		if Nick=P_Aux.Nick then
			Unique_Client:=False;
		else 
			P_Aux:= P_Aux.Next;
		end if;
	end loop;
return Unique_Client;  --Devuelve un True si el nick es unico. Si hay otro igual, un false. 
end Unique_Client;



procedure Add_Client (Collection: in out Collection_Type;
                         EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String;
                         Unique: in Boolean)      is

P_Aux:Cell_A;

begin
	P_Aux := Collection.P_First;
	if Unique_Client(Collection, Nick) then 	--writer	
		Collection.P_First:= new Cell'(EP, Nick, P_Aux);
		Collection.Total := Collection.Total + 1;
	elsif not Unique and not Unique_Client(Collection, Nick) then --reader
		Collection.P_First:= new Cell'(EP, Nick, P_Aux);
		Collection.Total := Collection.Total + 1;
	elsif not Unique_Client(Collection, Nick) and Unique then
		raise Client_Collection_Error;
	end if; 
end Add_Client;


procedure Delete_Client (Collection: in out Collection_Type; 
                           Nick: in ASU.Unbounded_String) is

P_Aux : Cell_A;
P_Ant : Cell_A; 
Borrado:Boolean;
begin
	P_Ant := Collection.P_First;
	P_Aux := P_Ant.Next;
	Borrado := False;
	while P_Aux /= null and not Borrado loop 
		if Nick = P_Aux.Nick then
			P_Ant.Next:=P_Aux.Next;
			Free(P_Aux);
			Collection.Total:= Collection.Total-1;				
			Borrado:= True; 
		else
			P_Ant := P_Aux;
			P_Aux := P_Aux.Next;
		end if;
	end loop;
	if not Borrado then
		raise Client_Collection_Error;
	end if;
end Delete_Client;

function Search_Client (Collection: in Collection_Type;
                           EP: in LLU.End_Point_Type)
                           return ASU.Unbounded_String is

Nick : ASU.Unbounded_String;
P_Aux: Cell_A;
Encontrado : Boolean:=False; 
begin

	P_Aux := Collection.P_First; 
	while P_Aux /= null and not Encontrado loop
		if EP = P_Aux.Client_EP then			
			Encontrado:=True;
			Nick:=P_Aux.Nick;
		else 
			P_Aux:=P_Aux.Next;
		end if;
	end loop;
	if Encontrado then
		return Nick;
	else 
		raise Client_Collection_Error;	
	end if;
end Search_Client; 

procedure Send_To_All (Collection: in Collection_Type;
                          P_Buffer: access LLU.Buffer_Type) is
P_Aux : Cell_A;
begin
	P_Aux := Collection.P_First; 
	if Collection.Total /= 0 then
		while P_Aux /= null loop	
			--ATI.Put_Line("Envio a " & LLU.Image(P_Aux.Client_EP)); 
			LLU.Send(P_Aux.Client_EP, P_Buffer);			
			P_Aux:=P_Aux.Next;
		end loop;
	end if;
end Send_To_All;

function Collection_Image (Collection: in Collection_Type)
                             return String is
P_Aux: Cell_A;
Nick: ASU.Unbounded_String;
User_List:ASU.Unbounded_String;
begin
	P_Aux := Collection.P_First;
	while P_Aux /= null loop
		Nick:=ASU.To_Unbounded_String(LLU.Image(P_Aux.Client_EP));
		Nick:=ASU.Tail(Nick,ASU.Index(Nick," "));
		Nick:=P_Aux.Nick & Nick;
		User_List := User_List & ASCII.LF & Nick & ASCII.LF; 
		P_Aux:=P_Aux.Next;
	end loop;
return ASU.To_String(User_List);
end Collection_Image;

end client_collections;
