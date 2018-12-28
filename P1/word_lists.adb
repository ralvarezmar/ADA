--Rubén Álvarez 

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;


package body word_lists is 
use type ASU.Unbounded_String;

procedure Free is new Ada.Unchecked_Deallocation(Cell, Word_List_Type); 

	
procedure Print_All (List: in Word_List_Type) is 
	P_Aux : Word_List_Type; 		
	begin		
	P_Aux := List; 
		loop 
			Ada.Text_IO.Put("|" & ASU.To_String(P_Aux.Word) & "| - "); 
			Ada.Text_IO.Put_Line(Integer'Image(P_Aux.Count));
			P_Aux := P_Aux.Next; 		--No me dejaba hacerlo con list al ser un in
		exit when P_Aux.Next = null;
		end loop;
end Print_All;


procedure Add_Word (List: in out Word_List_Type;
		    Word: in ASU.Unbounded_String) is 

	P_Aux : Word_List_Type; 
	Palabra_Anadida : Boolean := False; 
	P_Help: Word_List_Type; 	
	begin 
	P_Aux := List; 
	P_Help:= null;
	if List = null then 
		List:= new Cell'(Word, 1, null);
		Palabra_Anadida:= True;	
	end if; 
	while P_Aux /= null and not Palabra_Anadida loop 
			if Word = P_Aux.Word then
				P_Aux.Count:= P_Aux.Count + 1;
				Palabra_Anadida:= True;		
			else 
				P_Help:= P_Aux; 
				P_Aux := P_Aux.next;					
			end if;			
	end loop;
	if not Palabra_Anadida then		
		P_Help.Next:= new Cell'(Word, 1, null); 
	end if;		
end Add_Word; 


procedure Delete_Word (List: in out Word_List_Type;
				Word: in ASU.Unbounded_String) is

	P_Aux : Word_List_Type;	
	P_Help: Word_List_Type;
	Palabra_Borrada: Boolean := False; 
	begin
	P_Aux := List;
	P_Help:= null;
	while not Palabra_Borrada or P_Aux /= null loop
		if Word = P_Aux.Word then
			P_Help.Next := P_Aux.Next; 
			Free(P_Aux);
			Palabra_Borrada := True;
		else 
			P_Help := P_Aux;
			P_Aux  := P_Aux.Next;
		end if;
	end loop;
end Delete_word; 

procedure Search_Word (List: in Word_List_Type;
			Word: in ASU.Unbounded_String;
			Count: out Natural) is
	
	P_Aux : Word_List_Type;
	Palabra_Encontrada : Boolean := False;
	begin
	P_Aux := List;
		while not Palabra_Encontrada or P_Aux.next = null loop 
			if Word = P_Aux.Word then
				Count := P_Aux.Count;						
				Palabra_Encontrada := True;
			else
				P_Aux := P_Aux.Next;
			end if;
		end loop;

end Search_Word;

procedure Max_Word (List: in Word_List_Type;
			Word: out ASU.Unbounded_String;
			Count: out Natural) is
	
	P_Aux : Word_List_Type;
	P_Busca : Word_List_Type;
	begin 
	P_Aux := List;
	P_Busca := List;
	if P_Aux /= null then
		Count := P_Aux.Count;
		Word := P_Aux.Word;
	end if;
 
	while P_Aux /= null loop  --se sale cuando P_aux apunta a null(ha recorrido toda la lista)
		if P_Aux.Count > P_Busca.Count then
			Word := P_Aux.Word; 
			Count:= P_Aux.Count;
			P_Busca := P_Aux;
			P_Aux := P_Aux.Next;  
		else 
			P_Aux := P_Aux.Next; 
		end if;
	end loop; 
	Ada.Text_IO.Put("La palabra mas frecuente es: | " & ASU.To_String(Word) & " |");
	Ada.Text_IO.Put_Line(Integer'Image(Count));

end Max_Word; 
		
end Word_Lists;
