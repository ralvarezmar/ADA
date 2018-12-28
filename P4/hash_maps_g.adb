with Ada.Text_IO;
with Ada.Unchecked_Deallocation;


package body Hash_Maps_G is


procedure Get (M       : in out Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
	
   i: Hash_Range := Hash(Key);
   j: Hash_Range := 0;
   begin     
      Success := False;
	loop
        if  M(i).Contenido = Delete and j = 0 then
		j := i;
		i := i + 1;
	elsif M(i).Key = Key and M(i).Contenido = Full then
            Value := M(i).Value; 	    
	    Success := True; 
	   	 if j/=0 then         
	   		 M(j) := M(i); --Muevo al primer hueco generado por un delete
	  	 	 M(i).Contenido := Delete;
		end if;
	else 
		i := i + 1;
	end if;         
	exit when Success or M(i).Contenido = Empty or i = Hash(Key);
	end loop;      
   end Get;	

procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is 
     Found : Boolean;
     i:Hash_Range := Hash(Key);
   begin
      -- Si ya existe Key, cambiamos su Value
      Found := False;
      loop
         if M(i).Key = Key and M(i).Contenido = Full then
            M(i).Value := Value;	
            Found := True;
	else 
         i :=  i + 1;
	end if;
	exit when Found or M(i).Contenido = Empty or i = Hash(Key);
	end loop;
      -- Si no hemos encontrado Key añadimos
      i := Hash(Key);
      if not Found then
		loop
		 	if M(i).Contenido = Delete or M(i).Contenido = Empty then
	       		  M(i).Key := Key;
			  M(i).Value := Value;
			  M(i).Contenido := Full;
			  Found:=True;
			 -- Ada.Text_IO.Put_Line ("Añado a" & Hash_Range'Image(i));
                        else
				 i := i+1;
			end if;			
		exit when Found or i > Hash_Range'Last;
		end loop;
	if not Found then
		 raise Full_Map;
	 end if;	
    end if;
end Put;

procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is
    i:Hash_Range;
    begin
      i := Hash(Key);
      Success := False;
	loop	
	 	if M(i).Contenido=Full and M(i).Key = Key then
		  Success := True;
		  M(i).Contenido := Delete;
		 else
		   i := i+1;
		 end if;
	exit when Success or M(i).Contenido = Empty or i = Hash(Key);
      end loop;
   end Delete;

function Map_Length (M : Map) return Natural is
   Elementos: Natural := 0;
	i: Natural := 1 ;   
   begin
	  for i in Hash_Range loop		
		if M(i).Contenido = Full then
			Elementos:=Elementos+1;
		end if;
	end loop;	
      return Elementos;
   end Map_Length;

  function First (M: Map) return Cursor is
   Found: Boolean := False; 
   i: Integer := 0;
    begin
	 loop				
		if M(Hash_Range(i)).Contenido = Full then
			Found := True;
		else 
			i := i + 1;
		end if;	
	exit when Found or Hash_Range(i) = Hash_Range'Last;	
	end loop;
    if Found then
      return (M => M,
		 Element => i);
    else 
      return (M => M,
		 Element => -1);
    end if;
   end First;

   function Last (M:Map) return Cursor is
   Found: Boolean := False; 
   i: Hash_Range := Hash_Range'Last;
    begin
	loop
		if M(i).Contenido = Full then
			Found := True;
		else 
			i := i - 1;
		end if;		
	exit when Found or i = Hash_Range'First;
	end loop;
    if Found then
      return (M => M,
		 Element => Integer(i));
    else 
      return (M => M,
		 Element => -1);
    end if;
   end Last;

  procedure Prev(C: in out Cursor) is  
  i : Integer :=C.Element - 1;
  Found : Boolean := False;
  begin
	loop
		if C.M(Hash_Range(i)).Contenido=Full then 
			Found := True;
		else
			i:= i - 1;
		end if; 
	 exit when Found or Hash_Range(i) = Hash_Range'First;
  	 end loop;
	if Found then
  		C.Element := i;
	else 
		C.Element := -1;
	end if;
  end Prev;

   procedure Next (C: in out Cursor) is --Con contador. Sin el no me mostraba la ultima casilla o se me salia de rango 
  i : Integer := C.Element+1;
  Found : Boolean := False; 
  Cont: Hash_Range := Hash_Range(C.Element);
  begin
	while not Found and Cont /= Hash_Range'Last loop
		if C.M(Hash_Range(i)).Contenido=Full then 
			Found := True; 
		else 
			i:= i + 1;
			Cont := Cont+1;
		end if; 
  	 end loop;	

	if Found then
  		C.Element := i;
	else 
		C.Element := -1;
	end if;
   end Next;

   function Element (C: Cursor) return Element_Type is
   begin
   if not Has_Element(C) then 
	raise No_Element;
   end if;
      if C.M(Hash_Range(C.Element)).Contenido = Full then
         return (Key   => C.M(Hash_Range(C.Element)).Key,
                 Value => C.M(Hash_Range(C.Element)).Value);
      else
         raise No_Element;
      end if;
   end Element;

   function Has_Element (C: Cursor) return Boolean is
   begin
      if C.Element < 0 then
         return False;
      else
         return True;
      end if;
   end Has_Element;


end Hash_Maps_G;
