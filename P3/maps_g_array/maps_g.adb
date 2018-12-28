with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Maps_G is

   procedure Get (M       : Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
      i:Natural;
   begin
      i := 1;
      Success := False;
      while not Success and Max > i loop
         if M(i).Contenido and M(i).Key = Key then
            Value := M(i).Value;
            Success := True;
         end if;
         i := i + 1;
      end loop;
   end Get;


   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is
      Found : Boolean;
      i:Natural;
   begin
      -- Si ya existe Key, cambiamos su Value
      i := 1;
      Found := False;
      while not Found and Max>i loop
         if M(i).Key = Key then
            M(i).Value := Value;
	    M(i).Contenido := True;		
            Found := True;
         end if;
         i :=  i + 1;
      end loop;
      -- Si no hemos encontrado Key añadimos
      i := 1 ;
      if not Found then
	if Map_Length(M) <= Max then
		while not Found and Max>i loop
		 	if not M(i).Contenido then
	       		  M(i).Key := Key;
			  M(i).Value := Value;
			  M(i).Contenido := True;  
			  Found:=True;
                        end if; 
			i := i+1;
		end loop;
	else
		 raise Full_Map;
	 end if;	
    end if;
   end Put;



   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is
    i:Natural;
    begin
      i := 1;
      Success := False;
      while not Success and Max>i loop	
	 	if M(i).Contenido and M(i).Key = Key then
		  Success := True;
		  M(i).Contenido := False;  
		 else
		   i := i+1;
		 end if;
      end loop;
   end Delete;

   function Map_Length (M : Map) return Natural is
   i:Natural := 1 ;
   Elementos: Natural := 0;
   begin
 	while i /= Max loop		
		if M(i).Contenido = True then
			Elementos:=Elementos+1;
		end if;		
		i := i + 1;
	end loop;
      return Elementos;
   end Map_Length;

 --Falta
   function First (M: Map) return Cursor is
   Found: Boolean := False; 
   i:Natural := 1;
    begin
	while not Found and i/=Max loop				
		if M(i).Contenido = True then
			Found := True;
		else 
			i := i + 1;
		end if;		

	end loop;
    if Found then
      return (M => M,
		 Element => i);
    else 
      return (M => M,
		 Element => 0);
    end if;
   end First;

   function Last (M:Map) return Cursor is
   Found: Boolean := False; 
   i:Natural := Max ;
    begin
	while not Found and i>0 loop
		if M(i).Contenido = True then
			Found := True;
		else 
			i := i - 1;
		end if;		
	end loop;
    if Found then
      return (M => M,
		 Element => i);
    else 
      return (M => M,
		 Element => 0);
    end if;
   end Last;

  procedure Prev(C: in out Cursor) is  
  i : Natural := C.Element - 1;
  Found : Boolean := False; 
  begin
	while not Found and i>0 loop
		if C.M(i).Contenido then 
			Found := True; 
		else
			i:= i - 1;
		end if; 
  	 end loop;
	if Found then
  		C.Element := i;
	else 
		C.Element := 0;
	end if;
  end Prev;

   procedure Next (C: in out Cursor) is
  i : Natural := C.Element + 1;
  Found : Boolean := False; 
  begin
	while not Found and i<Max loop
		if C.M(i).Contenido then 
			Found := True; 
		else
			i:= i + 1;
		end if; 
  	 end loop;
	if Found then
  		C.Element := i;
	else 
		C.Element := 0;
	end if;
   end Next;

   function Element (C: Cursor) return Element_Type is
   begin
   if C.Element = 0 then 
	raise No_Element;
   end if;
      if C.M(C.Element).Contenido = True then
         return (Key   => C.M(C.Element).Key,
                 Value => C.M(C.Element).Value);
      else
         raise No_Element;
      end if;
   end Element;

   function Has_Element (C: Cursor) return Boolean is
   begin
      if C.Element /= 0 then
         return True;
      else
         return False;
      end if;
   end Has_Element;

end Maps_G;

