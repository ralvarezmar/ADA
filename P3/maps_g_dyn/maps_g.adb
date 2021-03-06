with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Maps_G is

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);


   procedure Get (M       : Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
      P_Aux : Cell_A;
   begin
      P_Aux := M.P_First;
      Success := False;
      while not Success and P_Aux /= null Loop
         if P_Aux.Key = Key then
            Value := P_Aux.Value;
            Success := True;
         end if;
         P_Aux := P_Aux.Next;
      end loop;
   end Get;


  procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is
      P_Aux : Cell_A;
      Found : Boolean;
   begin
      -- Si ya existe Key, cambiamos su Value
      P_Aux := M.P_First;
      Found := False;
      while not Found and P_Aux /= null loop
         if P_Aux.Key = Key then
            P_Aux.Value := Value;
            Found := True;
         end if;
         P_Aux := P_Aux.Next;
      end loop;
      -- Si no hemos encontrado Key aÃ±adimos al principio
	P_Aux := M.P_First;
      if not Found then
	if M.Length /= 0 then
	 	M.P_First := new Cell'(Key, Value, P_Aux, null);--aÃ±adido null
		M.Length := M.Length + 1;
		P_Aux.Prev:= M.P_First;

	else 
		M.P_First := new Cell'(Key, Value, null,null); 
		M.P_Last := M.P_First;
		M.Length := M.Length + 1;
	 end if;
       end if;
	 if M.Length>Max then
	 	raise Full_Map;
	 end if;
   end Put;


  procedure Delete (M      : in out Map;
                    Key     : in  Key_Type;
                   Success : out Boolean) is
  P_Current  : Cell_A;
 begin
  Success := False;
     P_Current  := M.P_First;
     while not Success and P_Current /= null  loop
		if P_Current.Key = Key then
		    Success := True;
		    M.Length := M.Length - 1;
		    if M.Length > 0 then
			    if M.P_First = P_Current then --Borro el primero de la lista
				M.P_First := M.P_First.Next;
				M.P_First.Prev:=null;			   
			    elsif M.P_Last = P_Current then  --Ultimo de la lista
				P_Current.Prev.Next:= null;
				M.P_Last:= P_Current.Prev;
			    else --Cualquiera
			      	P_Current.Prev.Next := P_Current.Next;
			        P_Current.Next.Prev:=P_Current.Prev;
			    end if;			 
		    elsif M.Length= 0 then --Solo un elemento
			M.P_Last := null;
			M.P_First := null;
		    end if;
		    Free (P_Current);
		 else		   
		    P_Current := P_Current.Next;
		 end if;
  end loop;
   end Delete;

  function Map_Length (M : Map) return Natural is
   begin
      return M.Length;
   end Map_Length;

 
   function First (M: Map) return Cursor is
   begin
      return (M => M, Element_A => M.P_First);
   end First;

   function Last (M:Map) return Cursor is
   begin
     return (M => M, Element_A => M.P_Last);
   end Last;

  procedure Prev(C: in out Cursor) is
  begin
      if C.Element_A /= null Then
         C.Element_A := C.Element_A.Prev;
     end if;
  end Prev;


   procedure Next (C: in out Cursor) is
   begin
      if C.Element_A /= null Then
         C.Element_A := C.Element_A.Next;
      end if;
   end Next;

   function Element (C: Cursor) return Element_Type is
   begin
      if C.Element_A /= null then
         return (Key   => C.Element_A.Key,
                 Value => C.Element_A.Value);
      else
         raise No_Element;
      end if;
   end Element;

   function Has_Element (C: Cursor) return Boolean is
   begin
      if C.Element_A /= null then
         return True;
      else
         return False;
      end if;
   end Has_Element;

end Maps_G;

