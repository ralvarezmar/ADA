with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Real_Time;
package body Maps_G is

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);


   procedure Get (M       : Map;
		  Key	  : out Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is --Quitar todas las keys de este procedure en los paquetes principales 
      P_Aux : Cell_A;
   begin
      if M.Length > 0 then --Solo tenemos que devolver el primer elemento
	       P_Aux := M.P_First;
		Value := P_Aux.Value;
		Key := P_Aux.Key;
		Success := True;
      else
	Success := False; 
      end if;
   end Get;


  procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is
      P_Aux : Cell_A;
   begin
	P_Aux := M.P_Last;     
	if M.Length /= 0 then
	 	M.P_Last.Next := new Cell'(Key, Value, null,M.P_Last);
		M.P_Last := M.P_Last.Next;
		M.Length := M.Length + 1;		
		--P_Aux.Prev:= M.P_First;
	else 
		M.P_First := new Cell'(Key, Value, null,null); 
		M.P_Last := M.P_First;
		M.Length := M.Length + 1;
	 end if;
	 if M.Length>Max then
	 	raise Full_Map;
	 end if;
   end Put;


  procedure Delete (M      : in out Map;
                    --Key     : in  Key_Type;
                   Success : out Boolean) is
  P_Current  : Cell_A;
 begin
     P_Current  := M.P_First;
	--if P_Current.Key = Key then --No necesario ya que solo necesito borrar el primero de la lista 
		    M.Length := M.Length - 1;
		    if M.Length > 0 then
			    if M.P_First = P_Current then --Borro el primero de la lista
				M.P_First := M.P_First.Next;
				M.P_First.Prev:=null;	
			    end if;		    
		    elsif M.Length = 0 then --Solo un elemento
			M.P_Last := null;
			M.P_First := null;
		    end if;
		    Free (P_Current);
		    Success := True;
	--end if;
   end Delete;

  function Map_Length (M : Map) return Natural is
   begin	 
      return M.Length;
   end Map_Length;

 
   function First (M: Map) return Key_Type is
   begin
      return (M.P_First.Key);
   end First;

   function Last (M:Map) return Key_Type is
   begin
     return (M.P_Last.Key);
   end Last;


end Maps_G;

