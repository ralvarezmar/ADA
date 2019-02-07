--Rub�n �lvarez Mart�n

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Word_Lists;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Strings.Maps;

procedure Words is
	package ASU renames Ada.Strings.Unbounded;
	package TI_A renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	use type ASU.Unbounded_String;
	Usage_Error	: exception;

	File_Name		: ASU.Unbounded_String;
	File			: Ada.Text_IO.File_Type;
	Finish			: Boolean;
	Line			: ASU.Unbounded_String;
	Palabra			: ASU.Unbounded_String;
	Count 			: Integer;
	Lista_Palabras		: Word_Lists.Word_List_Type;

procedure Cortar(Line: in out ASU.Unbounded_String;
		Lista_Palabras :out Word_Lists.Word_List_Type) is
	Numero: Integer;
	Cerrar: Boolean := False;
	begin
		while not Cerrar loop
			Numero:= ASU.Index(Line, Ada.strings.Maps.To_set(" ,.-:/;"));
			if Numero > 1 then
				Palabra:= ASU.Head(Line,Numero-1);
				Word_Lists.Add_Word(Lista_Palabras, Palabra);
				Line:= ASU.Tail(Line, ASU.Length(Line) - Numero);
			elsif Numero = 1 then   	--se encuentra un espacio
				Line:= ASU.Tail(Line, ASU.Length(Line) - 1);
			elsif Numero < 1 then 	--solo pasa al final de linea
				Palabra := Line;
				Word_Lists.Add_Word(Lista_Palabras, Palabra);
				Cerrar := True;
			end if;
		end loop;
end Cortar;


procedure Leer_Fichero(File_Name: in ASU.Unbounded_String)  is
	begin
		Ada.Text_IO.Open(File, Ada.Text_IO.In_File, ASU.To_String(File_Name));
		while not Finish loop
			begin
				Line := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line(File));
				Cortar(Line, Lista_Palabras);  	-- Trocear y contar en esta linea
				exception
					when Ada.IO_Exceptions.End_Error =>
					Finish := True;
			end;
		end loop;
	Ada.Text_IO.Close(File);
end Leer_Fichero;


procedure Menu (Lista_Palabras : in out Word_Lists.Word_List_Type) is

	Opcion : Integer;
	Cerrar : Boolean := False;
	begin
	while not Cerrar loop
		TI_A.Put_Line("Opciones");
		TI_A.Put_Line("1 Anadir palabra");
		TI_A.Put_Line("2 Borrar palabra");
		TI_A.Put_Line("3 Buscar palabra");
		TI_A.Put_Line("4 Mostrar todas las palabras");
		TI_A.Put_Line("5 Salir");
		TI_A.Put("Que opcion elige? ");
		Opcion := Integer'Value(TI_A.Get_Line);
		case Opcion is
			when 1 =>
				TI_A.Put_Line("�Que palabra desea anadir?");
				Palabra := ASU.To_Unbounded_String(TI_A.Get_Line);
				Word_Lists.Add_Word(Lista_Palabras, Palabra);
				TI_A.Put_Line("Palabra anadida");
			when 2 =>
				TI_A.Put_Line("Que palabra desea borrar?");
				Palabra := ASU.To_Unbounded_String(TI_A.Get_Line);
				Word_Lists.Delete_Word(Lista_Palabras, Palabra);
				TI_A.Put_Line("Palabra borrada");
			when 3 =>
				TI_A.Put_Line("�Que palabra desea buscar?");
				Palabra := ASU.To_Unbounded_String(TI_A.Get_Line);
				Word_Lists.Search_Word(Lista_Palabras, Palabra, Count);
				Ada.Text_Io.Put_Line("Aparece " & Integer'Image(Count) & " veces ");
			when 4 =>
				Word_Lists.Print_All(Lista_Palabras);
			when 5 =>
				Word_Lists.Max_Word(Lista_Palabras, Palabra, Count);
				Cerrar := True;
			when others =>
				TI_A.Put_Line("Elija una opcion de la lista");
			end case;
			TI_A.Put_Line("");
		end loop;
end Menu;

begin

		if ACL.Argument_Count < 1 or ACL.Argument_Count > 3 then
			raise Usage_Error;
		end if;

	if ACL.Argument_Count = 1 then
		File_Name := ASU.To_Unbounded_String(ACL.Argument(1));
		Leer_Fichero(File_Name);
		Word_Lists.Max_Word(Lista_Palabras, Palabra, Count);
	elsif ACL.Argument_Count = 2 then
		File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
		Leer_Fichero(File_Name);
		if ACL.Argument(1) = "-l" then
			Word_Lists.Add_Word(Lista_Palabras, Palabra);
			Word_Lists.Print_All(Lista_Palabras);
			Word_Lists.Max_Word(Lista_Palabras, Palabra, Count);
		elsif ACL.Argument(1) = "-i" then
			Word_Lists.Add_Word(Lista_Palabras, Palabra);
			Menu(Lista_Palabras);
		end if;
	elsif ACL.Argument_Count = 3 then
		File_Name := ASU.To_Unbounded_String(ACL.Argument(3));
		Leer_Fichero(File_Name);
		Word_Lists.Add_Word(Lista_Palabras, Palabra);
		Menu (Lista_Palabras);
	end if;

	exception
		when Usage_Error =>
			TI_A.Put_Line("Use: ");
			TI_A.Put_Line("    " & ACL.Command_Name & " <file>");
		when Ada.IO_Exceptions.Name_error =>
			TI_A.Put_Line("Introduzca un fichero valido");
end Words;
