-- $Id: lists_generic-marshalling.adb,v 1.3 1996/11/10 20:27:32 jgb Exp $
--

with Ada.Tags;

with Ada.IO_Exceptions;

package body Lists_Generic.Marshalling is

   procedure Output
     (Stream: access Ada.Streams.Root_Stream_Type'Class;
      A_List: in     List) is

      A_Ref:      Reference := First (A_List);
   begin
      Natural'Output (Stream, Length (A_List));
      while A_Ref /= Null_Reference loop
         Element'Output (Stream, Get_Element (A_List, A_Ref));
         Forward (A_List, A_Ref);
      end loop;
   end Output;

   function Input
     (Stream: access Ada.Streams.Root_Stream_Type'Class)
      return List is

      List_Length: Natural;
      A_List:      List;
   begin
      List_Length := Natural'Input (Stream);
      for Count in 1 .. List_Length loop
         Insert_Tail (A_List, Element'Input (Stream));
      end loop;
      return A_List;
   exception
      when Ada.Tags.Tag_Error | Ada.IO_Exceptions.Data_Error =>
         raise Bad_List;
   end Input;

end Lists_Generic.Marshalling;
