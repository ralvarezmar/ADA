-- $Id: lists_generic-marshalling.ads,v 1.4 1998/01/19 12:46:24 jgb Exp $
--

with Ada.Streams;

generic
--   with procedure Output
--     (Stream: access Ada.Streams.Root_Stream_Type'Class;
--      Item:   in     Element);
--   with function Input
--     (Stream: access Ada.Streams.Root_Stream_Type'Class)
--      return Element;
package Lists_Generic.Marshalling is

   pragma Elaborate_Body;

   procedure Output
     (Stream: access Ada.Streams.Root_Stream_Type'Class;
      A_List: in     List);

   function Input
     (Stream: access Ada.Streams.Root_Stream_Type'Class)
      return List;

   Bad_List: exception;

end Lists_Generic.Marshalling;
