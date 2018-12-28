-- $Id: lower_layer.ads,v 1.16 1998/02/10 20:20:16 jgb Exp $
--
------------------------------------------------------------------------------
--                                                                          --
--    Copyright (C) 1997  Jesus M. Gonzalez Barahona                        --
--                                                                          --
--    This library is free software; you can redistribute it and/or         --
--    modify it under the terms of the GNU Library General Public           --
--    License as published by the Free Software Foundation; either          --
--    version 2 of the License, or (at your option) any later version.      --
--                                                                          --
--    This library is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     --
--    Library General Public License for more details.                      --
--                                                                          --
--    You should have received a copy of the GNU Library General Public     --
--    License along with this library; see file COPYING.LIB. If not,        --
--    write to the Free Software Foundation, Inc., 675 Mass Ave,            --
--    Cambridge, MA 02139, USA.                                             --
--                                                                          --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- This package provides both a Stream type (which is fundamentally
--  a buffer) and an Address type.
-- Around the type Address are defined some more types (Access to
--  the class-wide type, array of addresses) and subprograms for
--  binding, unbinding, snding and receiving. For all these, both
--  a version primitive of Address and one for Address_CA are offered.
-- Binding can be done in two ways: the simple one, intended for
--  receiving messages with Receive, and another one which registers a
--  handler for receiving.
------------------------------------------------------------------------------

-- For using (abstract) type Stream.
with Ada.Streams;

package Lower_Layer is

   -------------------------------------------------------------------
   --
   -- Stream type and friends.
   --
   -- The type Stream is parameterized with Length, which specifies
   --  the length of the buffer used. But the actual part of the
   --  buffer used could be any portion of it.
   -- The type Stream is designed to be used by attributes Read and
   --  Write of a type. Several writes and reads can be performed on
   --  the same stream, but it is convenient that all the writes are
   --  done first, and reads later. If after some write-reads the
   --  Stream is to be used again, subprogram Reset can be issued,
   --  leaving the Stream empty, just like after declaring it.
   --
   -------------------------------------------------------------------

   type Stream (Length: Ada.Streams.Stream_Element_Offset) is
     new Ada.Streams.Root_Stream_Type with private;

   -- Resets a Stream (reinitializes it).
   --  If Lenght <> 0 then set the stream length ot that value.
   procedure Reset
     (A_Stream: in out Stream;
      Length:   in     Ada.Streams.Stream_Element_Offset := 0);


   -- Reads some elements from the Stream (will be called by attributes
   --  Read and Input of a tagged type using this Stream).
   --  Updates fields Data, Used and First of the Stream.
   --
   procedure Read
     (A_Stream : in out Stream;
      Data     :    out Ada.Streams.Stream_Element_Array;
      Length   :    out Ada.Streams.Stream_Element_Offset);


   -- Writes some elements to the Stream ((will be called by attributes
   --  Write and Output of a tagged type using this Stream).
   --  Updates fields Data, Used and First of the Stream.
   --
   procedure Write
     (A_Stream: in out Stream;
      Data:     in     Ada.Streams.Stream_Element_Array);


   -- Gets the contents of a stream. Doesn't update fields
   --   Data, Used and First of the Stream.
   --
   function Get_All (A_Stream : in Stream)
                     return Ada.Streams.Stream_Element_Array;

   -- For copying streams.
   --
   procedure Copy (Destination: access Stream;
                   Source:      access Stream);

   -- For getting a printable string.
   --
   function Image (A_Stream: in Stream) return String;

   type Stream_A is access all Stream;

   -------------------------------------------------------------------
   --
   -- Tagged types Address and Address_CA.
   --
   -------------------------------------------------------------------

   type Address is abstract tagged private;

   type Address_CA is access all Address'Class;

   type Handler_A is access procedure (From: in Address_CA;
                                       To:   in     Address_CA;
                                       Data: access Stream);

   function Image (An_Address: in Address) return String;

   procedure Bind (An_Address: in Address;
                   Handler:    in Handler_A := null) is abstract;

   procedure Bind_Any (An_Address: out Address;
                       Handler:    in Handler_A := null) is abstract;

   procedure Unbind (An_Address: in Address) is abstract;

   -- !!!: First and Length are used for flexibility (any
   --      slice of an array can be sent.
   --
   procedure Send
     (To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1)
      is abstract;

   procedure Send_From
     (From:     in     Address;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in     Ada.Streams.Stream_Element_Offset;
      First:  in     Ada.Streams.Stream_Element_Offset := 1)
      is abstract;


   -- !!!: Length is needed (we don't know how many elements
   --      will be received).
   --
   procedure Receive
     (From: out Address_CA;
      To:     in     Address;
      Data:   access Ada.Streams.Stream_Element_Array;
      Length: in out Ada.Streams.Stream_Element_Offset)
      is abstract;

   type Address_Array is array (Positive range <>) of Address_CA;

   type Address_Array_A is access Address_Array;

   procedure Bind (An_Address: in Address_CA;
                   Handler:    in Handler_A := null);

   procedure Bind_Any (An_Address: in Address_CA;
                       Handler:    in Handler_A := null);

   procedure Unbind (An_Address: in Address_CA);

   procedure Send (To:   in     Address_CA;
                   Data: access Stream);

   procedure Send (From:   in     Address_CA;
                   To:   in     Address_CA;
                   Data: access Stream);

   procedure Receive (From:   out     Address_CA;
                      To:   in     Address_CA;
                      Data: access Stream);

   function ">=" (Left, Right: Address) return Boolean is abstract;

   -- For comparing (and ordering) addresses.
   --
   function "=" (Left, Right: Address_CA) return Boolean;

   function ">=" (Left, Right: Address_CA) return Boolean;

   function Get_New (An_Address: in Address_CA) return Address_CA;

   function Is_Null (An_Address: in Address_CA) return Boolean;

   procedure Free (An_Address: in out Address_CA);

   function Image (An_Address: in Address_CA) return String;

   ------------------------------------------------------
   -- Exceptions.
   ------------------------------------------------------

   -- Error while binding. This is probably an internal error,
   --  due to some bug, or a misfunction of the lower layer.
   --  Could also be a non valid address specified for binding.
   --
   Binding_Error:   exception;

   -- Error while unbinding. This is probably an internal error,
   --  due to some bug, or a misfunction of the lower layer.
   --  Could also be a non valid address specified for unbinding
   --  (e.g., an unbound address).
   --
   Unbinding_Error: exception;

   -- Error sending.
   --
   Sending_Error:   exception;

   -- Error receiving.
   --
   Receiving_Error: exception;

   -- An operation which requires binding was issued on an unbound
   --  address.
   --
   Unbound_Address: exception;

   -- Internal error, probably due to a bug.
   --
   Internal_Error: exception;

   -- Address_CA was null.
   --
   Null_Address: exception;

   -- Incorrect format for an address.
   --
   Bad_Address: exception;

   -- Forbidden call to subprogram (usually, the call doesn't
   --  make sense.
   --
   Forbidden_Call: exception;

   -- Stream with not enough free elements for a Write operation
   --
   Stream_Overflow: exception;




private

   type Stream (Length: Ada.Streams.Stream_Element_Offset) is
     new Ada.Streams.Root_Stream_Type with
      record
         -- Current (used) length of the buffer.
         Current_Length: Ada.Streams.Stream_Element_Offset := 0;
         -- First is the first element of the used portion of the buffer.
         --  If the buffer is empty, its value is 0.
         First: Ada.Streams.Stream_Element_Offset := 1;
         -- Data is the buffer associated with the Stream.
         Data:  aliased Ada.Streams.Stream_Element_Array (1 .. Length);
      end record;

   type Address is abstract tagged null record;


   procedure Write
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     in     Address_CA);

   for Address_CA'Write use Write;

   procedure Read
     (A_Stream: access Ada.Streams.Root_Stream_Type'Class;
      Item:     out    Address_CA);

   for Address_CA'Read use Read;

   -- Send_No_Faults (never injects faults).
   --
   procedure Send_No_Faults (To:   in     Address_CA;
                             Data: access Stream);

end Lower_Layer;
