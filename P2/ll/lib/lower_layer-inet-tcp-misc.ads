-- $Id: lower_layer-inet-tcp-misc.ads,v 1.3 1997/03/13 21:43:21 jgb Exp $
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
-- Ancillary services for TCP interface (connection oriented services).
------------------------------------------------------------------------------

with Ada.Streams;

-- Used in private section.
with Ada_Sockets;

package Lower_Layer.Inet.TCP.Misc is

   type Connection is new Ada.Streams.Root_Stream_Type with private;

   procedure Connect (An_Address: in Address'Class;
                      A_Connection: out Connection);

   procedure Dispose (A_Connection: in out Connection);

   procedure Listen_Connection (An_Address: in Address'Class);

   procedure Wait_Connection (An_Address: in Address'Class;
                              A_Connection: out Connection);

   procedure Read
     (A_Connection: in out Connection;
      Data:         out    Ada.Streams.Stream_Element_Array;
      Length:       out    Ada.Streams.Stream_Element_Offset);

   procedure Write
     (A_Connection: in out Connection;
      Data:         in     Ada.Streams.Stream_Element_Array);

   -- Raised when a Wait_Connection is done for a not listenning address.
   Not_Listening: exception;

   -- Attempt to listen on an already "listened" address.
   Already_Listening: exception;

private

   type Connection is new Ada.Streams.Root_Stream_Type with record
      Socket: Ada_Sockets.Socket;
   end record;

end Lower_Layer.Inet.TCP.Misc;
