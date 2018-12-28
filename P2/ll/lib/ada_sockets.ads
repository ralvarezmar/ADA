-- $Id: ada_sockets.ads,v 1.14 1998/01/19 12:46:42 jgb Exp $
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
-- Thick interface to BSD sockets (UDP only)
------------------------------------------------------------------------------

with Interfaces.C;

with C_Sockets;

-- For managing dinamic storage.
with Unchecked_Deallocation;

package Ada_Sockets is

   pragma Elaborate_Body;

   package C renames Interfaces.C;

   ----------------------------------------------------------------------
   --
   -- Identifiers for Ports and Machines (network order).
   -- !!!: Implementation of Host_No and Port_No is left visible
   --      so that it can be used without using dummy conversion
   --      functions.
   ----------------------------------------------------------------------

   -- Hosts.
   --
   type Host_No is new C_Sockets.In_addr;

   -- Sometimes we need to name a `void' host.
   --
   Void_Host: constant Host_No;

   -- Exception: Bad_Host_No.
   --
   function To_Host (Host: in String) return Host_No;

   function To_String (Host: in Host_No) return String;

   -- Comparing two host numbers (network order).
   --
   function ">=" (Left, Right: in Host_No) return Boolean;

   function Get_Addr_By_Name (Name: in String) return Host_No;

   function Get_Host_Name return String;

   -- Ports.
   --
   type Port_No is new C.unsigned_short;

   -- Sometimes we need to name a `void' (any) port.
   --
--   Void_Port: constant Port_No;

   -- From an Integer (host order) to a Port_No
   function To_Port (Port: in Integer) return Port_No;

   -- From a Port_No to an Integer (host order)
   function To_Integer (Port: in Port_No) return Integer;

   -- Comparing two port numbers (network order).
   --
   function ">=" (Left, Right: in Port_No) return Boolean;

   ----------------------------------------------------------------------
   --
   -- Sockets.
   -- !!!: Socket implementation is visible so that users of this
   --      package can use it with subprograms in C_Sockets.
   ----------------------------------------------------------------------

   type Socket is new C.Int;

   type Kind is (TCP, UDP);

   --
   -- Subprograms both for UDP and TCP.
   --
   procedure Create_Socket (A_Kind:   in  Kind;
                            A_Socket: out Socket);

   procedure Destroy_Socket (A_Socket: in Socket);

   procedure Bind_Socket (A_Socket: in Socket;
                          A_Host:   in Host_No;
                          A_Port:   in Port_No);

   procedure Bind_Socket_Any (A_Socket: in  Socket;
                              A_Host:   in  Host_No;
                              A_Port:   out Port_No);

   --
   -- Subprograms only for TCP.
   --

   procedure Connect_Socket (A_Socket: in Socket;
                             A_Host:   in Host_No;
                             A_Port:   in Port_No);

   procedure Listen_Socket (Listen_Socket: in Socket);

   procedure Accept_Socket (Listen_Socket:   in  Socket;
                            Accepted_Socket: out Socket);

   -- Let's get a string from a socket.
   --
   function Image (A_Socket: in Socket) return String;

   -- Type for naming sets of sockets.
   --
   type Socket_Array is array (Positive range <>) of Socket;

   function Image (An_Array: in Socket_Array) return String;

   -- Procedure for selecting ready sockets (for reading)
   --
   function Ready_Socket (Set:     in Socket_Array;
                          Timeout: in Duration := Duration (0))
                          return Socket_Array;

   type Socket_Array_A is access Socket_Array;

   -----------------------------------------------------------------------
   -- Free (Socket_Array_A).
   -----------------------------------------------------------------------

   procedure Free is new
     Unchecked_Deallocation (Object => Socket_Array,
                             Name   => Socket_Array_A);

   function Image (Array_A: in Socket_Array_A) return String;

   ----------------------------------------------------------------------
   --
   -- Socket options.
   --
   ----------------------------------------------------------------------

   -- The root of the hierarchy
   type Option is abstract tagged null record;

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option) is abstract;

   -- Type for togle boolean options.
   type Togle is (Set, Unset);

   -- Type for options at socket level.
   --
   type Option_S is abstract new Option with null record;

   -- Type for boolean options at socket level (and operations).
   --
   type Option_S_Bool is new Option_S with private;

   -- Type for names of boolean options at socket level
   type Option_S_Bool_Name is (Reuse_Port);

   function Build_Option (Flag: in Togle;
                          Name: in Option_S_Bool_Name)
                          return Option_S_Bool;

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_S_Bool);

   -- Type for linger options at socket level (and operations).
   --
   type Option_S_Linger is new Option_S with private;

   function Build_Option (Flag:   in Togle;
                          Linger: in Integer)
                          return Option_S_Linger;

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_S_Linger);

   -- Type for options at IP level.
   --
   type Option_IP is abstract new Option with null record;

   -- Type for multicast requests at IP level
   type Option_IP_Multi is new Option_IP with private;

   -- Type for names of options at IP level (using Mreq)
   type Option_IP_Multi_Name is (Add_Membership, Drop_Membership);

   function Build_Option (Name: in Option_IP_Multi_Name;
                          Addr: in Host_No)
                          return Option_IP_Multi;

   procedure Set_Option (A_Socket:  in Socket;
                         An_Option: in Option_IP_Multi);

   ----------------------------------------------------------------------
   --
   -- Exceptions
   --
   ----------------------------------------------------------------------

   -- Error in Socket call (creating socket).
   --
   Socket_Failed: exception;

   -- Error in Conenct call (trying to connect with a receiver socket).
   --
   Connect_Failed: exception;

   -- Error in Listen call.
   --
   Listen_Failed: exception;

   -- Error in accept call.
   --
   Accept_Failed: exception;

   -- Error in Select call (or in its set up).
   --
   Select_Failed: exception;

   -- Empty array of sockets.
   --
   Socket_Array_Empty: exception;

   -- Timeout expired.
   --
   Timeout_Expired: exception;

   -- No address found for specified host name.
   -- Raised by Get_Addr_By_Name.
   --
   Host_Name_Not_Found: exception;

   -- A string is not a valid host number.
   -- Raised by To_Host.
   --
   Bad_Host_No:    exception;

   -- Error in Bind call (binding socket).
   --
   Bind_Failed:    exception;

   -- Error setting socket options.
   --
   Setsock_Failed: exception;

private

   Void_Host: constant Host_No := (S_Addr => C_Sockets.INADDR_ANY);

--   Void_Port: constant Port_No := To_Port (0);

   type Option_S_Bool is new Option_S with record
      Value:  C.Int := 1;
      Name:   C.Int;
   end record;

   type Option_S_Linger is new Option_S with record
      OnOff:  C.Int := 1; -- Default: set.
      Linger: C.Int := 0; -- Default: wait for 0.
   end record;

   type Option_IP_Multi is new Option_IP with
      record
         Name: C.Int;
         Mreq: C_Sockets.IP_Mreq;
      end record;

   -- Maximun pending connections for accept (TCP).
   --
   Back_Log: constant C.Int := 5;

end Ada_Sockets;
