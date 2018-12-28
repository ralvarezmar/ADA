-- $Id: c_sockets.ads,v 1.11 1998/01/19 12:46:43 jgb Exp $
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
-- Thin interface to Unix/C sockets (simplified UDP only).
--  Contributions by tatou.
------------------------------------------------------------------------------

-- Architecture dependent code.
--
with C_Sockets_Arch;

with Interfaces.C.Strings;

with Ada.Streams;

package C_Sockets is

   pragma Preelaborate;

   package C renames Interfaces.C;
   package C_Str renames Interfaces.C.Strings;
--   package Arch  renames C_Sockets_Arch;

   -------------------------------------------------------------------------
   -- Constants defined in <sys/socket.h>   (NetBSD, Solaris)
   --                      <linux/socket.h> (Linux)
   -------------------------------------------------------------------------

   AF_INET:     constant C.int := 2; -- <linux/socket.h>
--   SOCK_STREAM: constant C.int := 1;
--   SOCK_DGRAM:  constant C.int := 2; -- <linux/socket.h>
   SOCK_STREAM: constant C.int := C_Sockets_Arch.SOCK_STREAM;
   SOCK_DGRAM:  constant C.int := C_Sockets_Arch.SOCK_DGRAM;

   -- Parameter "level" of setsockopt
   --
   SOL_SOCKET: constant C.int := C_Sockets_Arch.SOL_SOCKET;

   -- Parameter "optname" of setsockopt (for socket level)
   --
   SO_REUSEPORT: constant C.Int := C_Sockets_Arch.SO_REUSEPORT;
   SO_LINGER:    constant C.Int := C_Sockets_Arch.SO_LINGER;

   -------------------------------------------------------------------------
   -- Types defined in <sys/socket.h>   (NetBSD)
   --                  <linux/socket.h> (Linux)
   -------------------------------------------------------------------------

   -- struct linger {
   --   int    l_onoff;  /* Linger active           */
   --   int    l_linger; /* How long to linger for  */
   -- };
   --

   type Linger is record
      OnOff:  C.Int;
      Linger: C.Int;
   end record;
   pragma Convention (C, Linger);

   -------------------------------------------------------------------------
   -- Constants defined in <netinet/in.h>
   -------------------------------------------------------------------------

   INADDR_ANY: constant Interfaces.unsigned_32 := 0; -- <linux/in.h>

   -- Parameter "level" of setsockopt
   IPPROTO_IP: constant C.int           := 0; -- <linux/in.h>

   -- Parameter "optname" of setsockopt (for level IP)
   IP_MULTICAST_IF:     constant C.int := C_Sockets_Arch.IP_MULTICAST_IF;
   IP_MULTICAST_TTL:    constant C.int := C_Sockets_Arch.IP_MULTICAST_TTL;
   IP_MULTICAST_LOOP:   constant C.int := C_Sockets_Arch.IP_MULTICAST_LOOP;
   IP_ADD_MEMBERSHIP:   constant C.int := C_Sockets_Arch.IP_ADD_MEMBERSHIP;
   IP_DROP_MEMBERSHIP:  constant C.int := C_Sockets_Arch.IP_DROP_MEMBERSHIP;

   -------------------------------------------------------------------------
   -- Types from <netinet/in.h>
   -------------------------------------------------------------------------

   subtype in_addr is C_Sockets_Arch.In_Addr;

   subtype sockaddr_in is C_Sockets_Arch.Sockaddr_In;

   -- <linux/in.h>
   --
   -- struct ip_mreq {
   --     struct  in_addr imr_multiaddr;  /* IP multicast address of group */
   --     struct  in_addr imr_interface;  /* local IP address of interface */
   --     };

   type ip_mreq is
      record
         imr_multiaddr: in_addr;
         imr_interface: in_addr;
      end record;
   pragma Convention (C, ip_mreq);

   -------------------------------------------------------------------------
   -- Types from <sys/time.h>
   -------------------------------------------------------------------------

   -- <linux/time.h>
   --
   -- struct timeval {
   --   long    tv_sec;         /* seconds */
   --   long    tv_usec;        /* microseconds */
   -- };

   type Timeval is
      record
         Tv_Sec: C.Int;
         Tv_usec: C.Int;
      end record;
   pragma Convention (C, Timeval);

   -------------------------------------------------------------------------
   -- Types from <sys/types.h>
   -------------------------------------------------------------------------

   -- <linux/posix_types.h>
   --
   -- #undef __NFDBITS
   -- #define __NFDBITS (8 * sizeof(unsigned int))
   --
   -- #undef __FD_SETSIZE
   -- #define __FD_SETSIZE      256
   --
   -- #undef __FDSET_INTS
   -- #define __FDSET_INTS      (__FD_SETSIZE/__NFDBITS)
   --
   -- typedef struct fd_set {
   --   unsigned int fds_bits [__FDSET_INTS];
   -- } __kernel_fd_set;

   FD_SETSIZE: constant Integer := 256;
   subtype FD_Set_Range is Integer range 0..FD_SETSIZE-1;
   type FD_Set is array (FD_Set_Range'Range) of Boolean;
   pragma Pack (Fd_Set);
--   for Fd_Set'Bit_Order use System.High_Order_First;

   -- From <netdb.h>
   --
   -- struct  hostent {
   --   char    *h_name;        /* official name of host */
   --   char    **h_aliases;    /* alias list */
   --   int     h_addrtype;     /* host address type */
   --   int     h_length;       /* length of address */
   --   char    **h_addr_list;  /* list of addresses from name server */
   --   };

   type Chars_Ptr_Ptr is access all C_Str.Chars_Ptr;
   type Address_Ptr is access all In_Addr;
   type Address_Ptr_Ptr is access all Address_Ptr;

   type Hostent is
      record
         H_Name:      C_Str.Chars_Ptr;
         H_Aliases:   Chars_Ptr_Ptr;
         H_Addrtype:  C.Int;
         H_Length:    C.Int;
         H_Addr_List: Address_Ptr_Ptr;
      end record;
   pragma Convention (C, Hostent);

   ------------------------------------------------------------------------
   --
   -- Some functions...
   -- (These are really the binded funtions)
   --
   ------------------------------------------------------------------------

   --
   -- Socket
   --
   -- int socket(int domain, int type, int protocol)
   --
   -- DESCRIPTION
   --  Socket() creates an endpoint for communication and returns a
   --  descriptor.

   function Socket (Domain:    in C.int;
                    Sock_Type: in C.int;
                    Protocol:  in C.int)
                    return C.int;
   pragma Import (C, Socket, "socket");

   --
   -- Bind
   --
   -- int bind(int s, struct sockaddr *name, int namelen)
   --
   -- DESCRIPTION
   --  Bind() assigns a name to an unnamed socket.  When a socket is created
   --  with socket(2) it exists in a name space (address family) but has
   --  no name assigned.  Bind() requests that name be assigned to
   --  the socket.

   function Bind (Sock_FD: in     C.int;
                  Name:    access sockaddr_in;
                  Len:     in     C.int)
                  return C.int;
   pragma Import (C, Bind, "bind");


   --
   -- Connect
   --
   -- int connect (int s, struct sockaddr *servaddr, int addrlen)
   --
   -- DESCRIPTION
   --  Connect() establish a connection between the local system and the
   --  foreign system. Messages are exchanged between the two systems
   --  and specific parameters relating to the conversations might be
   --  agreed on (buffer sizes, etc.). The connect system call does
   --  not return until the connection is established, or an error is
   --  returned to the process. The client does not have to bind a
   --  local address before calling connect.

   function Connect (Sock_FD: in     C.int;
                     Name:    access sockaddr_in;
                     Len:     in     C.int)
                     return C.int;
   pragma Import (C, Connect, "connect");


   --
   -- Listen
   --
   -- int listen (int s, int backlog)
   --
   -- DESCRIPTION
   --  It is usually executed after both the socket and bind system
   --  calls, and inmediately before the accept system call. The backlog
   --  argument specifies how many connection requests can be queued
   --  by the system while it waits for the server to execute the accept
   --  system call.

   function Listen (Sock_FD: in C.int;
                    Backlog: in C.Int)
                    return C.int;
   pragma Import (C, Listen, "listen");

   --
   -- Accept
   --
   -- int accept (int s, struct sockaddr *addr, int *addrlen)
   --
   -- DESCRIPTION
   --  The argument s is a socket that has been created with socket(2),
   --  bound to an address with bind(2),  and is listening for
   --  connections after a listen(2).  The accept() argument extracts
   --  the first connection request on the queue of pending connections,
   --  creates a new socket with the same properties of s and allocates
   --  a new file descriptor for the socket.  If no pending connections
   --  are present on the queue, and the socket is not marked as
   --  non-blocking, accept() blocks the caller until a connection is
   --  present.  If the socket is marked non-blocking and no pending
   --  connections are present on the queue, accept() returns an error
   --  as described below. The accepted socket may not be used to accept
   --  more connections.  The original socket s remains open.
   --

   function FD_Accept (Sock_FD: in     C.int;
                       Peer:    access sockaddr_in;
                       Len:     access C.int)
                       return C.int;
   pragma Import (C, FD_Accept, "accept");

   --
   -- Recv, Recvfrom
   --
   -- int recv(int s, void *buf, size_t len, int flags)
   --
   -- int recvfrom(int s, void *buf, int len, int flags,
   --   struct sockaddr *from, int *fromlen)
   --
   -- DESCRIPTION
   --  Recvfrom() and recvmsg() are used to receive messages from a socket, and
   --  may be used to receive data on a socket whether or not it is connection-
   --  oriented.


-- XXX
   function Recv (Sock_FD: in     C.int;
                  Buffer:  in     Ada.Streams.Stream_Element_Array;
                  Len:     in     C.int;
                  Flags:   in     C.Int)
                  return C.Int;
   pragma Import (C, Recv, "recv");

-- XXX
   function Recvfrom (Sock_FD:   in     C.int;
                      Buffer:    in     Ada.Streams.Stream_Element_Array;
                      Len:       in     C.int;
                      Flags:     in     C.int;
                      From_Addr: access sockaddr_in;
                      From_Len:  access C.int)
                      return C.int;
   pragma Import (C, Recvfrom, "recvfrom");


   --
   -- Send, Sendto
   --
   -- int send(int s, const void *msg, int len, int flags)
   --
   -- int sendto(int s, const void *msg, int len, int flags,
   --   const struct sockaddr *to, int tolen)
   --
   -- DESCRIPTION
   --  Send(), sendto(), and sendmsg() are used to transmit a message
   --  to another socket.  Send() may be used only when the socket
   --  is in a connected state, while sendto() and sendmsg() may be
   --  used at any time.

   function Send   (Sock_FD:  in C.int;
                    Buffer:   in Ada.Streams.Stream_Element_Array;
                    Len:      in C.int;
                    Flags:    in C.Int)
                    return C.int;
   pragma Import (C, Send, "send");

   function Sendto (Sock_FD: in     C.int;
                    Buffer:  in     Ada.Streams.Stream_Element_Array;
                    Len:     in     C.int;
                    Flags:   in     C.int;
                    To_Addr: access sockaddr_in;
                    To_Len:  in     C.int)
                    return C.int;
   pragma Import (C, Sendto, "sendto");

   --
   -- GetSockOpt, SetSockOpt
   --
   -- DESCRIPTION
   --  Getsockopt() and setsockopt() manipulate the options associated with a
   --  socket.  Options may exist at multiple protocol levels; they are always
   --  present at the uppermost ``socket'' level.
   --

   -- GetSockOpt
   --
   -- int getsockopt(int s, int level, int optname,
   --   void *optval, int *optlen)
   --

   function GetSockOpt (Sock_FD:  in C.int;
                        Level:    in C.int;
                        Opt_Name: in C.int;
                        Opt_Val:  access C.Int;
                        Opt_Len:  access C.int)
                        return C.int;
--   pragma Import (C, GetSockOpt, "getsockopt");

   function GetSockOpt (Sock_FD:  in C.int;
                        Level:    in C.int;
                        Opt_Name: in C.int;
                        Opt_Val:  access Ip_mreq;
                        Opt_Len:  access C.int)
                        return C.int;
   pragma Import (C, GetSockOpt, "getsockopt");

   -- SetSockOpt
   --
   -- int setsockopt(int s, int level, int optname,
   --   void *optval, int optlen)
   --
   -- NOTES
   --  I'll define three functions, one with access int for Opt_Val,
   --  other with access to Linger, and another
   --  one with ip_mreq, because this is what upper layers need.

   function SetSockOpt (Sock_FD:  in C.int;
                        Level:    in C.int;
                        Opt_Name: in C.int;
                        Opt_Val:  access C.Int;
                        Opt_Len:  in C.int)
                        return C.int;
--   pragma Import (C, SetSockOpt, "setsockopt");

   function SetSockOpt (Sock_FD:  in C.int;
                        Level:    in C.int;
                        Opt_Name: in C.int;
                        Opt_Val:  access Linger;
                        Opt_Len:  in C.int)
                        return C.int;
--   pragma Import (C, SetSockOpt, "setsockopt");

   function SetSockOpt (Sock_FD:  in C.int;
                        Level:    in C.int;
                        Opt_Name: in C.int;
                        Opt_Val:  access Ip_mreq;
                        Opt_Len:  in C.int)
                        return C.int;
   pragma Import (C, SetSockOpt, "setsockopt");

   --
   -- Close
   --
   -- int close(int d)
   --
   -- DESCRIPTION
   --  The close() call deletes a descriptor from the per-process object
   --  reference table.  If this is the last reference to the
   --  underlying object, the object will be deactivated.  For example,
   --  on the last close of a file the current seek pointer associated
   --  with the file is lost; on the last close of a socket(2)
   --  associated naming information and queued data are discarded;
   --  on the last close of a file holding an advisory lock the lock
   --  is released

   function Close (Sock_FD: C.Int) return C.Int;
   pragma Import (C, Close, "close");

   -- Select
   --
   -- int
   -- select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
   --        struct timeval *timeout)
   --
   -- DESCRIPTION
   --  Select() examines the I/O descriptor sets whose addresses are
   --  passed in readfds, writefds, and exceptfds to see if some of
   --  their descriptors are ready for reading, are ready for writing,
   --  or have an exceptional condition pending, respectively.
   -- NOTES
   --  I cannot use the identifier Select because it's reserved in Ada.
   --

   function FD_Select (Nfds:      in C.Int;
                       ReadFDS:   access FD_Set;
                       WriteFDS:  access FD_Set;
                       ExceptFDS: access FD_Set;
                       Timeout:   access Timeval)
                       return C.int;
   pragma Import (C, FD_Select, "select");

   -- Htonl, Htons, Ntohl, Ntohs
   --
   -- DESCRIPTION
   --  These routines convert 16 and 32 bit quantities between network
   --  byte order and host byte order.  On machines which have a byte
   --  order which is the same as the network order, routines are
   --  defined as null macros. These routines are most often used
   --  in conjunction with Internet addresses
   --  and ports as returned by gethostbyname(3) and getservent(3).

   --
   -- Htons
   --
   -- U_Short htons(u_short hostshort)
   --

   function Htons (Data: in C.unsigned_short) return C.unsigned_short;
   pragma Import (C, Htons, "htons");

   --
   -- Ntohs
   --
   -- U_Short ntohs(u_short netshort)
   --

   function Ntohs (Data: in C.unsigned_short) return C.unsigned_short;
   pragma Import (C, Ntohs, "ntohs");

   --
   -- Htonl
   --
   -- U_Long htonl(u_long hostlong)
   --

   function Htonl (Data: in Interfaces.unsigned_32) return Interfaces.unsigned_32;
   pragma Import (C, Htonl, "htonl");

   --
   -- Ntohl
   --
   -- u_long ntohl(u_long netlong)
   --

   function Ntohl (Data: in Interfaces.unsigned_32) return Interfaces.unsigned_32;
   pragma Import (C, Ntohl, "ntohl");

   --
   -- Inet_Addr, Inet_Aton
   --
   -- DESCRIPTION
   --  The routines inet_aton(), inet_addr() and inet_network()
   --  interpret character strings representing numbers expressed
   --  in the Internet standard `.' notation.
   --  The inet_addr() and inet_network() functions
   --  return numbers suitable for use as Internet addresses and
   --  Internet network numbers, respectively.

   --
   -- Inet_Addr
   --
   -- unsigned long inet_addr(char *cp)

   function Inet_Addr (Addr: in C_Str.Chars_ptr)
                       return Interfaces.unsigned_32;
   pragma Import (C, Inet_addr, "inet_addr");

   --------------------------------------------------------------
   -- Patch added by Alvaro Polo (apoloval@gsyc.escet.urjc.es) --
   -- for GNAT 4.0 compability.                                --
   --------------------------------------------------------------


   -- Null address constant.
   -- <linux/in.h> on GNU/Linux systems
   -- <netinet/in.h> on Mac OS X/Darwin systems.
   INADDR_NONE : constant Interfaces.unsigned_32 := 16#ffffffff#;

   -------------------
   -- End of patch. --
   -------------------

   -- Deprecated (not available on Solaris). Substituted by inet_addr
   --
   -- Inet_Aton
   --
   -- int inet_aton(char *cp, struct in_addr *pin)

--   function Inet_Aton (Str_Host: in C_Str.Chars_Ptr;
--                     In_Host:  access In_Addr)
--                     return C.Int;
--   pragma Import (C, Inet_Aton, "inet_aton");

   --
   -- Inet_Ntoa
   --
   -- char * inet_ntoa(struct in_addr in)
   --
   -- DESCRIPTION
   --  The routine inet_ntoa() takes an Internet
   --  address and returns an ASCII string representing the address in
   --  `.' notation.

   function Inet_Ntoa (Addr: in In_Addr)
                       return C_Str.Chars_Ptr;
   pragma Import (C, Inet_Ntoa, "inet_ntoa");
   pragma Import_Function (Inet_Ntoa, Mechanism => (Addr => Value));

   --
   -- Get_Sock_Name
   --
   -- int getsockname(int s, struct sockaddr *name, int *namelen)
   --
   -- DESCRIPTION
   --  Getsockname() returns the current name for the specified socket.
   --  The namelen parameter should be initialized to indicate the
   --  amount of space pointed to by name. On return it contains the
   --  actual size of the name returned (in bytes).

   function Get_Sock_Name (Socket: in     C.int;
                           Name:   access sockaddr_in;
                           Len:    access C.int)
                           return C.int;
   pragma Import (C, Get_Sock_Name, "getsockname");

   -- Get_Host_By_Name
   --
   --      struct hostent *
   --      gethostbyname(const char *name)
   --
   -- DESCRIPTION
   --      The gethostbyname() and gethostbyaddr() functions each return
   --      a Pointer to an object with the following structure
   --      describing an internet Host referenced by name or by address,
   --      respectively.  This structure Contains either the information
   --      obtained from the name server, named(8),  brokenout fields
   --      from a line in /etc/hosts, or database entries supplied by the
   --      yp(8) system .  If the local name server is not running these
   --      routines do a lookup in /etc/hosts.

   type Hostent_Ptr is access all Hostent;

   function Get_Host_By_Name (Name: in C_Str.Chars_Ptr)
                              return Hostent_Ptr;
   pragma Import (C, Get_Host_By_Name, "gethostbyname");

   -- From <sys/param.h>
   --
   -- MAXHOSTNAMELEN
   Max_Host_Name_Len: constant C.Int := 256;

   -- Get_Host_Name
   --
   --      int
   --      gethostname(char *name, int namelen)
   --
   -- DESCRIPTION
   --      Gethostname() returns the standard host name for the current
   --  processor, as previously set by sethostname().  The parameter
   --  namelen specifies the size of the name array.  The returned
   --  name is null-terminated unless insufficient space is provided.

   function Get_Host_Name (Name:     in C_Str.Chars_Ptr;
                           Name_Len: in C.Int)
                           return C.Int;
   pragma Import (C, Get_Host_Name, "gethostname");

--    Errno: C.Int;
--    pragma Import (C, Errno, "errno");

   -- jcenteno (2003.12.09): always obtain errno from function
   function Errno return Interfaces.C.int;
   pragma Import (C, Errno, "__get_errno");


   -- pheras: Return C variable errno. Importing the C variable does not work!
   -- jgb: This is probably due to the LinuxThreads implementation

   function C_Errno return Interfaces.C.int;
   pragma Import (C, C_Errno, "__get_errno");

end C_Sockets;
