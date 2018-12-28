-- $Id: c_sockets_arch.ads,v 1.5 1998/01/19 12:46:35 jgb Exp $
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
-- Linux version.
--  In this package goes the architecture dependent code (directly used
--  by C_Sockets, and not using facilites from C_Sockets).
------------------------------------------------------------------------------

with Interfaces.C;

package C_Sockets_Arch is

   pragma Preelaborate;

   package C renames Interfaces.C;

   -------------------------------------------------------------------------
   -- Constants defined in <linux/socket.h>   (Linux)
   -- #define SOCK_STREAM       1   /* stream (connection) socket       */
   -- #define SOCK_DGRAM        2   /* datagram (conn.less) socket      */
   -- #define SOCK_RAW  3           /* raw socket                       */
   -- #define SOCK_RDM  4           /* reliably-delivered message       */
   -- #define SOCK_SEQPACKET    5   /* sequential packet socket */
   -- #define SOCK_PACKET       10  /* linux specific way of    */
   --                               /* getting packets at the dev       */
   --                               /* level.  For writing rarp and     */
   --                               /* other similar things on the      */
   --                               /* user level.                      */

   SOCK_STREAM: constant C.int := 1;
   SOCK_DGRAM:  constant C.int := 2;


   -- Parameter "level" of setsockopt
   -- #define SOL_SOCKET      1
   --
   SOL_SOCKET: constant C.int := 1; -- <asm/socket.h>

   -- Parameter "optname" of setsockopt (for socket level)
   --    #define SO_REUSEADDR   2
   --    #define SO_LINGER      13
   --    /* To add :#define SO_REUSEPORT 15 */  ???
   --
   SO_REUSEPORT: constant C.Int := 2; -- <asm/socket.h>
                                      -- (really SO_REUSEADDR)
   SO_LINGER:    constant C.Int := 13;

   -- Parameter "optname" of setsockopt (for level IP)
   IP_MULTICAST_IF:     constant C.Int := 32; -- <linux/socket.h>
   IP_MULTICAST_TTL:    constant C.Int := 33; -- <linux/socket.h>
   IP_MULTICAST_LOOP:   constant C.Int := 34; -- <linux/socket.h>
   IP_ADD_MEMBERSHIP:   constant C.Int := 35; -- <linux/socket.h>
   IP_DROP_MEMBERSHIP:  constant C.Int := 36; -- <linux/socket.h>

   -- Needed for defining Sockaddr_In.
   -- <linux/in.h>
   --
   -- struct in_addr {
   --   __u32   s_addr;
   -- };
   --
   type in_addr is
      record
         s_addr:     Interfaces.unsigned_32;
      end record;
   pragma Convention (C, In_Addr);

   -- <linux/in.h>
   --
   -- #define __SOCK_SIZE__     16          /* sizeof(struct sockaddr)  */
   -- struct sockaddr_in {
   --   short int               sin_family; /* Address family           */
   --   unsigned short int      sin_port;   /* Port number              */
   --   struct in_addr  sin_addr;           /* Internet address         */

   --   /* Pad to size of `struct sockaddr'. */
   --   unsigned char           __pad[__SOCK_SIZE__ - sizeof(short int) -
   --                   sizeof(unsigned short int) - sizeof(struct in_addr)];
   -- };
   -- #define sin_zero  __pad               /* for BSD UNIX comp. -FvK  */

   type sockaddr_in is
      record
         sin_family: C.short;
         sin_port:   C.unsigned_short;
         sin_addr:   in_addr;
         sin_zero:   C.char_array (0..7);
      end record;
   pragma Convention (C, sockaddr_in);

end C_Sockets_Arch;
