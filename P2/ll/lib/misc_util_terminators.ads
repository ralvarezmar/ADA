-- $Id: misc_util_terminators.ads,v 1.3 1996/12/22 00:14:31 jgb Exp $
--

-- I need to do some work when main procedure is completed,
--  but still unfinished. For instance, kill all receiving
--  tasks (which are hanged in a RecvFrom call to a socket).
--  Pakages whishing to do some of this work will register
--  a terminator here. When Execute_Terminators is invoked,
--  all of that Terminators will in turn be executed.

package Misc_Util_Terminators is

   -- We want list of terminators to be initialized before anybody
   --  calls here.
   pragma Elaborate_Body;

   -- Access to a Terminator procedure.
   type Teminator_Access is access procedure;

   -- Register a terminator.
   procedure Register_Terminator (Code: in Teminator_Access;
				  Name: in String);

   -- Execute all terminators.
   procedure Execute_Terminators;

   -- List of terminators. Useful for debugging.
   function Image return String;

end Misc_Util_Terminators;
