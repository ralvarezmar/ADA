with Ada.Real_Time;
with Hash_Maps_G;
With Ada.Strings.Unbounded;
with Maps_G;
with Lower_Layer_UDP;

package Procedures is
   package LLU renames Lower_Layer_UDP;
   package ASU  renames Ada.Strings.Unbounded;
   use type LLU.End_Point_Type;
   use type Ada.Real_Time.Time;

type Seq_N_T is mod Integer'Last;

   procedure Retransmission_Client;

   procedure Retransmission_Server;

   procedure Print_Old;

   procedure Print_Old_Inv;

   procedure Print_Active;

   procedure Print_Active_Inv;
end Procedures;
