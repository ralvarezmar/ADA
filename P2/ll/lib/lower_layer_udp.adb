-- $Id$
--
------------------------------------------------------------------------------
--                                                                          --
--    Copyright (C) 1997-2000  Jesus M. Gonzalez Barahona                   --
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

with Lower_Layer.Inet.UDP.Uni;
with Lower_Layer.Inet.UDP.Multi;
with Lower_Layer.Inet.Misc;
with Misc_Util_Terminators;
with Ada_Sockets;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Real_Time;
with Ada.Task_Identification;

with Ada.Text_IO; use Ada.Text_IO;


package body Lower_Layer_UDP is
   use type Ada.Streams.Stream_Element_Offset;

   Debug: constant Boolean := False;

   -- There is a pool of receiving buffers associated with each endpoint.
   -- Pool_Size is the maximum number of receiving buffers available for each
   -- endpoint. If an application does not  receive Pool_Size messages
   -- arrived to an endpoint, the next ones will be dropped until some of
   -- the ones stored are received.
   Pool_Size : constant Integer := 200;

   -- Maximum data size for a buffer, measured in bytes:
   --     Max_IP_dgram_size - (IP_hdr_size + UDP_hdr_size)
   Max_Buffer_Size : constant Ada.Streams.Stream_Element_Offset := 65535 - (20+8);


   subtype Restricted_Faults_Percent_Type is Integer range 1..100;
   package Random_Percent is
     new Ada.Numerics.Discrete_Random (Restricted_Faults_Percent_Type);
   Percent_Generator: Random_Percent.Generator;
   package Random_Delay renames Ada.Numerics.Float_Random;
   Delay_Generator: Random_Delay.Generator;

   use type Lower_Layer.Address_Ca; -- for "=" operator

   -- Maximum number of End_Points to handle concurrently
   -- *** Check what happen if it's not a constant!!! It compiles but
   -- *** raises an strange exception. The problem is due to the entry
   -- *** families
   Capacity : constant Positive := 20;

   type Index is new Integer range 1 .. Capacity;

   -- Uses directly its Lower_Layer equivalent
   procedure Reset(A_Buffer: in out Buffer_Type) is
   begin
      Lower_Layer.Reset(A_Buffer, 0);
   end Reset;


   -- Uses directly its Lower_Layer equivalent
   procedure Copy (Destination: access Buffer_Type;
                   Source:      access Buffer_Type)
     renames Lower_Layer.Copy;


   -- Uses directly its Lower_Layer equivalent
   function Image (A_Buffer: in Buffer_Type) return String
     renames Lower_Layer.Image;


   -- Uses directly its Lower_Layer equivalent
   function Image (An_EP: in End_Point_Type) return String
     renames Lower_Layer.Image;


   -- Uses directly its Lower_Layer equivalent
   function Is_Null (An_EP: in End_Point_Type) return Boolean
     renames Lower_Layer.Is_Null;

   -- Uses directly its Lower_Layer equivalent
   function "=" (Left, Right: in End_Point_Type) return Boolean
     renames Lower_Layer."=";


   -- Creates an End_Point and calls to the proper
   -- Lower_Layer.XXX.Build (unicast or multicast)
   function Build(IP: String; Port: Natural) return End_Point_Type is

      function Is_Multi (An_IP: String) return Boolean is
         use type Ada_Sockets.Host_No;
      begin
         if Ada_Sockets.To_Host (An_IP) >= Ada_Sockets.To_Host ("224.0.0.0")
           and then
           Ada_Sockets.To_Host ("239.255.255.255") >= Ada_Sockets.To_Host (An_IP)
         then
            return True;
         else
            return False;
         end if;
      end Is_Multi;

      Addr: Lower_Layer.Address_CA;

   begin
      if Is_Multi(IP) then
         Addr := new Lower_Layer.Inet.UDP.Multi.Address;
         Lower_Layer.Inet.UDP.Multi.Build
           (Lower_Layer.Inet.UDP.Multi.Address(Addr.all), IP, Port);
      else --Unicast Address
         Addr := new Lower_Layer.Inet.UDP.Uni.Address;
         Lower_Layer.Inet.UDP.Uni.Build
           (Lower_Layer.Inet.UDP.Uni.Address(Addr.all), IP, Port);
      end if;
      return Addr;
   end Build;


   -- Uses directly its Lower_Layer.Inet.Misc equivalent
   function To_IP (Name: in String) return String
     renames Lower_Layer.Inet.Misc.To_IP;

   -- Uses directly its Ada_Sockets equivalent
   function Get_Host_Name return String
     renames Ada_Sockets.Get_Host_Name;

   ---
   -- Exceptions and Data structures to handle Binding and Receiving
   ---

   Random_Propagation_Delay_Error: exception;
   -- raised by Set_Random_Propagation_Delay_Error;

   No_Bind_Before_Receive : exception;
   -- raised by buffer.locate

   No_Resources : exception;
   -- raised by buffer.allocate_new


   package Pools is
      -- Number of buffers available for each endpoint
      Size_Array: constant Integer := Pool_Size;


      type Buffer_Array is
        array (0 .. Size_Array-1) of aliased Buffer_Type(Max_Buffer_Size);
      type Pool is record
         Next_Put : Integer := 0;
         Next_Get : Integer := 0;
         Size     : Integer := 0;
         The_Pool : Buffer_Array;
      end record;

      -- Allways succeeds. If A_Pool is full, the Data is substituted for the
      -- oldest element in A_Pool
      -- Note that the entry Put is not using this degree of sophistication:
      --   it justs throws packages when there is no room in the pool
      procedure Put (Data: access Buffer_Type; A_Pool: in out Pool);

      -- Returns empty=true and nothing in Data if there are no elements to
      -- be get. Retrieves in Data the oldest element in A_Pool and Empty=false
      -- is return in
      -- Data if there are elements.
      procedure Get (Data: access Buffer_Type;
                     A_Pool: in out  Pool;
                     Empty : out boolean);

      function Size(A_Pool: Pool) return Integer;

      function Get_Size_Array return Integer;
   end Pools;

   package body Pools is
      function Get_Size_Array return Integer is
      begin
         return Size_Array;
      end Get_Size_Array;

      function Size(A_Pool: Pool) return Integer is
      begin
         return A_Pool.Size;
      end Size;

      procedure Put (Data: access Buffer_Type; A_Pool: in out Pool) is
      begin
         Lower_Layer.Reset (A_Pool.The_Pool(A_Pool.Next_Put));
         Lower_Layer.Copy  (A_Pool.The_Pool(A_Pool.Next_Put)'Access,
                            Data);

         if A_Pool.Size < Size_Array then
            A_Pool.Size := A_Pool.Size + 1;
         end if;

         -- Push forward the Next_Get pointer so that it always returns the
         -- oldest element
         if A_Pool.Next_Put = A_Pool.Next_Get and A_Pool.Size = Size_Array then
            A_Pool.Next_Get := (A_Pool.Next_Get + 1) mod Size_Array;
         end if;

         A_Pool.Next_Put := (A_Pool.Next_Put + 1) mod Size_Array;


      end Put;

      procedure Get (Data: access Buffer_Type;
                     A_Pool: in out Pool;
                     Empty : out boolean) is
      begin
         if A_Pool.Size > 0 then
            Empty := False;
         Lower_Layer.Reset (Data.all);

         Lower_Layer.Copy (Data,
                           A_Pool.The_Pool(A_Pool.Next_Get)'Access);
         A_Pool.Size := A_Pool.Size - 1;
         A_Pool.Next_Get := (A_Pool.Next_Get + 1) mod Size_Array;
         else
            Empty:=True;
         end if;
      end Get;

   end Pools;

   type Data is array (Index) of Pools.Pool;

   type Load is array (Index) of Boolean;

   type End_Points is array (Index) of End_Point_Type;

   -- Protected object used to store data received by Handler_To_Buffer
   -- and where Receive retrieves it.
   protected  Buffer is
      entry      Put (Index)  (From_EP: in End_Point_Type;
                              An_EP: in End_Point_Type;
                              Data: access Buffer_Type);
      entry     Get (Index)  (From_EP: out End_Point_Type;
                              An_EP: in End_Point_Type;
                              Data: access Buffer_Type);
      function  Locate (An_Ep : End_Point_Type) return Index;
      procedure Allocate_New (An_Ep : End_Point_Type; Location : out Index);
      procedure Free (An_Ep : End_Point_Type);
      function  Count_Loaded return Natural;
   private
      From_EPs: End_Points;
      The_EP: End_Points;
      Loaded: Load  := (others => False);
      The_Data: Data;
      Is_Free : Load := (others => True); -- is the slot assigned?
   end Buffer;

   protected body Buffer is
      entry Get (for I in Index) (From_EP: out End_Point_Type;
                                  An_EP: in End_Point_Type;
                                  Data: access Buffer_Type)
      when Loaded (I) = True is

         Empty: Boolean;
      begin
         if Debug then Put_Line("entrando en el Get"); end if;

         Pools.Get (Data, The_Data(I), empty);
         if Pools.Size(The_Data(I)) = 0 then
            Loaded (I) := False;
         end if;

         From_EP := From_EPs (I);

         if Debug then Put_Line("saliendo del   Get"); end if;
      end Get;


      entry Put (for I in Index) (From_EP: in End_Point_Type;
                                  An_EP: in End_Point_Type;
                                  Data: access Buffer_Type)
      when True is
      begin
         if Debug then Put_Line("entrando en el Put"); end if;

         -- If there is room for a buffer, store it. If not, just throw it away
         if Pools.Size(The_Data(I)) < Pools.Get_Size_Array then
            From_EPs (I) := From_EP;
            The_EP (I) := An_EP;

            Pools.Put (Data, The_Data(I));
            Loaded (I) := True;
         end if;

         if Debug then Put_Line("saliendo del   Put"); end if;
      end Put;


      function Locate (An_Ep : End_Point_Type) return Index is
         I : Index;

         Finish_Loop : Boolean; -- flag to exit from loop
         Found : Boolean;
      begin -- Locate
         if Debug Then Put_Line ("entrando en locate"); end if;

         Found := False;
         Finish_loop := False;
         I := Index'First;
         while not Finish_loop
         loop
            if (not Is_free (I)) and then An_Ep = The_EP (I) then
               Finish_Loop := True;
               Found := True;
            else
               if I < Index'Last then
                  I := I + 1;
               else Finish_Loop := True;
               end if;
            end if;
         end loop;

         if Debug Then Put_Line ("saliendo de locate"); end if;

         if Found then
            return I;
         else
            Raise No_Bind_Before_Receive;
         end if;
      end Locate;


      procedure Allocate_New (An_Ep : End_Point_Type; Location : out Index)  is
         I : Index := Index'First;

         Finish_Loop : Boolean; -- flag to exit from loop
         Found : Boolean;
      begin -- allocate_new
         if Debug Then Put_Line ("entrando en allocate_new"); end if;
         Found := False;
         Finish_loop := False;
         while not Finish_loop
         loop
            if (Is_free (I)) then
               Finish_Loop := True;
               Found := True;
            else
               if I < Index'Last then
                  I := I + 1;
               else
                  Finish_Loop := True;
               end if;
            end if;
         end loop;

         if Found then
            Is_Free (I) := False;
            The_Ep (I) := An_Ep;
            Loaded (I) := False;

            Location := I;
         else
            if Debug then Put_Line ("raising no_resource"); end if;
            raise No_Resources;
         end if;

         If Debug Then Put_Line ("saliendo de allocate_new"); end if;
      end Allocate_New;

      procedure Free (An_Ep : End_Point_Type) is
         I : Index := Locate (An_Ep);
      begin   -- Free
         Loaded (I) := False;
         Is_Free (I) := True;
      end Free;

      function  Count_Loaded return Natural is
         Total: Natural range 0..Integer(Index'Last) := 0;
      begin
         for I in Index loop
            if Loaded(I) then
               Total := Total + 1;
            end if;
         end loop;
         return Total;
      end Count_Loaded;

   end Buffer;



   -- Handler which stores received data into protected Buffer
   procedure Handler_To_Buffer(From : in End_Point_Type;
                               To:   in     End_Point_Type;
                               Data: access Buffer_Type) is
      I : Index;
   begin
      I  := Buffer.Locate (To);

      if Debug then
         Put_Line("entrando en el Handler, con index = " & I'Img );
      end if;

      Buffer.Put (I)(From, To, Data);

      if Debug then Put_Line("saliendo del   Handler"); end if;
   end Handler_To_Buffer;

   -- Actual access to Handler_To_Buffer
   The_Handler_To_Buffer: Handler_Type := Handler_To_Buffer'Access;



   -- Calls to Lower_Layer.Bind.
   --  (a) If A_Handler parameter is not given,
   --      reception with tiemouts will be enabled through the special
   --      The_Handler_To_Buffer handler. Receive will be used then to
   --      Receive
   --  (b) If A_Handler is given, reception will be done by that handler.
   --      No call to Receive must be done
   procedure Bind (An_EP:    in End_Point_Type;
                   A_Handler: in Handler_Type := null) is
      I : Index;
   begin
      if A_Handler = null then

         Lower_Layer.Bind(An_EP,
                          Lower_Layer.Handler_A(The_Handler_To_Buffer));
         Buffer.Allocate_New (An_EP, Location => I);
      else
         Lower_Layer.Bind(An_EP, Lower_Layer.Handler_A(A_Handler));
      end if;
   end Bind;


   -- Calls to Lower_Layer.Bind_Any.
   --  (a) If A_Handler parameter is not given,
   --      reception with tiemouts will be enabled through the special
   --      The_Handler_To_Buffer handler. Receive will be used then to
   --      Receive
   --  (b) If A_Handler is given, reception will be done by that handler.
   --      No call to Receive must be done
   procedure Bind_Any (An_EP:   out End_Point_Type;
                       A_Handler: in Handler_Type := null) is
      Addr: Lower_Layer.Address_CA := new Lower_Layer.Inet.UDP.Uni.Address;
      I: Index;
   begin
      if A_Handler = null then
         Lower_Layer.Bind_Any(Addr,
                              Lower_Layer.Handler_A(The_Handler_To_Buffer));
         Buffer.Allocate_New (Addr, Location => I);
      else
         Lower_Layer.Bind_Any(Addr,
                              Lower_Layer.Handler_A(A_Handler));
      end if;
      An_EP := Addr;
   end Bind_Any;


   -- Calls to its Lower_Layer equivalent
   procedure Unbind(An_EP: in End_Point_Type) is
      Had_Timer: Boolean := True;
      I: Index;
   begin
      if (Debug) then Put_Line("entrando en Unbind"); end if;
      Lower_Layer.Unbind(An_EP);
      begin
         I := Buffer.Locate(An_EP);
      exception
         when No_Bind_Before_Receive => Had_Timer := False;
      end;

      if Had_Timer then
         Buffer.Free (An_Ep);
      end if;
      if (Debug) then Put_Line("saliendo del Unbind"); end if;
   end Unbind;




   -- Mode type of simulated faults: vector or percent
   type Faults_Mode_Type is (Vector, Percent, None);
   Faults_Mode: Faults_Mode_Type := None;

   -- Package variables to simulate faulty "Send" invocations with Faults Vectors
   Round_Number: Natural := 0;
   The_Faults_Vector: Faults_Vector_Type (1..100) := (others => False);
   TFV_Effective_Length: Integer := 1;

   -- Package variables to simulate faulty "Send" invocations with Faults Percent
   Faults_Percent: Faults_Percent_Type;

   -- Copies Vector into Vector'Length slice of The_Faults_Vector
   procedure Set_Faults_Vector(The_Vector: in Faults_Vector_Type) is
   begin
      Faults_Mode := Vector;
      The_Faults_Vector(1..The_Vector'Length) := The_Vector;
      TFV_Effective_Length := The_Vector'Length;
   end Set_Faults_Vector;

   -- Put into Vector the Vector'Length slice of The_Faults_Vector
   function Get_Faults_Vector return Faults_Vector_Type is
      Vector: Faults_Vector_Type :=
        The_Faults_Vector (1.. TFV_Effective_Length);
   begin
      return Vector;
   end Get_Faults_Vector;

   -- Copies P into Faults_Percent
   procedure Set_Faults_Percent (P: in Faults_Percent_Type) is
   begin
      Faults_Mode := Percent;
      Faults_Percent := P;
   end Set_Faults_Percent;

   -- Returns Faults_Percent
   function Get_Faults_Percent return Faults_Percent_Type is
   begin
      return Faults_Percent;
   end Get_Faults_Percent;


   -- Actualy sends data now, considering potencial faults,
   -- with or without From
   procedure Send_Now (From:   in     End_Point_Type;
                       To:     in     End_Point_Type;
                       Data:   access Buffer_Type) is
      Value: Faults_Percent_Type;
      Fails_Now: Boolean := False;
   begin
      case Faults_Mode is
         when Vector =>
            Round_Number := (Round_Number mod TFV_Effective_Length) + 1;
            if The_Faults_Vector(Round_Number) then
               Fails_Now := True;
            else
               Fails_Now := False;
            end if;
         when Percent =>
            if Faults_Percent = 0 then
               Fails_Now := False;
            else
               Value := Random_Percent.Random (Percent_Generator);
               if Value <= Faults_Percent then
                  Fails_Now := True;
               else
                  Fails_Now := False;
               end if;
            end if;
         when None =>
            Fails_Now := False;
      end case;
      if Fails_Now then
         if Debug then Put_Line("SEND MISSES!"); end if;
      else
         if Debug then Put_Line("Going to send!"); end if;
         if Lower_Layer.Is_Null (From) then
            Lower_Layer.Send(To, Data);
         else
            Lower_Layer.Send(From, To, Data);
         end if;
         if Debug then Put_Line("Sent!"); end if;
      end if;
   end Send_Now;




   -- Retrieves from protected Buffer data stored in it by
   -- Handler_To_Buffer. Waits for data Timeout seconds, return
   -- with True in Expired if timeout expired.
   -- Actually does all work, with or without From
   procedure Receive_With_Or_Without_From (From:    out End_Point_Type;
                                           To:       in End_Point_Type;
                                           Data: access Buffer_Type;
                                           Timeout:  in Duration;
                                           Expired: out Boolean) is
      I : Index := Buffer.Locate (To);
   begin  -- Receive
      if Debug then
         Put_Line("entrando en el Receive, con index = " & I'Img);
      end if;

      Expired := False;

      select
-- jcenteno 20061017
-- changed from Data to Data.all'Access
-- to avoid Program_Error with gnat-4.0 in Ubuntu Dapper
         Buffer.Get(I)(From, To, Data.all'Access);
         if Debug then Put_Line("*** después de salir del Get"); end if;
      or
         delay Timeout;
         if Debug then Put_Line("### expiró el plazo"); end if;
         Expired := True;
      end select;

      if Debug then Put_Line("saliendo del   Receive"); end if;
   end Receive_With_Or_Without_From;



   -- Retrieves from protected Buffer data stored in it by
   -- Handler_To_Buffer. Waits for data Timeout seconds, return
   -- with True in Expired if timeout expired.
   -- Actually, simply calls Receive_With_Or_Without_From
   procedure Receive (To:       in End_Point_Type;
                      Data: access Buffer_Type;
                      Timeout:  in Duration;
                      Expired: out Boolean) is
      From: End_Point_Type;
   begin  -- Receive
      Receive_With_Or_Without_From (From, To, Data, Timeout, Expired);
   end Receive;



   -- Retrieves from protected Buffer data stored in it by
   -- Handler_To_Buffer. Waits for data Timeout seconds, return
   -- with True in Expired if timeout expired.
   -- Actually, simply calls Receive_With_Or_Without_From
   procedure Receive_From (From:    out End_Point_Type;
                           To:       in End_Point_Type;
                           Data: access Buffer_Type;
                           Timeout:  in Duration;
                           Expired: out Boolean) is
   begin  -- Receive
      Receive_With_Or_Without_From (From, To, Data, Timeout, Expired);
   end Receive_From;




   -- Retrieves from protected Buffer data stored in it by
   -- Handler_To_Buffer. Waits for data Duration'Last seconds
   procedure Receive(To:       in End_Point_Type;
                     Data: access Buffer_Type) is
      Timeout : Boolean;
   begin
      Receive (To, Data, Duration'Last, Timeout);
   end Receive;




   -- types and variables to handle simulated propagation delays
   type Delay_Type is (Fixed, Random, None);



   Constant_Propagation_Delay: Natural := 0;
   Min_Propagation_Delay: Natural := 0;
   Max_Propagation_Delay: Natural := 0;
   Propagation_Delay_Type: Delay_Type := None;

   -- Sets a fixed propagation delay of Delay_ms for each send
   procedure Set_Fixed_Propagation_Delay (Delay_ms : Natural) is
   begin
      Constant_Propagation_Delay := Delay_ms;
      Propagation_Delay_Type := Fixed;
   end;

   -- Sets a random propagation delay between Min_Delay_ms and Max_Delay_ms for each send
   procedure Set_Random_Propagation_Delay(Min_Delay_ms: Natural;
                                          Max_Delay_ms: Natural ) is
   begin
      if Min_Delay_ms > Max_Delay_ms then
        raise Random_Propagation_Delay_Error;
      end if;

      Min_Propagation_Delay := Min_Delay_ms;
      Max_Propagation_Delay := Max_Delay_ms;
      Propagation_Delay_Type := Random;
   end;



   type Entry_Status is (Free, To_Send);

   type Delay_Buff_Entry is record
      Send_Time   : Ada.Real_Time.Time;                   -- The time to send
      Origin      : Lower_Layer.Address_CA;               -- The origin
      Destination : Lower_Layer.Address_CA;               -- The destination
      Data        : aliased Buffer_Type(Max_Buffer_Size); -- The data to send
      Length      : Natural;
      Status      : Entry_Status:=FREE;                   -- entry status
      end record;

   type Delay_Buff is array (Integer Range <>) of Delay_Buff_Entry ;


   -- Protected object for accessing the buffer exclusively
   protected Delay_Buffer_Access is

      -- Adds one entry  to the buffer. Tells whether success.
      entry Add (From    : Lower_Layer.Address_CA;
                 To      : Lower_Layer.Address_CA;
                 Data    : access Buffer_Type);

      -- Checks and sends the mature buffer entries
      entry Check_And_Send (Die : out Boolean);
      procedure Set_Time_To_Die;
   private
      Time_To_Die : Boolean := false;
      Send_Buffer: Delay_Buff (1..300);
      Ocupacion: Integer := 0;
   end Delay_Buffer_Access;


   protected body Delay_Buffer_Access is

      procedure Set_Time_To_Die is
      begin
        Time_To_Die := true;
      end Set_Time_To_Die;

      entry Add (From :    in     Lower_Layer.Address_CA;
                     To :      in     Lower_Layer.Address_CA;
                     Data :    access Buffer_Type)
        when ocupacion < Send_Buffer'Last is

         use type Ada.Real_Time.Time;
         use type Ada.Real_Time.Time_Span;

         Ind : Integer;
         Now : Ada.Real_Time.Time;
         Maduration : Ada.Real_Time.Time_Span;
         Index: Integer;
         Actual_Delay: Natural;
      begin

         --Delay calculation, depending on the delay type
         case (Propagation_Delay_Type) is
            when Fixed =>
               Maduration := Ada.Real_Time.Milliseconds(Constant_Propagation_Delay);
            when Random =>
               Actual_Delay := Natural (Random_Delay.Random(Delay_Generator) *
                 Float(Max_Propagation_Delay - Min_Propagation_Delay) +
                                        Float(Min_Propagation_Delay));
               Maduration := Ada.Real_Time.Milliseconds(Actual_Delay);
            when None =>
               Maduration := Ada.Real_Time.Milliseconds(5);
         end case;

         Index := 0;
         loop
            Index := Index + 1;
            Ind := Index;
            exit when Send_Buffer(Ind).Status=Free;
         end loop;

         Now := Ada.Real_Time.Clock;
         Send_Buffer(Ind).Send_Time := Now + Maduration;
         Send_Buffer(Ind).Origin := From;
         Send_Buffer(Ind).Destination := To;
         Copy (Send_Buffer(Ind).Data'Access, Data);
         Send_Buffer(Ind).Status := To_Send;
         Ocupacion := Ocupacion + 1;

      end Add;


      entry Check_And_Send (Die : out Boolean)
      when ocupacion > 0 or Time_To_Die
      is
         use type Ada.Real_Time.Time;

         Now : Ada.Real_Time.Time;
         From, To : Lower_Layer.Address_CA ;

      begin
         Die := Time_To_Die;
         if not Die then
           Now :=  Ada.Real_Time.Clock;
           for Ind in 1..Send_Buffer'Last loop
              if (Send_Buffer(Ind).Status = To_Send  and then
                  Send_Buffer(Ind).Send_Time <= Now ) then
                 From := Send_Buffer(Ind).Origin;
                 To := Send_Buffer(Ind).Destination;
                 Send_Now (From, To, Send_Buffer(Ind).Data'Access);
                 Send_Buffer(Ind).Status := Free;
                 Ocupacion := Ocupacion-1;
              end if;
           end loop;
         end if;
      end Check_And_Send;

   end Delay_Buffer_Access;



   procedure Send_With_Or_Without_From (From:  in     End_Point_Type;
                                        To:    in     End_Point_Type;
                                        Data : access Buffer_Type ) is

   begin
      if  (Propagation_Delay_Type = None) then
         --sends inmediately
         Send_Now (From, To, Data);
      else

-- jcenteno 20061017
-- changed from Data to Data.all'Access
-- to avoid Program_Error with gnat-4.0 in Ubuntu Dapper
         Delay_Buffer_Access.Add (From, To, Data.all'Access);
      end if;
   end Send_With_Or_Without_From;



   -- Sends data inmediately or put it into the send buffer for mature
   -- Only calls Send_With_Or_Without_From (with null From) to do the real work
   procedure Send (To: in End_Point_Type;
                   Data : access Buffer_Type ) is
   begin
      Send_With_Or_Without_From (null, To, Data);
   end Send;

   -- Sends data inmediately or put it into the send buffer for mature
   -- Uses From End_Point to send through it.
   -- Only calls Send_With_Or_Without_From to do the real work
   procedure Send_From (From:  in     End_Point_Type;
                        To:    in     End_Point_Type;
                        Data : access Buffer_Type ) is

   begin
      Send_With_Or_Without_From (From, To, Data);
   end Send_From;


   -- The task which peridically check the buffer entries looking for
   -- the ready (mature) ones
   task type Send_Task;

   task body Send_Task is
     Time_To_Die: Boolean := false;
   begin
      while not Time_To_Die
      loop
         Delay_Buffer_Access.Check_And_Send (Time_To_Die);
      end loop;
   end Send_Task;


   procedure Finalize is
   begin
      Delay_Buffer_Access.Set_Time_To_Die;
      Misc_Util_Terminators.Execute_Terminators;
   end Finalize;


   T1 : Send_Task;

begin
   -- Initializes random generators
   Random_Percent.Reset (Percent_Generator);
   Random_Delay.Reset(Delay_Generator);

end Lower_Layer_UDP;
