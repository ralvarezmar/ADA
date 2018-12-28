with Lower_Layer.Inet.UDP.Uni;
with Ada.Streams;

package Lower_Layer_UDP is

   -- subtype for declaring Buffer_Type vars:
   --     Buffer: aliased Buffer_Type(1024);
   subtype Buffer_Type is Lower_Layer.Stream;

   -- resets a buffer, emptying all data out of it
   procedure Reset(A_Buffer: in out Buffer_Type);

   -- copies into a Destination Buffer a Source buffer
   procedure Copy (Destination: access Buffer_Type;
                   Source:      access Buffer_Type);

   -- gives a string representation of a Buffer
   function Image (A_Buffer: in Buffer_Type) return String;



   -- subtype for declaring communications end-points
   subtype End_Point_Type is Lower_Layer.Address_CA;

   -- gives a string representation of an End_Point
   function Image (An_EP: in End_Point_Type) return String;

   -- checks if an End_Point contains a value
   function Is_Null (An_EP: in End_Point_Type) return Boolean;

   -- operator to compare End_Points
   function "=" (Left, Right: in End_Point_Type) return Boolean;



   -- type for declaring access to procedures which will
   -- handle reception of incoming data
   type Handler_Type is
      access procedure (From: in End_Point_Type;
                        To:   in     End_Point_Type;
                        Data: access Buffer_Type);

   -- Builds an End_Point from an IP address and a port number
   -- It builds either an unicast or a multicast End_Point,
   -- depending on IP parameter
   function Build(IP: String; Port: Natural) return End_Point_Type;



   -- Returns a String containing the IP address
   -- associated with a host name
   function To_IP (Name: in String) return String;

   -- Returns the running host name
   function Get_Host_Name return String;


   -- Binds to an End_Point in order to be able to receive data on it.
   -- (a) If no handler is provided, a call to Receive procedure is
   --     needed to receive the incoming data, with timeouts feature
   -- (b) If A_Handler is provided, such procedure will be
   --     asynchronously called when some data arrives and no call
   --     to Receive should be done.
   procedure Bind(An_EP:     in End_Point_Type;
                  A_Handler: in Handler_Type := null);

   -- Builds an unicast End_Point for the local machine, and binds to it
   -- to be able to receive data on it.
   -- (a) If no handler is provided, a call to Receive procedure is
   --     needed to receive the incoming data, with timeouts feature
   -- (b) If A_Handler is provided, such procedure will be
   --     asynchronously called when some data arrives and no call
   --     to Receive should be done.
   procedure Bind_Any(An_EP: out End_Point_Type;
                      A_Handler: in Handler_Type := null);

   -- Disables the bindind to an End_Point.
   procedure Unbind(An_EP: in End_Point_Type);


   -- A faults vector is an array of boolean components. A
   -- subsequent Send is going to miss the packet if there is a True value
   -- into vector's position corresponding to current number of Send
   -- invocations (modulus the vector's length).
   type Faults_Vector_Type is array (Positive range <>) of Boolean;
   procedure Set_Faults_Vector (The_Vector: in Faults_Vector_Type);
   function Get_Faults_Vector return Faults_Vector_Type;


   -- A faults percent is a number between 0 and 100 indicating the random
   -- probability that a subsequent Send is going to miss
   subtype Faults_Percent_Type is Integer range 0..100;
   procedure Set_Faults_Percent(P: in Faults_Percent_Type);
   function Get_Faults_Percent return Faults_Percent_Type;


   -- Sets a simulated fixed propagation time. The send function will wait
   -- this ms amount before actually send the data
   procedure Set_Fixed_Propagation_Delay(Delay_ms : Natural);


   -- Sets a simulated random propagation time. The send function will wait
   -- a time choosed randomly betweens the given limits before actually send
   -- the data over the channel
   procedure Set_Random_Propagation_Delay(Min_Delay_ms : Natural;
                                          Max_Delay_Ms : Natural);


   -- Sends buffer contents (accessed by Data) to To End_Point.
   procedure Send(To:     in     End_Point_Type;
                  Data:   access Buffer_Type);

   -- Sends buffer contents (accessed by Data) to To End_Point.
   -- Uses From End_Point to send through it.
   procedure Send_From (From:     in     End_Point_Type;
                        To:     in     End_Point_Type;
                        Data:   access Buffer_Type);


   -- Retrieves data sent to To End_Point during next Timeout seconds,
   -- and places it into the buffer accesed by Data. If no data arrives
   -- when Timeout expires, Expired is returned with False and nothing is
   -- put into the buffer.
   procedure Receive(To:       in End_Point_Type;
                     Data: access Buffer_Type;
                     Timeout:  in Duration;
                     Expired: out Boolean);

   -- Retrieves data sent to To End_Point during Integer'Last seconds,
   -- and places it into the buffer accesed by Data.
   procedure Receive(To:       in End_Point_Type;
                     Data: access Buffer_Type);


   -- Retrieves data sent to To End_Point during next Timeout seconds,
   -- and places it into the buffer accesed by Data. If no data arrives
   -- when Timeout expires, Expired is returned with False and nothing is
   -- put into the buffer.
   -- From End_Point returns the origin of Data
   procedure Receive_From (From:       out End_Point_Type;
                           To:       in End_Point_Type;
                           Data: access Buffer_Type;
                           Timeout:  in Duration;
                           Expired: out Boolean);


   -- Finalizes Lower_Layer internal mechanics
   procedure Finalize;

end Lower_Layer_UDP;
