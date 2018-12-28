-- Simple interface to Lower_Layer.Inet.*
--

with Lower_Layer.Inet.UDP.Uni;
with Lower_Layer.Inet.TCP;
with Ada.Streams;

package Lower_Layer_Inet is

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
      access procedure (To:   in     End_Point_Type;
                        Data: access Buffer_Type);

   -- Builds an End_Point from an IP address and a port number
   -- It builds either an unicast or a multicast End_Point,
   -- depending on IP parameter
   function Build_UDP (IP: String; Port: Natural) return End_Point_Type;
   function Build_TCP (IP: String; Port: Natural) return End_Point_Type;

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

   -- A faults vector is an array of less than 100 boolean components A
   -- subsequent Send is going to miss the packet if there is a True value
   -- into vector's position corresponding to current number of Send
   -- invocations (modulus the vector's length).
   type Faults_Vector_Type is array (Positive range <>) of Boolean;

   procedure Set_Faults_Vector(Vector: in Faults_Vector_Type);
   function Get_Faults_Vector return Faults_Vector_Type;

   -- Sends buffer contents (accessed by Data) to To End_Point.
   procedure Send(To:     in     End_Point_Type;
                  Data:   access Buffer_Type);

   -- Retrieves data sent to To End_Point during next Timeout seconds,
   -- and places it into the buffer accesed by Data. If no data arrives
   -- when Timeout expires, Expired is returned with False and nothing is
   -- put into the buffer.
   procedure Receive(To:       in End_Point_Type;
                     Data: access Buffer_Type;
                     Timeout:  in Duration;
                     Expired: out Boolean);
end Lower_Layer_UDP;
