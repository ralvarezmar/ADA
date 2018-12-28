-- $Id$
--

package Lower_Layer.Faults.Vector is

   -- Set fault model so that Faults faults will be produced
   -- in a deterministic way following a "faults vector"
   --
   -- A faults vector is an array of boolean components. A
   -- subsequent Should_Fail is going to return true if there is a True value
   -- into vector's position corresponding to current number of Should_Fail
   -- invocations (modulus the vector's length).

   type Faults_Vector_Type is array (Positive range <>) of Boolean;

   procedure Set_Fault_Model (Faults_Vector: in Faults_Vector_Type;
                              Fault: in Fault_Type := Any;
                              Start: in Natural := 1);

private

   type Fault_Model (Vector_Length: Natural) is
     new Faults.Fault_Model with record
      Faults_Vector: Faults_Vector_Type (1 .. Vector_Length);
   end record;

   function Should_Fail (Model: in Fault_Model) return Boolean;


   type Fault_Info is new Faults.Fault_Info with record
      Current: Natural;
   end record;

end Lower_Layer.Faults.Vector;
