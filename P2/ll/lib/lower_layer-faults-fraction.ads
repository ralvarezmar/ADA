-- $Id: lower_layer-faults-fraction.ads,v 1.1 1997/04/05 00:44:03 jgb Exp $
--

package Lower_Layer.Faults.Fraction is

   -- Set fault model so that Faults faults will be produced
   --  every Total faults (in a deterministic way).
   --
   procedure Set_Fault_Model (Faults: in Natural;
                              Total:  in Natural;
                              Start:  in Natural := 0;
                              Fault:  in Fault_Type := Any);

private

   -- Produce Faults faults every Total messages.
   --
   type Fault_Model is new Faults.Fault_Model with record
      Faults:  Natural;
      Total:   Natural;
   end record;

   function Should_Fail (Model: in Fault_Model) return Boolean;

   type Fault_Info is new Faults.Fault_Info with record
      Current: Natural;
   end record;

end Lower_Layer.Faults.Fraction;
