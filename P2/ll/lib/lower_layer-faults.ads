-- $Id: lower_layer-faults.ads,v 1.1 1997/04/05 00:44:05 jgb Exp $
--

-- Models of fault injection.

package Lower_Layer.Faults is

   -- We can set fault injections for sending, receiving of any
   --
   type Fault_Type is (Send, Receive, Any);

   -- There are two actions where a failure can occur: send and receive
   --
   type Fault_Action is (Sending, Receiving);

   -- Function to be called before dropping a message
   --
   function Should_Fail (Fault: in Fault_Type := Any) return Boolean;

   -- Function for selecting the printing of some information each
   --  a package is selected to fail.
   procedure Print_Information (Print: in Boolean := True);


   -- Root type for including info related to faults.
   --
   type Fault_Info is tagged null record;

   type Fault_Info_CA is access Fault_Info'Class;

   -- Root type for specifying faults (if not extended, no faults).
   --
   type Fault_Model is tagged record
      Info: Fault_Info_CA;
   end record;

   -- Fault procedure (should fail a message?). Primitive of Fault_Model
   --
   function Should_Fail (Model: in Fault_Model) return Boolean;

   -- Print a failure message
   --
   procedure Print_Failed (Failed: in Boolean;
                           Action: in Fault_Action);

   -- Fault models are typed variables.
   --
   type Fault_Model_CA is access Fault_Model'Class;

   -- Install a given fault model for a given fault type
   --
   procedure Set_Fault_Model (Model: in Fault_Model_CA;
                              Info:  in Fault_Info_CA;
                              Fault: in Fault_Type := Any);

private

   -------------------------------------------------------------------
   -- Fault model.
   -------------------------------------------------------------------

   -- Default is no faults.
   --
   Current_Fault_Model: array (Fault_Type'Range) of Faults.Fault_Model_CA
     := (others => null);

--   Current_Fault_Info: array (Fault_Type'Range) of Faults.Fault_Info_CA
--     := (others => null);

   Should_Print: Boolean := False;

end Lower_Layer.Faults;
