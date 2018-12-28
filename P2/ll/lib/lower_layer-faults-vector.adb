-- $Id$
--

with Debugging;

package body Lower_Layer.Faults.Vector is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Faults.Vector";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   ------------------------------------------------------------------
   -- Set_Fault_Model
   ------------------------------------------------------------------

   procedure Set_Fault_Model (Faults_Vector: in Faults_Vector_Type;
                              Fault: in Fault_Type := Any;
                              Start: in Natural := 1) is

      New_Fault_Mode: Fault_Model_CA :=
       new Fault_Model (Vector_Length => Faults_Vector'Length);
      New_Fault_Info: Fault_Info_CA :=
       new Fault_Info'(Current       => Faults_Vector'First - Start + 1);

   begin
--      Fault_Model (New_Fault_Mode.all).Vector_Length := Faults_Vector'Length;
      Fault_Model (New_Fault_Mode.all).Faults_Vector := Faults_Vector;
      Set_Fault_Model (New_Fault_Mode, New_Fault_Info, Fault);
   end Set_Fault_Model;

   ------------------------------------------------------------------
   -- Should_Fail
   ------------------------------------------------------------------

   function Should_Fail (Model: in Fault_Model) return Boolean is

      Return_Value: Boolean;
   begin
      Put_Line ("Vector_Length: " & Natural'Image (Model.Vector_Length));
      -- Determine return value
      Return_Value :=
        Model.Faults_Vector (Fault_Info (Model.Info.all).Current);
      Put_Line
        ("Current: " &
         Natural'Image (Fault_Info (Model.Info.all).Current) &
         ", Should_Fail: " &
         Boolean'Image (Model.Faults_Vector
                        (Fault_Info (Model.Info.all).Current))
         );
      -- Prepare return value for next call
      Fault_Info (Model.Info.all).Current :=
        (Fault_Info (Model.Info.all).Current mod Model.Vector_Length) + 1;
      return Return_Value;
   exception
      when Except: others =>
         Put_Line ("Unexpected exception raised");
         raise;
   end Should_Fail;

end Lower_Layer.Faults.Vector;
