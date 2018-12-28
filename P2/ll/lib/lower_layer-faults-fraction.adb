-- $Id: lower_layer-faults-fraction.adb,v 1.1 1997/04/05 00:44:02 jgb Exp $
--

package body Lower_Layer.Faults.Fraction is

   ------------------------------------------------------------------
   -- Set_Fault_Model
   ------------------------------------------------------------------

   procedure Set_Fault_Model (Faults: in Natural;
                              Total:  in Natural;
                              Start:  in Natural := 0;
                              Fault:  in Fault_Type := Any) is

      New_Fault_Mode: Fault_Model_CA := new Fault_Model;
      New_Fault_Info: Fault_Info_CA := new Fault_Info;
   begin
      Fault_Model (New_Fault_Mode.all).Faults := Faults;
      Fault_Model (New_Fault_Mode.all).Total := Total;
      Fault_Info (New_Fault_Info.all) :=
        (Current => Start);
      Set_Fault_Model (New_Fault_Mode, New_Fault_Info, Fault);
   end Set_Fault_Model;

   ------------------------------------------------------------------
   -- Should_Fail
   ------------------------------------------------------------------

   function Should_Fail (Model: in Fault_Model) return Boolean is

      Fail: Boolean;
   begin
      if Fault_Info (Model.Info.all).Current >= Model.Faults then
         -- No injection of faults
         Fail := False;
      else
         Fail := True;
      end if;
      Fault_Info (Model.Info.all).Current :=
        ((Fault_Info (Model.Info.all).Current + 1)
         mod Model.Total);
      return Fail;
   end Should_Fail;

end Lower_Layer.Faults.Fraction;
