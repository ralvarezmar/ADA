-- $Id: lower_layer-faults.adb,v 1.4 1998/01/19 12:46:44 jgb Exp $
--

with Ada.Text_IO;
with Debugging;

package body Lower_Layer.Faults is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Lower_Layer.Faults";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);

   ---------------------------------------------------------------------
   -- Should_Fail.
   ---------------------------------------------------------------------

   function Should_Fail (Fault: in Fault_Type := Any) return Boolean is

   begin
      if Current_Fault_Model (Fault) /= null and then
        Should_Fail (Current_Fault_Model(Fault).all) then
         Put_Line ("Should_Fail: True  (failure model: " &
                   Fault_Type'Image (Fault) & ")");
         return True;
      else
         Put_Line ("Should_Fail: False (failure model: " &
                   Fault_Type'Image (Fault) & ")");
         return False;
      end if;
   end Should_Fail;

   ---------------------------------------------------------------------
   -- Print_Failed
   ---------------------------------------------------------------------

   procedure Print_Failed (Failed: in Boolean;
                           Action: in Fault_Action) is

   begin
      if Should_Print then
         Ada.Text_IO.Put ("  ** Failure injector ");
         case Action is
            when Sending   =>
               Ada.Text_IO.Put ("  .   .   .   .   .   (sending): ");
            when Receiving =>
               Ada.Text_IO.Put ("(receiving): ");
         end case;
         if Failed then
            Ada.Text_IO.Put_Line ("FAILURE");
         else
            Ada.Text_IO.Put_Line ("CORRECT");
         end if;
      end if;
   end Print_Failed;


   ---------------------------------------------------------------------
   -- Print_Information.
   ---------------------------------------------------------------------

   procedure Print_Information (Print: in Boolean := True) is

   begin
      Should_Print := Print;
   end Print_Information;

   ---------------------------------------------------------------------
   -- Should_Fail (Fault_Model).
   ---------------------------------------------------------------------

   function Should_Fail (Model: in Fault_Model) return Boolean is

   begin
      return False;
   end Should_Fail;

   -------------------------------------------------------------------
   -- Set_Fault_Model.
   -------------------------------------------------------------------

   procedure Set_Fault_Model (Model: in Fault_Model_CA;
                              Info:  in Fault_Info_CA;
                              Fault: in Fault_Type := Any) is

   begin
      Current_Fault_Model (Fault) := Model;
      if (Model /= null) then
        Fault_Model (Current_Fault_Model (Fault).all).Info := Info;
      end if;
--      Current_Fault_Info  (Fault) := Info;
   end Set_Fault_Model;

end Lower_Layer.Faults;
