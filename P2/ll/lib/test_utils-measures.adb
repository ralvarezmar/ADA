-- $Id: test_utils-measures.adb,v 1.2 1998/02/10 20:20:26 jgb Exp $
--

with Ada.Calendar;

with Text_IO;

package body Test_Utils.Measures is

   Time_A, Time_B: Ada.Calendar.Time;
   Elapsed: Duration;

   procedure Start_Measure is

   begin
      Time_A := Ada.Calendar.Clock;
   end Start_Measure;

   procedure End_Measure (Message:       in String;
                          Num_Loops:     in Natural;
                          Short_Message: in String) is

      -- For "-".
      use type Ada.Calendar.Time;

   begin
      Time_B := Ada.Calendar.Clock;
      Elapsed := Time_B - Time_A;
      Text_IO.Put_Line
        (Duration'Image (Elapsed) &
         " sec. (" &
         Duration'Image (Elapsed * 1_000 / Num_Loops) &
         " msec. for each " & Short_Message & "): " & Message);
   end End_Measure;

end Test_Utils.Measures;
