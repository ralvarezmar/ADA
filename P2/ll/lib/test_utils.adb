-- $Id: test_utils.adb,v 1.5 1997/01/21 16:52:44 jgb Exp $
--

-- For printing result message.
with Text_IO;

-- For terminating.
with Misc_Util_Terminators;

with Debugging;

package body Test_Utils is


   --------------------------------------------------------
   -- Evaluate_Results
   --------------------------------------------------------

   procedure Evaluate_Results (Correct: in     String;
			       Actual:  in     String;
			       Passed:  in out Boolean) is

   begin
      if Passed = True then
	 Passed := (Correct = Actual);
      end if;
   end Evaluate_Results;


   --------------------------------------------------------
   -- Put_Results
   --------------------------------------------------------

   procedure Put_Results (Name:   in String;
			  Passed: in Boolean) is

   begin
      Text_IO.Put (Name & ": ");
      if Passed then
	 Text_IO.Put_Line ("PASSED");
      else
	 Text_IO.Put_Line ("NOT PASSED");
      end if;
   end Put_Results;

   --------------------------------------------------------
   -- Put_Results_And_Terminate.
   --------------------------------------------------------

   procedure Put_Results_And_Terminate (Name:   in String;
					Passed: in Boolean) is

   begin
      Put_Results (Name, Passed);
      Misc_Util_Terminators.Execute_Terminators;
   end Put_Results_And_Terminate;

   --------------------------------------------------------
   -- Show_And_Evaluate.
   --------------------------------------------------------

   procedure Show_And_Evaluate_Gen (Result:  in     String;
				    Correct: in     String;
				    Passed:  in out Boolean;
				    Message: in     String := "") is

      procedure Put_Line is new Debugging.Put_Line_Gen
	(Name => Name, Debug => Debug);

   begin
      Evaluate_Results (Result, Correct, Passed);
      Put_Line (Result & " (Passed: " & Boolean'Image (Passed) & ")",
		Message); 
   end Show_And_Evaluate_Gen;

   procedure Show_And_Evaluate (Name:    in     String;
				Debug:   in     Boolean;
				Result:  in     String;
				Correct: in     String;
				Passed:  in out Boolean;
				Message: in     String := "") is

   begin
      Evaluate_Results (Result, Correct, Passed);
      Debugging.Put_Line 
	(Name & Message, Debug,
	 Result & " (Passed: " & Boolean'Image (Passed) & ")");
   end Show_And_Evaluate;
      

end Test_Utils;
