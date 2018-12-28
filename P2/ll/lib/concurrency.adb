-- $Id: concurrency.adb,v 1.7 1997/12/30 11:20:42 jgb Exp $
--

with Misc_Util_Accesses;
with Debugging;

package body Concurrency is

   -----------------------------------------------------------------------
   -- Debugging.
   -----------------------------------------------------------------------

   -- For turning debugging on or off.
   Debug: Boolean := False;
   -- Name of the package, for debbugging.
   Name:  String  := "Concurrency";

   procedure Put_Line is new Debugging.Put_Line_Gen
     (Name => Name, Debug => Debug);
   procedure Put_Line_Error is new Debugging.Put_Line_Error_Gen
     (Name => Name);
   procedure Put_Exception_Error is new Debugging.Put_Exception_Error_Gen
     (Name => Name);

   ------------------------------------------------------------------
   -- Mutex type.
   ------------------------------------------------------------------

   use type Ada.Task_Identification.Task_Id;

   protected body Mutex is

-- XXX
-- Old Enter entry (incorrect) code: compiles and works with gnat <= 4.2
-- with gnat 4.3 generates this warning (and does not work):
-- warning: "Current_Task" should not be used in entry body (RM C.7(17))
-- warning: Program_Error will be raised at run time
--
--      entry Enter when
--        (Count = 1) or else
--        (Ada.Task_Identification.Current_Task = Owner) is

--      begin
--         Put_Line ("Entering, Count: " & Natural'Image (Count),
--                   " (Mutex.Enter)");
--         if Count = 0 then
--            -- I'm the owner, so just increment Owner_Count.
--            Owner_Count := Owner_Count + 1;
--         else
--            Count := 0;
--            Owner := Enter'Caller;
--         end if;
--      end Enter;
--
-- XXX
-- Now we have two entries:
--      Enter:          Gives access if Enter'Caller = Mutex.Owner,
--                      requeues to Enter_Other otherwise
--      Requeued_Enter: Gives access when Count=1. Should only be called
--                      by requeueing from Enter if Enter'Caler <> Mutex.Owner
--
     entry Enter (Id: Ada.Task_Identification.Task_Id)
      when True is
     begin
        if Id = Owner then
           Put_Line ("Entering, Count: " & Natural'Image (Count),
                     " (Mutex.Enter)");
           -- I'm the owner, so just increment Owner_Count.
           Owner_Count := Owner_Count + 1;
        else
           requeue Requeued_Enter;
        end if;
     end Enter;

     entry Requeued_Enter (Id: Ada.Task_Identification.Task_Id)
      when Count = 1 is
     begin
        Put_Line ("Entering, Count: " & Natural'Image (Count),
                  " (Mutex.Enter)");
        Count := 0;
        Owner := Id;
     end Requeued_Enter;



     entry Leave when Count = 0 is

     begin
        Put_Line ("Entering, Count: " & Natural'Image (Count),
                  " (Mutex.Leave)");
        if Owner_Count > 0 then
           Owner_Count := Owner_Count - 1;
        else
           Count := 1;
           Owner := Ada.Task_Identification.Null_Task_Id;
        end if;
     end Leave;

     function Image return String is

     begin
        return "Count: " & Natural'Image (Count) &
         ", Count_Owner: " & Natural'Image (Owner_Count) &
         ", Owner: " & Ada.Task_Identification.Image (Owner);
     end Image;

   end Mutex;

   function Image (A_Mutex: in Mutex_A) return String is

      function Is_Null is new Misc_Util_Accesses.Is_Null_A (Mutex, Mutex_A);
   begin
      if Is_Null (A_Mutex) then
         return A_Mutex.Image;
      else
         return "null";
      end if;
   end Image;

   ------------------------------------------------------------------
   -- Semaphore type.
   ------------------------------------------------------------------

   protected body Semaphore is

      entry Reset (A_Count: Natural := 0) when True is

      begin
         Count := A_Count;
      end Reset;

      -- Wait.
      --
      entry Wait when Count > 0 is

      begin
         Count := Count - 1;
      end Wait;

      -- Signal.
      --
      entry Signal when True is

      begin
         Count := Count + 1;
      end Signal;

      -- Image.
      --
      function Image return String is

      begin
         return Natural'Image (Count);
      end Image;

   end Semaphore;

   ------------------------------------------------------------------
   -- Timed_Wait (Semaphore_A).
   ------------------------------------------------------------------

   procedure Timed_Wait (A_Semaphore: in  Semaphore_A;
                         Timeout:     in  Duration;
                         Expired:     out Boolean) is

   begin
      select
         A_Semaphore.Wait;
         Expired := False;
      or
         delay Timeout;
         Expired := True;
      end select;
   end Timed_Wait;

   ------------------------------------------------------------------
   -- Timed_Wait (Semaphore_A, short version).
   ------------------------------------------------------------------

   procedure Timed_Wait (A_Semaphore: in  Semaphore_A;
                         Timeout:     in  Duration) is

      Expired: Boolean;
   begin
      Timed_Wait (A_Semaphore, Timeout, Expired);
   end Timed_Wait;

   ------------------------------------------------------------------
   -- Image (Semaphore_A).
   ------------------------------------------------------------------

   function Image (A_Semaphore: in Semaphore_A) return String is

      function Is_Null is new Misc_Util_Accesses.Is_Null_A
        (Semaphore, Semaphore_A);
   begin
      if Is_Null (A_Semaphore) then
         return A_Semaphore.Image;
      else
         return "null";
      end if;
   end Image;

end Concurrency;
