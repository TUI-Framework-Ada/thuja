package Shortcut_Handler is

   Max_Sequence_Length : constant := 3;
   Max_Shortcuts      : constant := 4;

   type Key_Sequence_t is array (1 .. Max_Sequence_Length) of Character;

   type Result_Kind_t is (No_Result, Shortcut_Activated, Keys_Passed_Through);

   type Handler_Result_t is record
      Kind           : Result_Kind_t := No_Result;
      Keys           : Key_Sequence_t := [others => Character'Val (0)];
      Key_Count      : Natural := 0;
      Shortcut_Index : Natural := 0;
   end record;

   --  Initialize the shortcut handler with the given timeout duration.
   --  Timeout is the maximum allowed gap between consecutive keystrokes
   --  in a shortcut sequence. Default is 100ms.
   procedure Initialize (Timeout : Duration := 0.1);

   --  Feed a key press into the state machine.
   --  Returns Shortcut_Activated if a shortcut completes,
   --  Keys_Passed_Through if buffered keys fail to match any shortcut,
   --  or No_Result if more keys are needed.
   function Process_Key (C : Character) return Handler_Result_t;

   --  Check whether the current buffered sequence has timed out.
   --  Call this periodically in the main loop so that pending
   --  shortcuts or orphaned keys are resolved even without new input.
   function Check_Timeout return Handler_Result_t;

   --  Get a human-readable name for a shortcut by its table index.
   function Get_Shortcut_Name (Index : Natural) return String;

end Shortcut_Handler;
