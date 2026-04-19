with Ada.Strings.Unbounded;

package Command_Sequence_Handling is

   --  Subtypes for standard Ada types
   subtype Character_t is Character;
   subtype Boolean_t is Boolean;
   subtype Natural_t is Natural;
   subtype String_t is String;

   type Result_Kind_t is (No_Result, Command_Activated, Keys_Passed_Through);

   type Handler_Result_t is record
      Kind          : Result_Kind_t := No_Result;
      Keys          : Ada.Strings.Unbounded.Unbounded_String :=
                        Ada.Strings.Unbounded.Null_Unbounded_String;
      Command_Index : Natural_t := 0;
   end record;

   --  Initialize the command sequence handler (resets internal state).
   procedure Initialize;

   --  Feed a key press into the state machine.
   --  Returns Command_Activated if a command sequence completes,
   --  Keys_Passed_Through if buffered keys fail to match any command,
   --  or No_Result if more keys are needed.
   function Process_Key (C : Character_t) return Handler_Result_t;

   --  Get a human-readable name for a command by its table index.
   function Get_Command_Name (Index : Natural_t) return String_t;

end Command_Sequence_Handling;
