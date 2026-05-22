--  selection.ads
--  Per-widget command sequence state machine.
--  Mirrors the pattern in Command_Sequence_Handling but operates on
--  a dynamically swapped command table that changes when widget focus changes.

with Ada.Strings.Unbounded;
with Components; use Components;

package Selection is

   subtype Character_t is Character;
   subtype Natural_t is Natural;
   subtype String_t is String;

   type Result_Kind_t is (No_Result, Command_Activated, Keys_Passed_Through);

   type Handler_Result_t is record
      Kind          : Result_Kind_t := No_Result;
      Keys          : Ada.Strings.Unbounded.Unbounded_String :=
                        Ada.Strings.Unbounded.Null_Unbounded_String;
      Command_Index : Natural_t := 0;
      Command_Name  : Ada.Strings.Unbounded.Unbounded_String :=
                        Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;

   --  Load a widget's command table. Clears the key buffer.
   procedure Activate_Widget_Commands (Commands : Widget_Command_Vectors.Vector);

   --  Clear the active command table. Clears the key buffer.
   procedure Deactivate_Widget_Commands;

   --  Returns True if a widget command table is currently active.
   function Has_Active_Commands return Boolean;

   --  Feed a key press into the widget command state machine.
   --  Returns Command_Activated if a command sequence completes,
   --  Keys_Passed_Through if buffered keys fail to match,
   --  or No_Result if more keys are needed.
   function Process_Key (C : Character_t) return Handler_Result_t;

   --  Clear the key buffer without changing the active command table.
   procedure Reset;

   --  Return the current pending key buffer contents.
   --  Useful for displaying "Buffering: A..." in a status area.
   function Get_Pending_Buffer return Ada.Strings.Unbounded.Unbounded_String;

end Selection;
