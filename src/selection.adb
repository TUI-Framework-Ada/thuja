--  selection.adb
--  Per-widget command sequence state machine implementation.

with Ada.Characters.Handling;

package body Selection is

   package SU renames Ada.Strings.Unbounded;

   --  The active command table (swapped on focus change)
   Active_Commands : Widget_Command_Vectors.Vector;
   Commands_Active : Boolean := False;

   --  Internal buffer of keys received so far
   Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;

   --  Find the first command whose keys exactly match the buffer.
   --  Returns the 1-based index, or 0 if no match.
   function Find_Exact_Match return Natural_t is
      use SU;
   begin
      for I in 0 .. Natural_t (Active_Commands.Length) - 1 loop
         if Active_Commands (I).Keys = Buffer then
            return I + 1;
         end if;
      end loop;
      return 0;
   end Find_Exact_Match;

   --  Return True if the current buffer is a prefix of at least one command.
   function Is_Prefix_Of_Any return Boolean is
      Buf_Str : constant String_t := SU.To_String (Buffer);
      Buf_Len : constant Natural_t := Buf_Str'Length;
   begin
      for I in 0 .. Natural_t (Active_Commands.Length) - 1 loop
         declare
            Cmd_Str : constant String_t :=
               SU.To_String (Active_Commands (I).Keys);
         begin
            if Cmd_Str'Length >= Buf_Len and then
               Cmd_Str (Cmd_Str'First .. Cmd_Str'First + Buf_Len - 1)
                  = Buf_Str
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Prefix_Of_Any;

   procedure Clear_Buffer is
   begin
      Buffer := SU.Null_Unbounded_String;
   end Clear_Buffer;

   --  Validate that no command sequence is a prefix of another.
   --  Raises Program_Error if a conflict exists.
   procedure Validate_No_Prefix_Conflicts (Commands : Widget_Command_Vectors.Vector) is
      Count : constant Natural_t := Natural_t (Commands.Length);
   begin
      for I in 0 .. Count - 1 loop
         for J in 0 .. Count - 1 loop
            if I /= J then
               declare
                  Seq_I : constant String_t := SU.To_String (Commands (I).Keys);
                  Seq_J : constant String_t := SU.To_String (Commands (J).Keys);
               begin
                  if Seq_J'Length > Seq_I'Length and then
                     Seq_J (Seq_J'First .. Seq_J'First + Seq_I'Length - 1) = Seq_I
                  then
                     raise Program_Error with
                        "Widget command prefix conflict: """ &
                        SU.To_String (Commands (I).Keys) &
                        """ is a prefix of """ &
                        SU.To_String (Commands (J).Keys) & """";
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Validate_No_Prefix_Conflicts;

   --  Public API

   procedure Activate_Widget_Commands (Commands : Widget_Command_Vectors.Vector) is
   begin
      Validate_No_Prefix_Conflicts (Commands);
      Active_Commands := Commands;
      Commands_Active := Natural_t (Commands.Length) > 0;
      Clear_Buffer;
   end Activate_Widget_Commands;

   procedure Deactivate_Widget_Commands is
   begin
      Active_Commands.Clear;
      Commands_Active := False;
      Clear_Buffer;
   end Deactivate_Widget_Commands;

   function Has_Active_Commands return Boolean is
   begin
      return Commands_Active;
   end Has_Active_Commands;

   function Process_Key (C : Character_t) return Handler_Result_t is
      Lower_C   : constant Character_t :=
         Ada.Characters.Handling.To_Lower (C);
      Match_Idx : Natural_t;
      R         : Handler_Result_t;
   begin
      if not Commands_Active then
         return R;  --  No_Result
      end if;

      SU.Append (Buffer, Lower_C);

      --  Check for exact match first
      Match_Idx := Find_Exact_Match;
      if Match_Idx > 0 then
         R.Kind := Command_Activated;
         R.Keys := Active_Commands (Match_Idx - 1).Keys;
         R.Command_Index := Match_Idx;
         R.Command_Name := Active_Commands (Match_Idx - 1).Name;
         Clear_Buffer;
         return R;
      end if;

      --  Check if buffer could still become a match
      if Is_Prefix_Of_Any then
         return R;  --  No_Result: keep buffering
      end if;

      --  No match possible — pass through buffered keys
      R.Kind := Keys_Passed_Through;
      R.Keys := Buffer;
      Clear_Buffer;
      return R;
   end Process_Key;

   procedure Reset is
   begin
      Clear_Buffer;
   end Reset;

   function Get_Pending_Buffer return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Buffer;
   end Get_Pending_Buffer;

end Selection;
