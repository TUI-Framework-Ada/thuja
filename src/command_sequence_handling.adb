with Ada.Characters.Handling;
with Ada.Containers.Vectors;

package body Command_Sequence_Handling is

   package SU renames Ada.Strings.Unbounded;

   --  A registered command: key sequence + display name
   type Command_Entry_t is record
      Keys : SU.Unbounded_String;
      Name : SU.Unbounded_String;
   end record;

   package Command_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural_t,
       Element_Type => Command_Entry_t);

   --  The command table (populated at elaboration)
   Command_Table : Command_Vectors.Vector;

   --  Internal buffer of keys received so far
   Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;

   ------------------------------------------------------------
   --  Internal helpers
   ------------------------------------------------------------

   --  Format a key sequence for display: "ab" -> "A -> B"
   function Format_Sequence (Keys : SU.Unbounded_String) return String_t is
      S      : constant String_t := SU.To_String (Keys);
      Result : SU.Unbounded_String;
   begin
      for I in S'Range loop
         if I > S'First then
            SU.Append (Result, " -> ");
         end if;
         SU.Append (Result, Ada.Characters.Handling.To_Upper (S (I)));
      end loop;
      return SU.To_String (Result);
   end Format_Sequence;

   --  Validate that no registered command sequence is a prefix of another.
   --  Raises Program_Error with a descriptive message if a conflict exists.
   procedure Validate_No_Prefix_Conflicts is
      Count : constant Natural_t := Natural_t (Command_Table.Length);
   begin
      for I in 0 .. Count - 1 loop
         for J in 0 .. Count - 1 loop
            if I /= J then
               declare
                  Seq_I : constant String_t :=
                     SU.To_String (Command_Table (I).Keys);
                  Seq_J : constant String_t :=
                     SU.To_String (Command_Table (J).Keys);
               begin
                  if Seq_J'Length > Seq_I'Length and then
                     Seq_J (Seq_J'First .. Seq_J'First + Seq_I'Length - 1)
                        = Seq_I
                  then
                     raise Program_Error with
                        "Command sequence conflict: """ &
                        Format_Sequence (Command_Table (I).Keys) &
                        """ is a prefix of """ &
                        Format_Sequence (Command_Table (J).Keys) &
                        """. Remove the higher-order (longer) sequence " &
                        "to resolve.";
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Validate_No_Prefix_Conflicts;

   --  Find the first command whose keys exactly match the buffer.
   --  Returns the 1-based index, or 0 if no match.
   function Find_Exact_Match return Natural_t is
      use SU;
   begin
      for I in 0 .. Natural_t (Command_Table.Length) - 1 loop
         if Command_Table (I).Keys = Buffer then
            return I + 1;
         end if;
      end loop;
      return 0;
   end Find_Exact_Match;

   --  Return True if the current buffer is a prefix of at least one command.
   function Is_Prefix_Of_Any return Boolean_t is
      Buf_Str : constant String_t := SU.To_String (Buffer);
      Buf_Len : constant Natural_t := Buf_Str'Length;
   begin
      for I in 0 .. Natural_t (Command_Table.Length) - 1 loop
         declare
            Cmd_Str : constant String_t :=
               SU.To_String (Command_Table (I).Keys);
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

   procedure Clear_State is
   begin
      Buffer := SU.Null_Unbounded_String;
   end Clear_State;

   function Make_Command_Result (Index : Natural_t) return Handler_Result_t is
      R : Handler_Result_t;
   begin
      R.Kind := Command_Activated;
      R.Keys := Command_Table (Index - 1).Keys;
      R.Command_Index := Index;
      return R;
   end Make_Command_Result;

   function Make_Pass_Through_Result return Handler_Result_t is
      R : Handler_Result_t;
   begin
      R.Kind := Keys_Passed_Through;
      R.Keys := Buffer;
      return R;
   end Make_Pass_Through_Result;

   ------------------------------------------------------------
   --  Public API
   ------------------------------------------------------------

   procedure Initialize is
   begin
      Clear_State;
   end Initialize;

   function Process_Key (C : Character_t) return Handler_Result_t is
      Lower_C   : constant Character_t :=
         Ada.Characters.Handling.To_Lower (C);
      Match_Idx : Natural_t;
      R         : Handler_Result_t;
   begin
      SU.Append (Buffer, Lower_C);

      --  Check for an exact match first.
      --  Since no command is a prefix of another (validated at elaboration),
      --  an exact match is always unambiguous — fire immediately.
      Match_Idx := Find_Exact_Match;
      if Match_Idx > 0 then
         R := Make_Command_Result (Match_Idx);
         Clear_State;
         return R;
      end if;

      --  Not an exact match — check if the buffer could still become one
      if Is_Prefix_Of_Any then
         return R;  --  No_Result: keep buffering
      end if;

      --  Buffer matches nothing and is not a prefix of anything.
      --  Pass through all buffered keys and start fresh.
      R := Make_Pass_Through_Result;
      Clear_State;
      return R;
   end Process_Key;

   function Get_Command_Name (Index : Natural_t) return String_t is
   begin
      if Index >= 1 and then Index <= Natural_t (Command_Table.Length) then
         return SU.To_String (Command_Table (Index - 1).Name);
      else
         return "Unknown";
      end if;
   end Get_Command_Name;

begin
   --  Register command sequences.
   --  Key characters are stored lowercase; input is lowercased before matching.
   --  There is no limit on sequence length.

   Command_Table.Append
      (Command_Entry_t'(Keys => SU.To_Unbounded_String ("ab"),
                        Name => SU.To_Unbounded_String ("A -> B")));

   Command_Table.Append
      (Command_Entry_t'(Keys => SU.To_Unbounded_String ("cd"),
                        Name => SU.To_Unbounded_String ("C -> D")));

   Command_Table.Append
      (Command_Entry_t'(Keys => SU.To_Unbounded_String ("efg"),
                        Name => SU.To_Unbounded_String ("E -> F -> G")));

   --  4-key sequence reusing the 'a' starter.  Second letter is 's',
   --  so "ab" is not a prefix of "asdf" — passes the validator.
   Command_Table.Append
      (Command_Entry_t'(Keys => SU.To_Unbounded_String ("asdf"),
                        Name => SU.To_Unbounded_String ("Quick Save")));

   --  5-key sequence reusing the 'e' starter.  Second letter is 'd',
   --  so "efg" is not a prefix of "edcba" — passes the validator.
   Command_Table.Append
      (Command_Entry_t'(Keys => SU.To_Unbounded_String ("edcba"),
                        Name => SU.To_Unbounded_String ("Reverse Mode")));

   --  Validate: no command may be a prefix of another.
   --  This fires at elaboration time (before main starts).
   Validate_No_Prefix_Conflicts;

end Command_Sequence_Handling;
