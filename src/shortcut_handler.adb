with Ada.Calendar;
with Ada.Characters.Handling;

package body Shortcut_Handler is

   --  Internal shortcut table entry
   type Shortcut_Entry_t is record
      Keys   : Key_Sequence_t;
      Length : Natural;
   end record;

   --  Hardcoded shortcuts:
   --    1: A -> B       (2-key)
   --    2: C -> D       (2-key)
   --    3: A -> B -> C  (3-key, shares prefix with shortcut 1)
   --    4: E -> F -> G  (3-key)
   Shortcut_Table : constant array (1 .. Max_Shortcuts) of Shortcut_Entry_t :=
      [1 => (Keys => ['a', 'b', Character'Val (0)], Length => 2),
       2 => (Keys => ['c', 'd', Character'Val (0)], Length => 2),
       3 => (Keys => ['a', 'b', 'c'],               Length => 3),
       4 => (Keys => ['e', 'f', 'g'],               Length => 3)];

   --  Internal state
   Buffer         : Key_Sequence_t := [others => Character'Val (0)];
   Buffer_Count   : Natural := 0;
   Last_Key_Time  : Ada.Calendar.Time := Ada.Calendar.Clock;
   Pending_Index  : Natural := 0;
   Is_Active      : Boolean := False;
   Config_Timeout : Duration := 0.1;

   ------------------------------------------------------------
   --  Internal helpers
   ------------------------------------------------------------

   --  Find the first shortcut whose keys exactly match the buffer.
   --  Returns the 1-based index, or 0 if no match.
   function Find_Exact_Match return Natural is
   begin
      for I in Shortcut_Table'Range loop
         if Shortcut_Table (I).Length = Buffer_Count then
            declare
               Match : Boolean := True;
            begin
               for J in 1 .. Buffer_Count loop
                  if Buffer (J) /= Shortcut_Table (I).Keys (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return I;
               end if;
            end;
         end if;
      end loop;
      return 0;
   end Find_Exact_Match;

   --  Return True if the buffer is a prefix of any shortcut
   --  strictly longer than Min_Length.
   function Is_Prefix_Of_Longer (Min_Length : Natural) return Boolean is
   begin
      for I in Shortcut_Table'Range loop
         if Shortcut_Table (I).Length > Min_Length then
            declare
               Is_Prefix : Boolean := True;
            begin
               for J in 1 .. Buffer_Count loop
                  if Buffer (J) /= Shortcut_Table (I).Keys (J) then
                     Is_Prefix := False;
                     exit;
                  end if;
               end loop;
               if Is_Prefix and Buffer_Count <= Shortcut_Table (I).Length then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Is_Prefix_Of_Longer;

   --  Return True if the buffer is a prefix of any shortcut
   --  (regardless of length, as long as it's longer than the buffer).
   function Is_Prefix_Of_Any return Boolean is
   begin
      return Is_Prefix_Of_Longer (Buffer_Count);
   end Is_Prefix_Of_Any;

   --  Reset all internal state.
   procedure Clear_State is
   begin
      Buffer := [others => Character'Val (0)];
      Buffer_Count := 0;
      Pending_Index := 0;
      Is_Active := False;
   end Clear_State;

   --  Build a Shortcut_Activated result for the given shortcut index.
   function Make_Shortcut_Result (Index : Natural) return Handler_Result_t is
      R : Handler_Result_t;
   begin
      R.Kind := Shortcut_Activated;
      R.Key_Count := Shortcut_Table (Index).Length;
      R.Keys (1 .. R.Key_Count) :=
         Shortcut_Table (Index).Keys (1 .. R.Key_Count);
      R.Shortcut_Index := Index;
      return R;
   end Make_Shortcut_Result;

   --  Build a Keys_Passed_Through result from the current buffer.
   function Make_Pass_Through_Result return Handler_Result_t is
      R : Handler_Result_t;
   begin
      R.Kind := Keys_Passed_Through;
      R.Keys := Buffer;
      R.Key_Count := Buffer_Count;
      return R;
   end Make_Pass_Through_Result;

   ------------------------------------------------------------
   --  Public API
   ------------------------------------------------------------

   procedure Initialize (Timeout : Duration := 0.1) is
   begin
      Config_Timeout := Timeout;
      Clear_State;
   end Initialize;

   function Process_Key (C : Character) return Handler_Result_t is
      use Ada.Calendar;
      Now       : constant Ada.Calendar.Time := Clock;
      Lower_C   : constant Character := Ada.Characters.Handling.To_Lower (C);
      Match_Idx : Natural;
      R         : Handler_Result_t;
   begin
      --  If the previous sequence timed out, resolve it before
      --  processing the new key.
      if Is_Active and then (Now - Last_Key_Time) > Config_Timeout then
         if Pending_Index > 0 then
            R := Make_Shortcut_Result (Pending_Index);
         else
            R := Make_Pass_Through_Result;
         end if;
         --  Start a fresh sequence with the new key
         Buffer := [others => Character'Val (0)];
         Buffer (1) := Lower_C;
         Buffer_Count := 1;
         Last_Key_Time := Now;
         Pending_Index := 0;
         Is_Active := True;
         return R;
      end if;

      --  Append the new key to the buffer
      Last_Key_Time := Now;
      Is_Active := True;

      if Buffer_Count < Max_Sequence_Length then
         Buffer_Count := Buffer_Count + 1;
         Buffer (Buffer_Count) := Lower_C;
      else
         --  Buffer full — pass through everything and start fresh
         R := Make_Pass_Through_Result;
         Clear_State;
         Buffer (1) := Lower_C;
         Buffer_Count := 1;
         Is_Active := True;
         return R;
      end if;

      --  Check for an exact match
      Match_Idx := Find_Exact_Match;

      if Match_Idx > 0 then
         if Is_Prefix_Of_Longer (Buffer_Count) then
            --  This match is also a prefix of a longer shortcut.
            --  Wait for the next key or timeout before deciding.
            Pending_Index := Match_Idx;
            return R;  --  No_Result
         else
            --  Unambiguous match — fire immediately
            R := Make_Shortcut_Result (Match_Idx);
            Clear_State;
            return R;
         end if;

      elsif Is_Prefix_Of_Any then
         --  Not a complete match yet, but could become one.
         return R;  --  No_Result

      else
         --  The buffer doesn't match and isn't a prefix of anything.
         if Pending_Index > 0 then
            --  We had a pending shorter match — fire it.
            --  The key that broke the sequence starts a new sequence.
            R := Make_Shortcut_Result (Pending_Index);
            Buffer := [others => Character'Val (0)];
            Buffer (1) := Lower_C;
            Buffer_Count := 1;
            Pending_Index := 0;
            Is_Active := True;
            return R;
         else
            --  Nothing matched — pass all buffered keys through.
            R := Make_Pass_Through_Result;
            Clear_State;
            return R;
         end if;
      end if;
   end Process_Key;

   function Check_Timeout return Handler_Result_t is
      use Ada.Calendar;
      Now : constant Ada.Calendar.Time := Clock;
      R   : Handler_Result_t;
   begin
      if not Is_Active or Buffer_Count = 0 then
         return R;  --  No_Result
      end if;

      if (Now - Last_Key_Time) > Config_Timeout then
         if Pending_Index > 0 then
            R := Make_Shortcut_Result (Pending_Index);
         else
            R := Make_Pass_Through_Result;
         end if;
         Clear_State;
      end if;

      return R;
   end Check_Timeout;

   function Get_Shortcut_Name (Index : Natural) return String is
   begin
      case Index is
         when 1 => return "A -> B";
         when 2 => return "C -> D";
         when 3 => return "A -> B -> C";
         when 4 => return "E -> F -> G";
         when others => return "Unknown";
      end case;
   end Get_Shortcut_Name;

end Shortcut_Handler;
