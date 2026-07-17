with Ada.Text_IO;
with Console_Input_Mode;

package body Input_Handling is

   --  Add a new event to the back of the buffer (FIFO queue)
   procedure Enqueue (Buffer : in out Event_Buffer_t; Event : in Input_Event_t) is
   begin
      --  Append event to the end of the vector (FIFO queue)
      Buffer.Events.Append (Event);
   end Enqueue;

   --  Remove and return the oldest event from the front of the buffer
   function Dequeue (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean_t is
   begin
      --  Check if buffer is empty
      if Buffer.Events.Is_Empty then
         return False;
      end if;

      --  Get the first (oldest) event
      Event := Buffer.Events.First_Element;

      --  Remove it from the front
      Buffer.Events.Delete_First;

      return True;
   end Dequeue;

   --  Remove and return the newest event from the back of the buffer
   function Pop_Last (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean_t is
   begin
      --  Check if buffer is empty
      if Buffer.Events.Is_Empty then
         return False;
      end if;

      --  Get the last (newest) event
      Event := Buffer.Events.Last_Element;

      --  Remove it from the front
      Buffer.Events.Delete_Last;

      return True;
   end Pop_Last;

   --  Protected object implementation
   protected body Protected_Input_Buffer_t is

      --  Add an event to the buffer (called by input reader)
      procedure Produce (Event : in Input_Event_t) is
      begin
         Enqueue (Events, Event);
      end Produce;

      --  Get an event from the buffer (called by main application)
      procedure Consume (Event : out Input_Event_t) is
         Success : Boolean_t;
      begin
         Success := Dequeue (Events, Event);
         if not Success then
            --  Use NUL character to indicate no input (not space!)
            Event := (Char_Value => Character_t'Val (0), Modifier => None, Cmd => None);
         end if;
      end Consume;

      --  Remove the most recent event from the buffer (called by input reader)
      procedure Remove_Last (Event : out Input_Event_t) is
         Success : Boolean_t;
      begin
         Success := Pop_Last (Events, Event);
         if not Success then
            --  Use NUL character to indicate no input (not space!)
            Event := (Char_Value => Character_t'Val (0), Modifier => None, Cmd => None);
         end if;
      end Remove_Last;

   end Protected_Input_Buffer_t;

   --  State machine for parsing input sequences.
   --
   --  Ctrl+letter detection:
   --    The terminal sends bytes 1..26 when Ctrl is held with A..Z.
   --    These overlap with several special control codes:
   --      Byte  8 = BS   (Ctrl+H)  → passed through unchanged (Modifier = None)
   --      Byte  9 = TAB  (Ctrl+I)  → maps to Cmd = Tab
   --      Byte 10 = LF   (Ctrl+J)  → maps to Cmd = Enter
   --      Byte 13 = CR   (Ctrl+M)  → maps to Cmd = Enter
   --    All other bytes 1..26 are treated as Ctrl+letter:
   --      Modifier   = Ctrl
   --      Char_Value = the ASCII letter ('a'..'z') derived from the raw byte
   --      Cmd        = None
   --
   --  ESC: a single ESC press emits Cmd = Quit immediately so demos can
   --  use it to exit (or, in the text editor, to leave Insert mode).
   --  This rules out terminal-encoded Alt+key sequences (which arrive as
   --  ESC + char) — that's the deliberate trade-off.  Modifier = Alt is
   --  retained in the type so future work can revisit, but is never
   --  produced by this parser.
   procedure Parse_Input (
      C           : in  Character_t;
      State       : in out Parse_State_t;
      Cmd         : out Command_t;
      Modifier    : out Modifier_t;
      Has_Command : out Boolean_t
   ) is
      ASCII_BS  : constant Character_t := Character_t'Val (8);
      ASCII_TAB : constant Character_t := Character_t'Val (9);
      ASCII_LF  : constant Character_t := Character_t'Val (10);
      ASCII_CR  : constant Character_t := Character_t'Val (13);
      ASCII_ESC : constant Character_t := Character_t'Val (27);

      ARROW_PREFIX      : constant Character_t := Character_t'('[');
      ARROW_UP          : constant Character_t := Character_t'('A');
      ARROW_DOWN        : constant Character_t := Character_t'('B');
      ARROW_RIGHT       : constant Character_t := Character_t'('C');
      ARROW_LEFT        : constant Character_t := Character_t'('D');
      Last_Event        : Input_Event_t;
      Second_Last_Event : Input_Event_t;

      Pos : constant Natural_t := Character_t'Pos (C);
   begin
      --  Default: always produce an event; no modifier; no high-level command.
      Has_Command := True;
      Cmd         := None;
      Modifier    := None;

      --  Escape_Received exists in the type for future expansion but the
      --  current parser never enters it; collapse to Normal defensively.
      State := Normal;

      --  ----------------------------------------------------------
      --  Ctrl+letter: raw bytes 1..26 excluding the codes that have
      --  their own semantic meaning (Tab, Enter, Backspace).
      --  ----------------------------------------------------------
      if Pos in 1 .. 26
         and then C /= ASCII_TAB  --  byte  9 → Tab command
         and then C /= ASCII_LF   --  byte 10 → Enter command
         and then C /= ASCII_CR   --  byte 13 → Enter command
         and then C /= ASCII_BS   --  byte  8 → Backspace (pass through)
      then
         --  Signal a Ctrl modifier to the caller.  Char remap to letter
         --  happens in the Input_Reader task once Has_Command is set.
         Modifier := Ctrl;
         return;
      end if;

      case C is
         when ASCII_TAB =>
            Cmd := Tab;

         when ASCII_LF | ASCII_CR =>
            Cmd := Enter;

         when ASCII_ESC =>
            --  Single ESC quits immediately.
            Cmd := Quit;

         when ARROW_UP .. ARROW_LEFT =>
            --  Check the last two inputs to see if this is an arrow or not.
            Input_Buffer.Remove_Last (Last_Event);
            Input_Buffer.Remove_Last (Second_Last_Event);
            if Last_Event.Cmd = None and Last_Event.Char_Value = ARROW_PREFIX
              and Second_Last_Event.Cmd = Quit then
               --  It was an arrow, switch to the correct arrow direction
               case C is
                  when ARROW_UP =>
                     Cmd := Up;

                  when ARROW_DOWN =>
                     Cmd := Down;

                  when ARROW_RIGHT =>
                     Cmd := Right;

                  when ARROW_LEFT =>
                     Cmd := Left;

                  when others =>
                     null;
               end case;
            else
               --  Not an arrow, resend the last two events if they exist
               if Second_Last_Event.Cmd /= None
                 or Second_Last_Event.Char_Value /= Character_t'Val (0) then
                  Input_Buffer.Produce (Second_Last_Event);
               end if;
               if Last_Event.Cmd /= None
                 or Last_Event.Char_Value /= Character_t'Val (0) then
                  Input_Buffer.Produce (Last_Event);
               end if;
            end if;

         when others =>
            null;  --  Cmd stays None; Has_Command is True.
      end case;
   end Parse_Input;

   --  Input reader task.
   --  Runs on its own Ada task, reading bytes from stdin via Get_Immediate
   --  and placing parsed Input_Event_t records into the shared protected buffer.
   task body Input_Reader is
      C           : Character_t;
      State       : Parse_State_t := Normal;
      Cmd         : Command_t;
      Mod_Key     : Modifier_t;
      Has_Command : Boolean_t;
      Event       : Input_Event_t;
      Running     : Boolean_t := False;
   begin
      loop
         select
            accept Start do
               Console_Input_Mode.Enable_VT_Input;
               Running := True;
            end Start;

            --  Main input reading loop
            while Running loop
               select
                  accept Stop do
                     Running := False;
                  end Stop;
               else
                  begin
                     Ada.Text_IO.Get_Immediate (C);
                     Parse_Input (C, State, Cmd, Mod_Key, Has_Command);

                     if Has_Command then
                        --  For Ctrl+letter events, map the raw byte (1..26)
                        --  back to its ASCII letter ('a'..'z') so callers
                        --  can work with a readable character.  The Modifier
                        --  field distinguishes it from an ordinary keypress.
                        if Mod_Key = Ctrl then
                           Event := (
                              Char_Value => Character_t'Val (Character_t'Pos (C) + 96),
                              Modifier   => Ctrl,
                              Cmd        => None
                           );
                        elsif Mod_Key = Alt then
                           --  For Alt+key, C is already the printable character
                           --  that followed ESC — no remapping needed.
                           Event := (
                              Char_Value => C,
                              Modifier   => Alt,
                              Cmd        => None
                           );
                        else
                           Event := (Char_Value => C, Modifier => None, Cmd => Cmd);
                        end if;
                        Input_Buffer.Produce (Event);
                     end if;
                  exception
                     when others =>
                        null;
                  end;
               end select;
            end loop;

         or
            accept Stop do
               Running := False;
            end Stop;

         or
            terminate;
         end select;
      end loop;
   end Input_Reader;

end Input_Handling;
