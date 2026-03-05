with Ada.Text_IO;

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
   --  ESC (byte 27) is outside 1..26 and maps to Cmd = Quit.
   --  The Escape_Received state is reserved for future multi-byte sequences
   --  (arrow keys, function keys via VT escape codes).
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

      Pos : constant Natural_t := Character_t'Pos (C);
   begin
      --  Default: always produce an event; no modifier; no high-level command.
      Has_Command := True;
      Cmd         := None;
      Modifier    := None;

      case State is
         when Normal =>

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
               --  Signal a Ctrl modifier to the caller.
               --  The caller maps the raw byte to a letter:
               --    byte 1 → 'a',  byte 2 → 'b',  …  byte 26 → 'z'
               --  (96 + Pos = ASCII value of the lowercase letter)
               Modifier := Ctrl;
               --  Cmd stays None — Ctrl+letter is not a Command_t value.

            else
               --  Ordinary character or a recognised special control code.
               case C is
                  when ASCII_TAB =>
                     Cmd := Tab;

                  when ASCII_LF | ASCII_CR =>
                     Cmd := Enter;

                  when ASCII_ESC =>
                     --  ESC maps to Quit.  In the future, transition to
                     --  Escape_Received here to start parsing multi-byte
                     --  VT sequences instead.
                     Cmd := Quit;

                  when others =>
                     null;  --  Cmd stays None; Has_Command is True.
               end case;
            end if;

         when Escape_Received =>
            --  Placeholder for future escape-sequence parsing
            --  (arrow keys, PgUp/PgDn, function keys via ANSI/VT sequences).
            --  For now, just reset and discard the follow-on byte.
            State := Normal;
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
