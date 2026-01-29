with Ada.Text_IO;

package body Input_Handling is

   --  Add a new event to the back of the buffer (FIFO queue)
   procedure Enqueue (Buffer : in out Event_Buffer_t; Event : in Input_Event_t) is
   begin
      --  Append event to the end of the vector (FIFO queue)
      Buffer.Events.Append (Event);
   end Enqueue;

   --  Remove and return the oldest event from the front of the buffer
   function Dequeue (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean is
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
         Success : Boolean;
      begin
         Success := Dequeue (Events, Event);
         if not Success then
            --  Use NUL character to indicate no input (not space!)
            Event := (Char_Value => Character'Val (0), Cmd => None);
         end if;
      end Consume;

   end Protected_Input_Buffer_t;

   --  State machine for parsing input sequences
   procedure Parse_Input (
      C           : in Character;
      State       : in Out Parse_State_t;
      Cmd         : out Command_t;
      Has_Command : out Boolean
   ) is
      ASCII_TAB : constant Character := Character'Val (9);
      ASCII_LF  : constant Character := Character'Val (10);
      ASCII_CR  : constant Character := Character'Val (13);
      ASCII_ESC : constant Character := Character'Val (27);
   begin
      Has_Command := True;  --  Always produce an event for the keyboard demo
      Cmd := None;

      case State is
         when Normal =>
            case C is
               when ASCII_TAB =>
                  Cmd := Tab;

               when ASCII_LF | ASCII_CR =>
                  Cmd := Enter;

               when ASCII_ESC =>
                  --  ESC triggers quit immediately, no state change needed
                  Cmd := Quit;

               when 'q' | 'Q' =>
                  Cmd := Quit;

               when others =>
                  null;  --  Cmd stays None, but Has_Command is True
            end case;

         when Escape_Received =>
            --  Reserved for future escape sequence handling (e.g., arrow keys)
            --  For now, just reset to normal state
            State := Normal;
      end case;
   end Parse_Input;

   --  Input reader task
   task body Input_Reader is
      C           : Character;
      State       : Parse_State_t := Normal;
      Cmd         : Command_t;
      Has_Command : Boolean;
      Event       : Input_Event_t;
      Running     : Boolean := False;
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
                     Parse_Input (C, State, Cmd, Has_Command);

                     if Has_Command then
                        Event := (Char_Value => C, Cmd => Cmd);
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
