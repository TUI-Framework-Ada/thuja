with Ada.Containers.Vectors;

package Input_Handling is

   --  Command types that can be generated from input
   type Command_t is (Tab, Quit, Enter, None);

   --  Input event containing both the raw character and parsed command
   type Input_Event_t is record
      Char_Value : Character := ' ';
      Cmd        : Command_t := None;
   end record;

   --  Instantiate vector for input events
   package Event_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Input_Event_t);

   --  Vector-based buffer for input events (unlimited capacity)
   type Event_Buffer_t is record
      Events : Event_Vectors.Vector;
   end record;

   --  Add a new event to the back of the buffer (FIFO queue)
   procedure Enqueue (Buffer : in out Event_Buffer_t; Event : in Input_Event_t);

   --  Remove and return the oldest event from the front of the buffer
   --  Returns True if an event was available, False if buffer was empty
   function Dequeue (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean;

   --  Protected object for thread-safe input buffer access
   protected type Protected_Input_Buffer_t is
      --  Add an input event to the buffer
      procedure Produce (Event : in Input_Event_t);

      --  Get the next input event from the buffer
      procedure Consume (Event : out Input_Event_t);

   private
      Events : Event_Buffer_t;
   end Protected_Input_Buffer_t;

   --  Global protected input buffer instance
   Input_Buffer : Protected_Input_Buffer_t;

   --  Task for reading input from stdin
   task Input_Reader is
      entry Start;
      entry Stop;
   end Input_Reader;

private

   --  State machine states for parsing input sequences
   type Parse_State_t is (Normal, Escape_Received);

   --  Parse a character and current state to determine command
   --  Returns the new state and any generated command
   procedure Parse_Input (
      C           : in Character;
      State       : in Out Parse_State_t;
      Cmd         : out Command_t;
      Has_Command : out Boolean
   );

end Input_Handling;
