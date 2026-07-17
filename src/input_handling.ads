with Ada.Containers.Vectors;

package Input_Handling is

   --  Subtypes for standard Ada types
   subtype Character_t is Character;
   subtype Boolean_t is Boolean;
   subtype Natural_t is Natural;

   --  Command types that can be generated from input
   type Command_t is (Tab, Quit, Enter, Up, Down, Right, Left, None);

   --  Modifier keys that can accompany a character.
   --  None   : no modifier held (ordinary keypress)
   --  Ctrl   : Ctrl key was held; Char_Value is the letter ('a'..'z')
   --           and the raw byte was in the range 1..26.
   --  Alt    : Alt key was held; detected via ESC + printable char sequence.
   --           Char_Value is the printable character that followed ESC.
   --  Shift is reserved for future expansion.
   type Modifier_t is (None, Ctrl, Alt);

   --  Input event containing the raw character, optional modifier, and parsed command.
   --  For a Ctrl+letter event:
   --    Char_Value = the letter ('a'..'z')
   --    Modifier   = Ctrl
   --    Cmd        = None  (Ctrl chars do not map to the Command_t enum)
   --  For an Alt+key event:
   --    Char_Value = the printable character that followed ESC
   --    Modifier   = Alt
   --    Cmd        = None
   --  For ordinary characters:
   --    Char_Value = the character as received
   --    Modifier   = None
   --    Cmd        = Tab / Quit / Enter / None as appropriate
   type Input_Event_t is record
      Char_Value : Character_t := ' ';
      Modifier   : Modifier_t  := None;
      Cmd        : Command_t   := None;
   end record;

   --  Instantiate vector for input events
   package Event_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural_t,
       Element_Type => Input_Event_t);

   --  Vector-based buffer for input events (unlimited capacity)
   type Event_Buffer_t is record
      Events : Event_Vectors.Vector;
   end record;

   --  Add a new event to the back of the buffer (FIFO queue)
   procedure Enqueue (Buffer : in out Event_Buffer_t; Event : in Input_Event_t);

   --  Remove and return the oldest event from the front of the buffer
   --  Returns True if an event was available, False if buffer was empty
   function Dequeue (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean_t;

   --  Remove and return the newest event from the back of the buffer
   --  Returns True if an event was available, False if buffer was empty
   function Pop_Last (Buffer : in out Event_Buffer_t; Event : out Input_Event_t) return Boolean_t;

   --  Protected object for thread-safe input buffer access
   protected type Protected_Input_Buffer_t is
      --  Add an input event to the buffer
      procedure Produce (Event : in Input_Event_t);

      --  Get the next input event from the buffer
      procedure Consume (Event : out Input_Event_t);

      --  Remove the most recent event from the buffer
      procedure Remove_Last (Event : out Input_Event_t);

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

   --  Parse a single raw byte from stdin.
   --  Updates the state machine, and outputs:
   --    Cmd         : high-level command (Tab, Quit, Enter) or None
   --    Modifier    : Ctrl if a Ctrl+letter was detected, otherwise None
   --    Has_Command : True when an event should be enqueued
   procedure Parse_Input (
      C           : in  Character_t;
      State       : in out Parse_State_t;
      Cmd         : out Command_t;
      Modifier    : out Modifier_t;
      Has_Command : out Boolean_t
   );

end Input_Handling;
