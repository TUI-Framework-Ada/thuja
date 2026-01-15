package Input_Handling is

   --  Maximum number of input events that can be buffered
   Buffer_Max_Size : constant Positive := 20;

   --  Represents a single input event (keystroke)
   type Input_Event_t is record
      Char_Value : Character;
   end record;

   --  Circular buffer for input events
   type Input_Buffer_t is private;

   --  Push a new event to the buffer
   --  If buffer is full, oldest event is replaced
   procedure Push (Buffer : in out Input_Buffer_t; Event : in Input_Event_t);

   --  Pop the oldest event from the buffer
   --  Returns True if an event was available, False if buffer was empty
   function Pop (Buffer : in out Input_Buffer_t; Event : out Input_Event_t) return Boolean;

   --  Check if buffer is empty
   function Is_Empty (Buffer : in Input_Buffer_t) return Boolean;

   --  Check if buffer is full
   function Is_Full (Buffer : in Input_Buffer_t) return Boolean;

   --  Clear all events from the buffer
   procedure Clear (Buffer : in out Input_Buffer_t);

   --  Get the current number of events in the buffer
   function Size (Buffer : in Input_Buffer_t) return Natural;

private

   type Event_Array_t is array (1 .. Buffer_Max_Size) of Input_Event_t;

   type Input_Buffer_t is record
      Events : Event_Array_t;
      Head   : Positive := 1;  --  Points to next write position
      Tail   : Positive := 1;  --  Points to next read position
      Count  : Natural := 0;   --  Current number of events in buffer
   end record;

end Input_Handling;
