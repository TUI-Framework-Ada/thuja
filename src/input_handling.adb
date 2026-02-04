package body Input_Handling is

   --  Push a new event to the buffer
   procedure Push (Buffer : in out Input_Buffer_t; Event : in Input_Event_t) is
   begin
      --  Store the event at the head position
      Buffer.Events (Buffer.Head) := Event;

      --  Advance head pointer (circular)
      if Buffer.Head = Buffer_Max_Size then
         Buffer.Head := 1;
      else
         Buffer.Head := Buffer.Head + 1;
      end if;

      --  If buffer was full, advance tail to overwrite oldest event
      if Buffer.Count = Buffer_Max_Size then
         --  Tail advances to maintain Count = Buffer_Max_Size
         if Buffer.Tail = Buffer_Max_Size then
            Buffer.Tail := 1;
         else
            Buffer.Tail := Buffer.Tail + 1;
         end if;
      else
         --  Buffer not full, increment count
         Buffer.Count := Buffer.Count + 1;
      end if;
   end Push;

   --  Pop the oldest event from the buffer
   function Pop (Buffer : in out Input_Buffer_t; Event : out Input_Event_t) return Boolean is
   begin
      --  Check if buffer is empty
      if Buffer.Count = 0 then
         return False;
      end if;

      --  Retrieve the event at the tail position
      Event := Buffer.Events (Buffer.Tail);

      --  Advance tail pointer (circular)
      if Buffer.Tail = Buffer_Max_Size then
         Buffer.Tail := 1;
      else
         Buffer.Tail := Buffer.Tail + 1;
      end if;

      --  Decrement count
      Buffer.Count := Buffer.Count - 1;

      return True;
   end Pop;

   --  Check if buffer is empty
   function Is_Empty (Buffer : in Input_Buffer_t) return Boolean is
   begin
      return Buffer.Count = 0;
   end Is_Empty;

   --  Check if buffer is full
   function Is_Full (Buffer : in Input_Buffer_t) return Boolean is
   begin
      return Buffer.Count = Buffer_Max_Size;
   end Is_Full;

   --  Clear all events from the buffer
   procedure Clear (Buffer : in out Input_Buffer_t) is
   begin
      Buffer.Head := 1;
      Buffer.Tail := 1;
      Buffer.Count := 0;
   end Clear;

   --  Get the current number of events in the buffer
   function Size (Buffer : in Input_Buffer_t) return Natural is
   begin
      return Buffer.Count;
   end Size;

end Input_Handling;
