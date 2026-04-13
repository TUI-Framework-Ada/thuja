with Graphics; use Graphics;

package Sort_Demo is

   ---------------------------------------------------------------------------
   --  Sort_Demo
   --
   --  Owns all non-ECS state and logic for the Sound of Sorting tab.
   --  Exposes render procedures that write directly into widget buffers,
   --  since the bar chart and code panel need custom pixel-level rendering.
   ---------------------------------------------------------------------------

   --  Layout constants (used by Tab_Demo to size widgets)
   Bar_W : constant := 40;
   Code_W : constant := 40;

   --  Initialise source code listings (call once at startup)
   procedure Initialise;

   --  Reset the current algorithm: reshuffle bars, rewind to step 0
   procedure Reset;

   --  Switch to algorithm N (1=Bubble, 2=Insertion, 3=Selection,
   --  4=Quick, 5=Merge, 6=Stalin) and reset
   procedure Switch_Algo (N : Positive);

   --  Toggle play/pause; if finished, reset instead
   procedure Toggle_Play;

   --  Advance one step forward (while paused)
   procedure Step_Forward;

   --  Increase step delay by 10 ms (max 1000)
   procedure Speed_Down;

   --  Decrease step delay by 10 ms (min 10)
   procedure Speed_Up;

   --  Advance playback by one step if enough time has elapsed (call each frame)
   procedure Tick;

   --  Render the bar chart into Buf (width W, height H)
   procedure Render_Bars (Buf : in out Buffer_T;
                          W   : in     TUI_Width;
                          H   : in     TUI_Height);

   --  Render the code panel into Buf (width W, height H)
   procedure Render_Code (Buf : in out Buffer_T;
                          W   : in     TUI_Width;
                          H   : in     TUI_Height);

   --  Render the status bar into Buf (width W, height H)
   procedure Render_Status (Buf : in out Buffer_T;
                            W   : in     TUI_Width;
                            H   : in     TUI_Height);

   --  Human-readable name of the current algorithm
   function Algo_Display_Name return String;

   --  Whether the sort is currently running (not paused, not finished)
   function Is_Running return Boolean;

   --  Help text for the chrome bar
   function Help_Text return String;

end Sort_Demo;
