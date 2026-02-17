-------------------------------------------------------------------------------
--  HTop_Demo.adb
--  
--  htop-style task manager - simplified version
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded;
with Graphics; use Graphics;
with System_Stats;
with Input_Handling;

procedure HTop_Demo is

   package SU renames Ada.Strings.Unbounded;
   package SS renames System_Stats;
   use type SS.Process_Array_Ptr;

   --  Process list display
   Max_Processes_Shown : constant := 15;  -- Smaller list
   Scroll_Offset : Natural := 0;
   
   --  State
   Running : Boolean := True;
   
   --  Update timing
   Update_Interval : constant Time_Span := Milliseconds(1000);
   Last_Key_Check : Time := Clock;
   
   --------------------------------------------------------
   -- Utility Functions
   --------------------------------------------------------
   
   function Trim (S : String) return String is
   begin
      if S'Length = 0 then
         return "";
      end if;
      
      for I in S'Range loop
         if S(I) /= ' ' then
            return S(I .. S'Last);
         end if;
      end loop;
      return "";
   end Trim;
   
   function Format_Percent (Value : Float) return String is
      Percent : Natural := Natural(Value * 100.0);
   begin
      if Percent > 100 then
         Percent := 100;
      end if;
      
      if Percent < 10 then
         return "  " & Trim(Natural'Image(Percent)) & "%";
      elsif Percent < 100 then
         return " " & Trim(Natural'Image(Percent)) & "%";
      else
         return "100%";
      end if;
   end Format_Percent;
   
   function Format_Memory (KB : Natural) return String is
   begin
      if KB < 1024 then
         return Trim(Natural'Image(KB)) & "K";
      elsif KB < 1024 * 1024 then
         return Trim(Natural'Image(KB / 1024)) & "M";
      else
         return Trim(Natural'Image(KB / (1024 * 1024))) & "G";
      end if;
   end Format_Memory;
   
   function Format_Uptime (Seconds : Natural) return String is
      Days : constant Natural := Seconds / 86400;
      Hours : constant Natural := (Seconds mod 86400) / 3600;
      Minutes : constant Natural := (Seconds mod 3600) / 60;
   begin
      if Days > 0 then
         return Trim(Natural'Image(Days)) & "d " & 
                Trim(Natural'Image(Hours)) & "h";
      elsif Hours > 0 then
         return Trim(Natural'Image(Hours)) & "h " & 
                Trim(Natural'Image(Minutes)) & "m";
      else
         return Trim(Natural'Image(Minutes)) & "min";
      end if;
   end Format_Uptime;
   
   function State_Char (State : SS.Process_State) return Character is
   begin
      case State is
         when SS.Running => return 'R';
         when SS.Sleeping => return 'S';
         when SS.Stopped => return 'T';
         when SS.Zombie => return 'Z';
         when SS.Unknown_State => return '?';
      end case;
   end State_Char;
   
   function Get_Bar_Color (Percent : Float) return Color_t is
   begin
      if Percent < 0.33 then
         return Green;
      elsif Percent < 0.66 then
         return Yellow;
      else
         return Red;
      end if;
   end Get_Bar_Color;
   
   procedure Draw_Bar (Y : Natural; X : Natural; Width : Natural; 
                       Value : Float; Label : String) is
      Filled : Natural := Natural(Value * Float(Width));
      Bar_Color : constant Color_t := Get_Bar_Color(Value);
      Percent_Str : constant String := Format_Percent(Value);
   begin
      if Filled > Width then
         Filled := Width;
      end if;
      
      -- Move cursor and draw label
      Ada.Text_IO.Put(CSI & Trim(Natural'Image(Y)) & ";" & 
                      Trim(Natural'Image(X)) & "H");
      Ada.Text_IO.Put(Label & "[");
      
      -- Set bar color
      Ada.Text_IO.Put(CSI & "38;2;" & 
                      Trim(U8'Image(Bar_Color.Red)) & ";" &
                      Trim(U8'Image(Bar_Color.Green)) & ";" &
                      Trim(U8'Image(Bar_Color.Blue)) & "m");
      
      -- Draw filled portion with = signs
      for I in 1 .. Filled loop
         Ada.Text_IO.Put('=');
      end loop;
      
      -- Reset color and draw empty portion with spaces
      Ada.Text_IO.Put(CSI & "0m");
      for I in Filled + 1 .. Width loop
         Ada.Text_IO.Put(' ');
      end loop;
      
      -- Close bracket
      Ada.Text_IO.Put("]" & Percent_Str);
   end Draw_Bar;
   
   --------------------------------------------------------
   -- Main Display
   --------------------------------------------------------
   
   procedure Draw_Header is
      Num_Cores : constant Natural := SS.Get_CPU_Count;
      Uptime : constant Natural := SS.Get_Uptime;
      Load_Avg : constant String := SS.Get_Load_Average;
      
      Total_Mem, Used_Mem, Avail_Mem, Free_Mem : Natural;
      Buffers, Cached, Swap_Total, Swap_Used : Natural;
   begin
      --  Clear screen and move to top
      Ada.Text_IO.Put(CSI & "2J" & CSI & "1;1H");
      
      --  Top border - CYAN for title
      Ada.Text_IO.Put(CSI & "1;36m");  -- Bright Cyan
      Ada.Text_IO.Put("  ");
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      Ada.Text_IO.New_Line;
      
      --  Title bar
      Ada.Text_IO.Put(CSI & "1;36m");  -- Bright Cyan
      Ada.Text_IO.Put("  |");
      Ada.Text_IO.Put(" THUJA TASK MANAGER - htop style");
      Ada.Text_IO.Put("     Press q to quit              ");
      Ada.Text_IO.Put("|");
      Ada.Text_IO.Put(CSI & "0m");
      Ada.Text_IO.New_Line;
      
      --  Separator
      Ada.Text_IO.Put(CSI & "1;36m");
      Ada.Text_IO.Put("  ");
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      Ada.Text_IO.New_Line;
      
      --  System info line
      Ada.Text_IO.Put(CSI & "1;36m  |" & CSI & "0m");
      Ada.Text_IO.Put(" Cores: " & Trim(Natural'Image(Num_Cores)));
      Ada.Text_IO.Put(" | Uptime: " & Format_Uptime(Uptime));
      Ada.Text_IO.Put(" | Load: " & Load_Avg & "   ");
      Ada.Text_IO.Put(CSI & "1;36m|" & CSI & "0m");
      Ada.Text_IO.New_Line;
      
      --  Separator
      Ada.Text_IO.Put(CSI & "1;36m");
      Ada.Text_IO.Put("  ");
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      Ada.Text_IO.New_Line;
      
      --  CPU section - GREEN borders
      Ada.Text_IO.Put(CSI & "1;32m  |" & CSI & "1m");  -- Bright Green
      Ada.Text_IO.Put(" CPU Usage:");
      Ada.Text_IO.Put(CSI & "0;32m");
      for I in 1 .. 64 loop
         Ada.Text_IO.Put(' ');
      end loop;
      Ada.Text_IO.Put(CSI & "1;32m|" & CSI & "0m");
      Ada.Text_IO.New_Line;
      
      for Core in 0 .. Num_Cores - 1 loop
         declare
            Usage : constant Float := SS.Get_CPU_Usage(Core);
            Row : constant Natural := 8 + Core;
            Core_Label : constant String := "  " & Trim(Natural'Image(Core)) & " ";
         begin
            Ada.Text_IO.Put(CSI & "1;32m  |" & CSI & "0m");  -- Green border
            Draw_Bar(Row, 5, 40, Usage, Core_Label);
            Ada.Text_IO.Put(CSI & Trim(Natural'Image(Row)) & ";78H");
            Ada.Text_IO.Put(CSI & "1;32m|" & CSI & "0m");
         end;
      end loop;
      
      --  Separator before memory - YELLOW
      declare
         Sep_Row : constant Natural := 8 + Num_Cores;
      begin
         Ada.Text_IO.Put(CSI & Trim(Natural'Image(Sep_Row)) & ";1H");
         Ada.Text_IO.Put(CSI & "1;33m");  -- Bright Yellow
         Ada.Text_IO.Put("  ");
         for I in 1 .. 76 loop
            Ada.Text_IO.Put('-');
         end loop;
         Ada.Text_IO.Put(CSI & "0m");
      end;
      
      --  Memory section - YELLOW borders
      SS.Get_Memory_Detailed(Total_Mem, Used_Mem, Free_Mem, Avail_Mem,
                             Buffers, Cached, Swap_Total, Swap_Used);
      
      declare
         Mem_Row : constant Natural := 9 + Num_Cores;
         Mem_Percent : Float := 0.0;
      begin
         if Total_Mem > 0 then
            Mem_Percent := Float(Used_Mem) / Float(Total_Mem);
         end if;
         
         Ada.Text_IO.Put(CSI & Trim(Natural'Image(Mem_Row)) & ";1H");
         Ada.Text_IO.Put(CSI & "1;33m  |" & CSI & "1m");  -- Yellow border
         Ada.Text_IO.Put(" Memory:");
         Ada.Text_IO.Put(CSI & "0m");
         Ada.Text_IO.Put(" " & Format_Memory(Used_Mem * 1024) & "/" & 
                        Format_Memory(Total_Mem * 1024));
         for I in 1 .. 56 loop
            Ada.Text_IO.Put(' ');
         end loop;
         Ada.Text_IO.Put(CSI & "1;33m|" & CSI & "0m");
         Ada.Text_IO.New_Line;
         
         Ada.Text_IO.Put(CSI & "1;33m  |" & CSI & "0m");
         Draw_Bar(Mem_Row + 1, 5, 50, Mem_Percent, " RAM ");
         Ada.Text_IO.Put(CSI & Trim(Natural'Image(Mem_Row + 1)) & ";78H");
         Ada.Text_IO.Put(CSI & "1;33m|" & CSI & "0m");
         
         if Swap_Total > 0 then
            declare
               Swap_Percent : constant Float := Float(Swap_Used) / Float(Swap_Total);
            begin
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put(CSI & "1;33m  |" & CSI & "0m");
               Draw_Bar(Mem_Row + 2, 5, 50, Swap_Percent, " SWP ");
               Ada.Text_IO.Put(CSI & Trim(Natural'Image(Mem_Row + 2)) & ";78H");
               Ada.Text_IO.Put(CSI & "1;33m|" & CSI & "0m");
            end;
         end if;
         
         --  Bottom border of memory box
         declare
            Bottom_Row : constant Natural := Mem_Row + 3;
         begin
            Ada.Text_IO.Put(CSI & Trim(Natural'Image(Bottom_Row)) & ";1H");
            Ada.Text_IO.Put(CSI & "1;33m");  -- Yellow
            Ada.Text_IO.Put("  ");
            for I in 1 .. 76 loop
               Ada.Text_IO.Put('-');
            end loop;
            Ada.Text_IO.Put(CSI & "0m");
         end;
      end;
   end Draw_Header;
   
   procedure Draw_Process_List is
      Procs : SS.Process_Array_Ptr := SS.Get_Process_List;
      Start_Row : constant Natural := 22;
      Displayed : Natural := 0;
      Total_Procs : Natural := 0;
   begin
      if Procs /= null then
         Total_Procs := Procs'Length;
      end if;
      
      --  Box border top - MAGENTA
      Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row - 1)) & ";1H");
      Ada.Text_IO.Put(CSI & "1;35m  ");  -- Bright Magenta
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      
      --  Header with scroll info
      Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row)) & ";1H");
      Ada.Text_IO.Put(CSI & "1;35m  |" & CSI & "1m");  -- Magenta border
      Ada.Text_IO.Put(" PID   USER     CPU% MEM%  S COMMAND");
      Ada.Text_IO.Put(CSI & "0m");
      Ada.Text_IO.Put("  ");
      Ada.Text_IO.Put(CSI & "33m");  -- Yellow
      Ada.Text_IO.Put("[j/k]scroll");
      Ada.Text_IO.Put(CSI & "0m ");
      Ada.Text_IO.Put(Trim(Natural'Image(Scroll_Offset + 1)));
      Ada.Text_IO.Put("-");
      if Total_Procs > 0 then
         Ada.Text_IO.Put(Trim(Natural'Image(Natural'Min(Scroll_Offset + Max_Processes_Shown, Total_Procs))));
      else
         Ada.Text_IO.Put("0");
      end if;
      Ada.Text_IO.Put("/");
      Ada.Text_IO.Put(Trim(Natural'Image(Total_Procs)));
      for I in 1 .. 10 loop
         Ada.Text_IO.Put(' ');
      end loop;
      Ada.Text_IO.Put(CSI & "1;35m|" & CSI & "0m");
      
      --  Separator
      Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row + 1)) & ";1H");
      Ada.Text_IO.Put(CSI & "1;35m  ");  -- Magenta
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      
      if Procs = null then
         Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row + 2)) & ";1H");
         Ada.Text_IO.Put(CSI & "1;35m  |" & CSI & "0m");
         Ada.Text_IO.Put("  No process information available");
         return;
      end if;
      
      --  Process rows (compact format)
      for I in 1 .. Procs'Length loop
         exit when Displayed >= Max_Processes_Shown;
         
         if I > Scroll_Offset then
            declare
               Proc : SS.Process_Info renames Procs(I);
               Row : constant Natural := Start_Row + 2 + Displayed;
               PID_Str : String(1 .. 5);
               User_Str : String(1 .. 8);
               CPU_Str : constant String := Format_Percent(Proc.CPU / 100.0);
               Mem_Str : constant String := Format_Percent(Proc.Memory / 100.0);
               Name : constant String := SU.To_String(Proc.Name);
            begin
               --  Format PID (right-aligned)
               declare
                  PID_Image : constant String := Trim(Natural'Image(Proc.PID));
               begin
                  PID_Str := [others => ' '];
                  for J in 1 .. Natural'Min(PID_Image'Length, 5) loop
                     PID_Str(6 - Natural'Min(PID_Image'Length, 5) + J - 1) := 
                        PID_Image(PID_Image'First + J - 1);
                  end loop;
               end;
               
               --  Format User (left-aligned, truncated)
               declare
                  User_Image : constant String := SU.To_String(Proc.User);
               begin
                  User_Str := [others => ' '];
                  for J in 1 .. Natural'Min(User_Image'Length, 8) loop
                     User_Str(J) := User_Image(User_Image'First + J - 1);
                  end loop;
               end;
               
               --  Draw compact row
               Ada.Text_IO.Put(CSI & Trim(Natural'Image(Row)) & ";1H");
               Ada.Text_IO.Put(CSI & "1;35m  |" & CSI & "0m ");  -- Magenta border
               Ada.Text_IO.Put(PID_Str & " " & User_Str & " ");
               Ada.Text_IO.Put(CPU_Str & " " & Mem_Str & "  ");
               Ada.Text_IO.Put(State_Char(Proc.State) & " ");
               
               --  Truncate command name to fit (35 chars)
               if Name'Length > 35 then
                  Ada.Text_IO.Put(Name(Name'First .. Name'First + 34));
               else
                  Ada.Text_IO.Put(Name);
                  for J in Name'Length + 1 .. 35 loop
                     Ada.Text_IO.Put(' ');
                  end loop;
               end if;
               
               Ada.Text_IO.Put(CSI & "1;35m|" & CSI & "0m");  -- Magenta border
               
               Displayed := Displayed + 1;
            end;
         end if;
      end loop;
      
      --  Clear any remaining rows and add borders
      for I in Displayed + 1 .. Max_Processes_Shown loop
         Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row + 1 + I)) & ";1H");
         Ada.Text_IO.Put(CSI & "1;35m  |" & CSI & "0m");  -- Magenta
         for J in 1 .. 74 loop
            Ada.Text_IO.Put(' ');
         end loop;
         Ada.Text_IO.Put(CSI & "1;35m|" & CSI & "0m");
      end loop;
      
      --  Bottom border
      Ada.Text_IO.Put(CSI & Trim(Natural'Image(Start_Row + 2 + Max_Processes_Shown)) & ";1H");
      Ada.Text_IO.Put(CSI & "1;35m  ");  -- Magenta
      for I in 1 .. 76 loop
         Ada.Text_IO.Put('-');
      end loop;
      Ada.Text_IO.Put(CSI & "0m");
      
      SS.Free_Process_List(Procs);
   end Draw_Process_List;
   
   --------------------------------------------------------
   -- Keyboard Handling with Simple Input
   --------------------------------------------------------
   
   procedure Check_Keyboard is
      use Input_Handling;
      Event : Input_Event_t;
   begin
      Input_Buffer.Consume(Event);
      
      -- Check if we got a real event (not NUL)
      if Event.Char_Value = Character'Val(0) then
         return;  -- No input available
      end if;
      
      -- Check for quit commands
      if Event.Cmd = Quit then
         Running := False;
         return;
      end if;
      
      -- Also check raw character for quit
      if Event.Char_Value = 'q' or Event.Char_Value = 'Q' then
         Running := False;
         return;
      end if;
      
      -- Check for scroll keys
      case Event.Char_Value is
         when 'j' | 'J' =>  -- Scroll down
            Scroll_Offset := Scroll_Offset + 1;
            
         when 'k' | 'K' =>  -- Scroll up
            if Scroll_Offset > 0 then
               Scroll_Offset := Scroll_Offset - 1;
            end if;
            
         when others =>
            null;
      end case;
   end Check_Keyboard;
   
   --------------------------------------------------------
   -- Main Loop
   --
   -- Root cause of broken input: main loop spent ~1 full second
   -- blocked on "delay until Next_Update", so keypresses only
   -- registered once per second and felt completely ignored.
   --
   -- Fix: Display_Task redraws every 1s in the background.
   -- Main task polls Input_Buffer every 50ms -> instant response.
   --------------------------------------------------------

   --  Declared here (inside procedure, before begin) so it can
   --  see Running and the draw procedures.
   task Display_Task is
      entry Stop;
   end Display_Task;

   task body Display_Task is
      Next_Draw : Time := Clock;
   begin
      loop
         select
            accept Stop;
            exit;
         else
            if not Running then
               exit;
            end if;
            Draw_Header;
            Draw_Process_List;
            Ada.Text_IO.Flush;
            Next_Draw := Next_Draw + Update_Interval;
            delay until Next_Draw;
         end select;
      end loop;
   end Display_Task;

begin
   --  Initialize terminal
   Clear_Screen;
   Enable_VT_Processing;
   Set_Cursor_Visible(False);

   --  Start Input_Reader task (calls Get_Immediate in background)
   Input_Handling.Input_Reader.Start;

   --  Warm up CPU stats (first call always returns 0)
   declare
      Dummy : constant Float := SS.Get_CPU_Usage_Average;
   begin
      null;
   end;

   --  Poll keyboard every 50ms - feels instant to the user
   while Running loop
      Check_Keyboard;
      delay 0.05;
   end loop;

   --  Shut down display task cleanly
   Display_Task.Stop;
   Input_Handling.Input_Reader.Stop;
   Set_Cursor_Visible(True);
   Reset_Styling;
   Clear_Screen;

exception
   when others =>
      Input_Handling.Input_Reader.Stop;
      Set_Cursor_Visible(True);
      Reset_Styling;
      Clear_Screen;
      raise;

end HTop_Demo;