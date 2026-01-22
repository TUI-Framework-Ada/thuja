with Input_Handling; use Input_Handling;
with Ada.Text_IO;
with Ada.Characters.Handling;

procedure Keyboard_Demo_Delayed is

   ------------------------------------------------------------
   --  CONFIGURATION: Adjust this delay to demonstrate the input buffer
   --  Set to 2.0 seconds to show buffering(super slow), 0.0 for normal operation
   ------------------------------------------------------------
   Input_Consume_Delay : constant Duration := 0.25;

   --  ANSI escape sequences
   CSI : constant String := Character'Val (27) & "[";

   --  Colors
   Reset_Color  : constant String := CSI & "0m";
   Red_BG       : constant String := CSI & "41m";   --  Red background
   Gray_BG      : constant String := CSI & "100m";  --  Gray background
   White_FG     : constant String := CSI & "97m";   --  Bright white text
   Black_FG     : constant String := CSI & "30m";   --  Black text

   --  Key display width
   Key_Width : constant := 4;

   --  Keyboard layout - each row (all valid keys on the demo keyboard)
   Row1_Keys : constant String := "`1234567890-=";
   Row2_Keys : constant String := "qwertyuiop[]\";
   Row3_Keys : constant String := "asdfghjkl;'";
   Row4_Keys : constant String := "zxcvbnm,./";

   --  All printable keys that exist on the keyboard demo
   All_Keys : constant String := Row1_Keys & Row2_Keys & Row3_Keys & Row4_Keys;

   --  Currently pressed key (NUL means none highlighted)
   Pressed_Key : Character := Character'Val (0);

   --  Check if a character is a valid key on our keyboard
   function Is_Valid_Key (C : Character) return Boolean is
      Lower_C : constant Character := Ada.Characters.Handling.To_Lower (C);
   begin
      --  Check special keys
      if C = Character'Val (9) then   --  Tab
         return True;
      elsif C = Character'Val (13) or C = Character'Val (10) then  --  Enter
         return True;
      elsif C = ' ' then  --  Space
         return True;
      end if;

      --  Check if it's in our keyboard layout
      for I in All_Keys'Range loop
         if Ada.Characters.Handling.To_Lower (All_Keys (I)) = Lower_C then
            return True;
         end if;
      end loop;

      return False;
   end Is_Valid_Key;

   --  Move cursor to position
   procedure Move_Cursor (Row, Col : Positive) is
   begin
      Ada.Text_IO.Put (CSI & Positive'Image (Row)(2 .. Positive'Image (Row)'Last)
                       & ";" & Positive'Image (Col)(2 .. Positive'Image (Col)'Last) & "H");
   end Move_Cursor;

   --  Clear screen
   procedure Clear_Screen is
   begin
      Ada.Text_IO.Put (CSI & "2J" & CSI & "1;1H");
   end Clear_Screen;

   --  Hide cursor
   procedure Hide_Cursor is
   begin
      Ada.Text_IO.Put (CSI & "?25l");
   end Hide_Cursor;

   --  Show cursor
   procedure Show_Cursor is
   begin
      Ada.Text_IO.Put (CSI & "?25h");
   end Show_Cursor;

   --  Draw a single key
   procedure Draw_Key (Row, Col : Positive; Key : Character; Is_Pressed : Boolean) is
      Display_Char : Character := Key;
      BG : constant String := (if Is_Pressed then Red_BG else Gray_BG);
      FG : constant String := (if Is_Pressed then White_FG else Black_FG);
   begin
      --  Convert to uppercase for display
      if Key in 'a' .. 'z' then
         Display_Char := Ada.Characters.Handling.To_Upper (Key);
      end if;

      Move_Cursor (Row, Col);
      Ada.Text_IO.Put (BG & FG);

      --  Draw key with borders
      Ada.Text_IO.Put ("[" & Display_Char & "]");
      Ada.Text_IO.Put (Reset_Color);
   end Draw_Key;

   --  Draw special wide key (like space, backspace, etc.)
   procedure Draw_Wide_Key (Row, Col : Positive; Label : String;
                            Is_Pressed : Boolean; Width : Positive) is
      BG : constant String := (if Is_Pressed then Red_BG else Gray_BG);
      FG : constant String := (if Is_Pressed then White_FG else Black_FG);
      Padding : constant Natural := (Width - Label'Length) / 2;
   begin
      Move_Cursor (Row, Col);
      Ada.Text_IO.Put (BG & FG);
      Ada.Text_IO.Put ("[");
      for I in 1 .. Padding loop
         Ada.Text_IO.Put (" ");
      end loop;
      Ada.Text_IO.Put (Label);
      for I in 1 .. (Width - Label'Length - Padding) loop
         Ada.Text_IO.Put (" ");
      end loop;
      Ada.Text_IO.Put ("]");
      Ada.Text_IO.Put (Reset_Color);
   end Draw_Wide_Key;

   --  Check if character matches (case insensitive)
   function Matches (Key : Character; Pressed : Character) return Boolean is
   begin
      return Ada.Characters.Handling.To_Lower (Key) = Ada.Characters.Handling.To_Lower (Pressed);
   end Matches;

   --  Draw the entire keyboard
   procedure Draw_Keyboard is
      Start_Row : constant := 2;
      Start_Col : constant := 3;
      Tab_Width : constant := 6;  --  Width of TAB key including brackets
   begin
      --  Row 1: ` 1 2 3 4 5 6 7 8 9 0 - =
      for I in Row1_Keys'Range loop
         Draw_Key (Start_Row, Start_Col + (I - Row1_Keys'First) * Key_Width,
                   Row1_Keys (I), Matches (Row1_Keys (I), Pressed_Key));
      end loop;

      --  Row 2: Tab Q W E R T Y U I O P [ ] \
      Draw_Wide_Key (Start_Row + 2, Start_Col, "TAB",
                     Pressed_Key = Character'Val (9), 4);
      for I in Row2_Keys'Range loop
         Draw_Key (Start_Row + 2, Start_Col + Tab_Width + (I - Row2_Keys'First) * Key_Width,
                   Row2_Keys (I), Matches (Row2_Keys (I), Pressed_Key));
      end loop;

      --  Row 3: A S D F G H J K L ; ' [Enter]
      for I in Row3_Keys'Range loop
         Draw_Key (Start_Row + 4, Start_Col + 4 + (I - Row3_Keys'First) * Key_Width,
                   Row3_Keys (I), Matches (Row3_Keys (I), Pressed_Key));
      end loop;
      --  Enter key
      Draw_Wide_Key (Start_Row + 4, Start_Col + 4 + Row3_Keys'Length * Key_Width, "ENT",
                     Pressed_Key = Character'Val (13) or Pressed_Key = Character'Val (10), 5);

      --  Row 4: Z X C V B N M , . /
      for I in Row4_Keys'Range loop
         Draw_Key (Start_Row + 6, Start_Col + 6 + (I - Row4_Keys'First) * Key_Width,
                   Row4_Keys (I), Matches (Row4_Keys (I), Pressed_Key));
      end loop;

      --  Row 5: Space bar
      Draw_Wide_Key (Start_Row + 8, Start_Col + 12, "SPACE",
                     Pressed_Key = ' ', 24);
   end Draw_Keyboard;

   --  Draw status line
   procedure Draw_Status is
   begin
      Move_Cursor (13, 3);
      Ada.Text_IO.Put (CSI & "K");  --  Clear line
      if Pressed_Key >= ' ' and Pressed_Key <= '~' then
         Ada.Text_IO.Put ("Last key: '" & Pressed_Key & "' (ASCII: " &
                          Integer'Image (Character'Pos (Pressed_Key)) & ")");
      elsif Pressed_Key = Character'Val (9) then
         Ada.Text_IO.Put ("Last key: TAB (ASCII: 9)");
      elsif Pressed_Key = Character'Val (13) or Pressed_Key = Character'Val (10) then
         Ada.Text_IO.Put ("Last key: ENTER (ASCII: " &
                          Integer'Image (Character'Pos (Pressed_Key)) & ")");
      elsif Pressed_Key = Character'Val (0) then
         Ada.Text_IO.Put ("(Key not on keyboard)");
      end if;
   end Draw_Status;

   Event : Input_Event_t;
   Running : Boolean := True;

begin
   --  Initialize display
   Clear_Screen;
   Hide_Cursor;

   --  Start input reader
   Input_Reader.Start;

   --  Initial draw
   Draw_Keyboard;

   --  Main loop
   while Running loop
      --  Get input event
      Input_Buffer.Consume (Event);

      if Event.Cmd /= None or Event.Char_Value /= Character'Val (0) then
         --  Check for quit (ESC only now, not Q since Q is on keyboard)
         if Event.Cmd = Quit and Event.Char_Value = Character'Val (27) then
            Running := False;
         elsif Is_Valid_Key (Event.Char_Value) then
            --  Only update if it's a valid key on our keyboard
            Pressed_Key := Event.Char_Value;
            Draw_Keyboard;
            Draw_Status;
         else
            --  Invalid key, show message but don't highlight anything
            Pressed_Key := Character'Val (0);
            Draw_Keyboard;
            Draw_Status;
         end if;
      end if;

      delay Input_Consume_Delay;
   end loop;

   --  Cleanup
   Input_Reader.Stop;
   Show_Cursor;
   Clear_Screen;

end Keyboard_Demo_Delayed;
