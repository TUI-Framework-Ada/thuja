with Input_Handling; use Input_Handling;
with Graphics; use Graphics;
with ECS; use ECS;
with IDs; use IDs;
with Components; use Components;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Shortcut_Handler;
with Ada.Calendar;

procedure Keyboard_Widget is

   package SU renames Ada.Strings.Unbounded;

   --  Vector of Entity_ID vectors (one per keyboard row)
   package Keyboard_Row_Vector is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Entity_ID_Vector.Vector,
       "="          => Entity_ID_Vector."=");

   Keyboard_Rows : Keyboard_Row_Vector.Vector;

   ------------------------------------------------------------
   --  CONFIGURATION
   ------------------------------------------------------------
   Input_Consume_Delay : constant Duration := 0.0;

   --  Terminal dimensions
   Term_Width  : constant TUI_Width := 65;
   Term_Height : constant TUI_Height := 16;

   --  Keyboard layout constants
   Key_Width   : constant := 3;  --  Width of each key in characters
   Start_Row   : constant TUI_Height := 2;
   Start_Col   : constant TUI_Width := 3;

   --  Centering offsets (Row 0 is widest at 56 columns)
   Row1_Offset : constant TUI_Width := 5;
   Row2_Offset : constant TUI_Width := 6;
   Row3_Offset : constant TUI_Width := 5;
   Row4_Offset : constant TUI_Width := 5;
   Row5_Offset : constant TUI_Width := 5;

   --  Background color for visual-only (non-interactive) keys
   Inactive_Key_Color : constant Color_t := Steel_Blue;

   --  Keyboard layout - each row
   Row1_Keys : constant String := "`1234567890-=";
   Row2_Keys : constant String := "qwertyuiop[]\";
   Row3_Keys : constant String := "asdfghjkl;'";
   Row4_Keys : constant String := "zxcvbnm,./";

   --  All printable keys
   All_Keys : constant String := Row1_Keys & Row2_Keys & Row3_Keys & Row4_Keys;

   --  Currently pressed key
   Pressed_Key : Character := Character'Val (0);

   --  ECS world
   World : Entity_Components;

   --  Check if a character is a valid key on our keyboard
   function Is_Valid_Key (C : Character) return Boolean is
      Lower_C : constant Character := Ada.Characters.Handling.To_Lower (C);
   begin
      if C = Character'Val (9) then return True; end if;  --  Tab
      if C = Character'Val (13) or C = Character'Val (10) then return True; end if;  --  Enter
      if C = Character'Val (8) or C = Character'Val (127) then return True; end if;  --  Backspace
      if C = ' ' then return True; end if;  --  Space

      for I in All_Keys'Range loop
         if Ada.Characters.Handling.To_Lower (All_Keys (I)) = Lower_C then
            return True;
         end if;
      end loop;
      return False;
   end Is_Valid_Key;

   --  Create a key entity at given position
   procedure Create_Key_Entity (
      Key_Char : Character;
      X : TUI_Width;
      Y : TUI_Height;
      Width : TUI_Width := TUI_Width (Key_Width);
      Height : TUI_Height := 1
   ) is
      Entity_Name : constant String := "key_" & Key_Char;
      Comp_Ptr : Components_Ptr;
      Widget : Widget_Component_T;
      Text : Text_Component_T;
      BG : Background_Color_Component_T;
      Display_Char : Character := Key_Char;
   begin
      --  Convert to uppercase for display
      if Key_Char in 'a' .. 'z' then
         Display_Char := Ada.Characters.Handling.To_Upper (Key_Char);
      end if;

      Comp_Ptr := Add_Entity (World, To_EID (Entity_Name));

      --  Widget component with position and size
      Widget.Position_X := X;
      Widget.Position_Y := Y;
      Widget.Size_Width := Width;
      Widget.Size_Height := Height;
      Widget.Is_Visible := True;
      Widget.Render_Buffer := Create_Buffer (Width, Height);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      --  Text component with the key character
      Text.Text := SU.To_Unbounded_String ("[" & Display_Char & "]");
      Text.Text_Color := Black;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);

      --  Background color (gray by default)
      BG.Background_Color := Gray;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
   end Create_Key_Entity;

   --  Create special wide key (TAB, ENTER, SPACE, etc.)
   procedure Create_Wide_Key_Entity (
      Key_Name : String;
      X : TUI_Width;
      Y : TUI_Height;
      Width : TUI_Width;
      BG_Color : Color_t := Gray
   ) is
      Entity_Name : constant String := "key_" & Key_Name;
      Comp_Ptr : Components_Ptr;
      Widget : Widget_Component_T;
      Text : Text_Component_T;
      BG : Background_Color_Component_T;
      Label : String (1 .. Natural (Width));
      Label_Start : Natural;
   begin
      Comp_Ptr := Add_Entity (World, To_EID (Entity_Name));

      --  Widget component
      Widget.Position_X := X;
      Widget.Position_Y := Y;
      Widget.Size_Width := Width;
      Widget.Size_Height := 1;
      Widget.Is_Visible := True;
      Widget.Render_Buffer := Create_Buffer (Width, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      --  Center the label in the key
      Label := [others => ' '];
      Label (1) := '[';
      Label (Natural (Width)) := ']';
      Label_Start := (Natural (Width) - Key_Name'Length) / 2 + 1;
      if Label_Start > 1 and Label_Start + Key_Name'Length - 1 < Natural (Width) then
         Label (Label_Start .. Label_Start + Key_Name'Length - 1) := Key_Name;
      end if;

      Text.Text := SU.To_Unbounded_String (Label);
      Text.Text_Color := Black;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);

      --  Background color
      BG.Background_Color := BG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
   end Create_Wide_Key_Entity;

   --  Create a text label entity (for status line, etc.)
   procedure Create_Text_Entity (
      Entity_Name : String;
      X : TUI_Width;
      Y : TUI_Height;
      Width : TUI_Width;
      Initial_Text : String := "";
      FG_Color : Color_t := White;
      BG_Color : Color_t := Black
   ) is
      Comp_Ptr : Components_Ptr;
      Widget : Widget_Component_T;
      Text : Text_Component_T;
      BG : Background_Color_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID (Entity_Name));

      --  Widget component
      Widget.Position_X := X;
      Widget.Position_Y := Y;
      Widget.Size_Width := Width;
      Widget.Size_Height := 1;
      Widget.Is_Visible := True;
      Widget.Render_Buffer := Create_Buffer (Width, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      --  Text component
      Text.Text := SU.To_Unbounded_String (Initial_Text);
      Text.Text_Color := FG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);

      --  Background color
      BG.Background_Color := BG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
   end Create_Text_Entity;

   --  Update text content of a text entity
   procedure Update_Text_Entity (Entity_Name : String; New_Text : String) is
      Comp_Ptr : Components_Ptr;
      Text : Text_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (World, To_EID (Entity_Name));
      if Comp_Ptr /= null and then Has_Component (Comp_Ptr.all, To_CID ("TextComponent")) then
         Text := Text_Component_T (Get_Component (Comp_Ptr.all, To_CID ("TextComponent")));
         Text.Text := SU.To_Unbounded_String (New_Text);
         Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);
      end if;
   end Update_Text_Entity;

   --  Create RenderInfo entity for the terminal
   procedure Create_Render_Info_Entity is
      Comp_Ptr : Components_Ptr;
      RI : Render_Info_Component_T;
      Root_Widget : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
   begin
      --  RenderInfo entity
      Comp_Ptr := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width := Term_Width;
      RI.Terminal_Height := Term_Height;
      RI.BackBuffer := Create_Buffer (Term_Width, Term_Height);
      RI.FrameBuffer := Create_Buffer (Term_Width, Term_Height);

      --  Force a full initial render by setting the Backbuffer to sentinel
      --  values so every pixel differs from the Framebuffer on the first frame.
      --  Without this, positions not covered by child entities never get
      --  explicitly rendered, leaving the terminal's native background visible
      --  (which may differ from RGB 0,0,0 Black).
      for X in TUI_Width'First .. Term_Width loop
         for Y in TUI_Height'First .. Term_Height loop
            Set_Buffer_Pixel (RI.Backbuffer, X, Y,
               (Char             => Character'Val (1),
                Char_Color       => White,
                Background_Color => White,
                Is_Bold          => True));
         end loop;
      end loop;

      Add_Component (Comp_Ptr.all, To_CID ("RenderInfo"), RI);

      --  Root widget entity
      Comp_Ptr := Add_Entity (World, To_EID ("root"));
      Root_Widget.Position_X := 1;
      Root_Widget.Position_Y := 1;
      Root_Widget.Size_Width := Term_Width;
      Root_Widget.Size_Height := Term_Height;
      Root_Widget.Is_Visible := True;
      Root_Widget.Render_Buffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Root_Widget);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_Marker);
   end Create_Render_Info_Entity;

   --  Create all keyboard key entities
   procedure Create_Keyboard_Entities is
      X : TUI_Width;
      Y : TUI_Height;
      Current_Row : Entity_ID_Vector.Vector;

      --  Helper: create a wide key entity and append its ID to the current row
      procedure Add_Wide_To_Row (
         Name : String; Col : TUI_Width; Row_Y : TUI_Height; W : TUI_Width;
         BG_Color : Color_t := Gray
      ) is
      begin
         Create_Wide_Key_Entity (Name, Col, Row_Y, W, BG_Color);
         Current_Row.Append (To_EID ("key_" & Name));
      end Add_Wide_To_Row;

      --  Helper: create a regular key entity and append its ID to the current row
      procedure Add_Key_To_Row (
         C : Character; Col : TUI_Width; Row_Y : TUI_Height
      ) is
      begin
         Create_Key_Entity (C, Col, Row_Y);
         Current_Row.Append (To_EID ("key_" & C));
      end Add_Key_To_Row;

   begin
      --  =============================================
      --  ROW 0: Function keys (Y = 2) -- no offset
      --  =============================================
      Y := Start_Row;
      Current_Row.Clear;
      X := Start_Col;

      Add_Wide_To_Row ("ESC", X, Y, 5, Pink);
      X := X + 5;

      --  F1 through F9 (width 4 each)
      for I in 1 .. 9 loop
         declare
            Img  : constant String := Natural'Image (I);
            Name : constant String := "F" & Img (Img'First + 1 .. Img'Last);
         begin
            Add_Wide_To_Row (Name, X, Y, 4, Inactive_Key_Color);
         end;
         X := X + 4;
      end loop;

      --  F10 through F12 (width 5 each)
      for I in 10 .. 12 loop
         declare
            Img  : constant String := Natural'Image (I);
            Name : constant String := "F" & Img (Img'First + 1 .. Img'Last);
         begin
            Add_Wide_To_Row (Name, X, Y, 5, Inactive_Key_Color);
         end;
         X := X + 5;
      end loop;

      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  ROW 1: Number row + BACKSPACE (Y = 4)
      --  =============================================
      Y := Y + 2;
      Current_Row.Clear;
      X := Start_Col + Row1_Offset;

      for I in Row1_Keys'Range loop
         Add_Key_To_Row (Row1_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      Add_Wide_To_Row ("BKSP", X, Y, 7);
      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  ROW 2: TAB + QWERTY row (Y = 6)
      --  =============================================
      Y := Y + 2;
      Current_Row.Clear;
      X := Start_Col + Row2_Offset;

      Add_Wide_To_Row ("TAB", X, Y, 5);
      X := X + 5;

      for I in Row2_Keys'Range loop
         Add_Key_To_Row (Row2_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  ROW 3: CAPS + Home row + ENTER (Y = 8)
      --  =============================================
      Y := Y + 2;
      Current_Row.Clear;
      X := Start_Col + Row3_Offset;

      Add_Wide_To_Row ("CAPS", X, Y, 7, Inactive_Key_Color);
      X := X + 7;

      for I in Row3_Keys'Range loop
         Add_Key_To_Row (Row3_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      Add_Wide_To_Row ("ENT", X, Y, 5);
      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  ROW 4: LSHIFT + Z row + RSHIFT (Y = 10)
      --  =============================================
      Y := Y + 2;
      Current_Row.Clear;
      X := Start_Col + Row4_Offset;

      Add_Wide_To_Row ("LSHIFT", X, Y, 8, Inactive_Key_Color);
      X := X + 8;

      for I in Row4_Keys'Range loop
         Add_Key_To_Row (Row4_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      Add_Wide_To_Row ("RSHIFT", X, Y, 8, Inactive_Key_Color);
      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  ROW 5: CTRL + ALT + SPACE + ALT + CTRL (Y = 12)
      --  =============================================
      Y := Y + 2;
      Current_Row.Clear;
      X := Start_Col + Row5_Offset;

      Add_Wide_To_Row ("LCTRL", X, Y, 7, Inactive_Key_Color);
      X := X + 7;
      Add_Wide_To_Row ("LALT", X, Y, 6, Inactive_Key_Color);
      X := X + 6;
      Add_Wide_To_Row ("SPACE", X, Y, 20);
      X := X + 20;
      Add_Wide_To_Row ("RALT", X, Y, 6, Inactive_Key_Color);
      X := X + 6;
      Add_Wide_To_Row ("RCTRL", X, Y, 7, Inactive_Key_Color);
      Keyboard_Rows.Append (Current_Row);

      --  =============================================
      --  Non-keyboard entities
      --  =============================================
      Create_Text_Entity ("status_line", Start_Col, 14, 55, "", White, Black);

      --  =============================================
      --  Register all children with root widget
      --  Iterate the vector-of-vectors keyboard layout
      --  =============================================
      declare
         Root_Comp : Components_Ptr;
         Root_W    : Widget_Component_T;
      begin
         Root_Comp := Get_Entity_Components (World, To_EID ("root"));
         Root_W := Widget_Component_T (
            Get_Component (Root_Comp.all, To_CID ("WidgetComponent"))
         );

         for Row_Idx in 0 .. Natural (Keyboard_Rows.Length) - 1 loop
            declare
               Row : constant Entity_ID_Vector.Vector :=
                  Keyboard_Rows (Row_Idx);
            begin
               for Key_Idx in 0 .. Natural (Row.Length) - 1 loop
                  Root_W.Children.Append (Row (Key_Idx));
               end loop;
            end;
         end loop;

         Root_W.Children.Append (To_EID ("status_line"));

         Add_Component (Root_Comp.all, To_CID ("WidgetComponent"), Root_W);
      end;
   end Create_Keyboard_Entities;

   --  Update key colors based on pressed key
   procedure Update_Key_Colors is
      Comp_Ptr : Components_Ptr;
      BG : Background_Color_Component_T;
      Key_Char : Character;
      Entity_Name : String (1 .. 10);
      Entity_Name_Len : Natural;
   begin
      --  Reset all keys to gray, highlight pressed key in red
      for I in All_Keys'Range loop
         Key_Char := All_Keys (I);
         Entity_Name_Len := 5;
         Entity_Name (1 .. 5) := "key_" & Key_Char;

         Comp_Ptr := Get_Entity_Components (World, To_EID (Entity_Name (1 .. Entity_Name_Len)));
         if Comp_Ptr /= null and then Has_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent")) then
            BG := Background_Color_Component_T (
               Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
            );

            if Ada.Characters.Handling.To_Lower (Key_Char) =
               Ada.Characters.Handling.To_Lower (Pressed_Key) then
               BG.Background_Color := Red;
            else
               BG.Background_Color := Gray;
            end if;

            Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
         end if;
      end loop;

      --  Handle special keys
      --  TAB
      Comp_Ptr := Get_Entity_Components (World, To_EID ("key_TAB"));
      if Comp_Ptr /= null then
         BG := Background_Color_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
         );
         BG.Background_Color := (if Pressed_Key = Character'Val (9) then Red else Gray);
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;

      --  ENTER
      Comp_Ptr := Get_Entity_Components (World, To_EID ("key_ENT"));
      if Comp_Ptr /= null then
         BG := Background_Color_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
         );
         BG.Background_Color := (if Pressed_Key = Character'Val (13) or Pressed_Key = Character'Val (10) then Red else Gray);
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;

      --  SPACE
      Comp_Ptr := Get_Entity_Components (World, To_EID ("key_SPACE"));
      if Comp_Ptr /= null then
         BG := Background_Color_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
         );
         BG.Background_Color := (if Pressed_Key = ' ' then Red else Gray);
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;

      --  BACKSPACE
      Comp_Ptr := Get_Entity_Components (World, To_EID ("key_BKSP"));
      if Comp_Ptr /= null then
         BG := Background_Color_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
         );
         BG.Background_Color := (if Pressed_Key = Character'Val (8)
                                  or Pressed_Key = Character'Val (127)
                                  then Red else Gray);
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;
   end Update_Key_Colors;

   --  Update status line using ECS text component
   procedure Update_Status is
      Status_Text : String (1 .. 55) := [others => ' '];
   begin
      if Pressed_Key >= ' ' and Pressed_Key <= '~' then
         Status_Text (1 .. 11) := "Last key: '";
         Status_Text (12) := Pressed_Key;
         Status_Text (13) := ''';
      elsif Pressed_Key = Character'Val (9) then
         Status_Text (1 .. 14) := "Last key: TAB ";
      elsif Pressed_Key = Character'Val (13) or Pressed_Key = Character'Val (10) then
         Status_Text (1 .. 16) := "Last key: ENTER ";
      elsif Pressed_Key = Character'Val (8) or Pressed_Key = Character'Val (127) then
         Status_Text (1 .. 20) := "Last key: BACKSPACE ";
      end if;
      Update_Text_Entity ("status_line", Status_Text);
   end Update_Status;

   --  Run all render systems
   procedure Render is
   begin
      WidgetBackgroundSystem (World);
      TextRenderSystem (World);
      BufferCopySystem (World);
      BufferDrawSystem (World);
   end Render;

   --  Highlight specific keys with Gold for shortcut activation
   procedure Highlight_Shortcut_Keys (Result : Shortcut_Handler.Handler_Result_t) is
      Comp_Ptr : Components_Ptr;
      BG : Background_Color_Component_T;
   begin
      for I in 1 .. Result.Key_Count loop
         declare
            Key_Name : constant String := "key_" & Result.Keys (I);
         begin
            Comp_Ptr := Get_Entity_Components (World, To_EID (Key_Name));
            if Comp_Ptr /= null and then
               Has_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
            then
               BG := Background_Color_Component_T (
                  Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent")));
               BG.Background_Color := Gold;
               Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
            end if;
         end;
      end loop;
   end Highlight_Shortcut_Keys;

   --  Update status line to show which shortcut was activated
   procedure Update_Shortcut_Status (Result : Shortcut_Handler.Handler_Result_t) is
      Status_Text : String (1 .. 55) := [others => ' '];
      Pos : Natural := 1;
      Prefix : constant String := "Shortcut: ";
   begin
      Status_Text (Pos .. Pos + Prefix'Length - 1) := Prefix;
      Pos := Pos + Prefix'Length;

      for I in 1 .. Result.Key_Count loop
         if I > 1 then
            Status_Text (Pos .. Pos + 3) := " -> ";
            Pos := Pos + 4;
         end if;
         Status_Text (Pos) := Ada.Characters.Handling.To_Upper (Result.Keys (I));
         Pos := Pos + 1;
      end loop;

      Update_Text_Entity ("status_line", Status_Text);
   end Update_Shortcut_Status;

   --  Handle the result from the shortcut handler
   procedure Handle_Shortcut_Result (Result : Shortcut_Handler.Handler_Result_t) is
   begin
      case Result.Kind is
         when Shortcut_Handler.No_Result =>
            null;

         when Shortcut_Handler.Shortcut_Activated =>
            --  Reset all keys to their default colors
            Pressed_Key := Character'Val (0);
            Update_Key_Colors;
            --  Highlight the shortcut keys with Gold
            Highlight_Shortcut_Keys (Result);
            --  Show shortcut info on the status line
            Update_Shortcut_Status (Result);
            Render;

         when Shortcut_Handler.Keys_Passed_Through =>
            --  Process the last passed-through key as a normal key press
            if Result.Key_Count > 0 then
               Pressed_Key := Result.Keys (Result.Key_Count);
               Update_Key_Colors;
               Update_Status;
               Render;
            end if;
      end case;
   end Handle_Shortcut_Result;

   Event            : Input_Event_t;
   SC_Result        : Shortcut_Handler.Handler_Result_t;
   Running          : Boolean := True;
   Last_Full_Render : Ada.Calendar.Time := Ada.Calendar.Clock;

begin
   --  Initialize
   Graphics.Clear_Screen;
   Shortcut_Handler.Initialize (Timeout => 0.15);

   --  Create ECS entities
   Create_Render_Info_Entity;
   Create_Keyboard_Entities;

   --  Initial render
   Render;

   --  Start input reader
   Input_Reader.Start;

   --  Main loop
   while Running loop
      Input_Buffer.Consume (Event);

      if Event.Cmd /= None or Event.Char_Value /= Character'Val (0) then
         if Event.Cmd = Quit and Event.Char_Value = Character'Val (27) then
            Running := False;
         elsif Is_Valid_Key (Event.Char_Value) then
            --  Feed key through the shortcut handler
            SC_Result := Shortcut_Handler.Process_Key (Event.Char_Value);
            Handle_Shortcut_Result (SC_Result);
         end if;
      end if;

      --  Check for shortcut sequence timeout
      SC_Result := Shortcut_Handler.Check_Timeout;
      Handle_Shortcut_Result (SC_Result);

      --  Periodically force a full re-render to recover from visual
      --  corruption caused by terminal resizes.  Resetting the Backbuffer
      --  to sentinel values makes every pixel differ from the Framebuffer,
      --  so the next Render pass redraws the entire screen.
      declare
         use Ada.Calendar;
         Now : constant Ada.Calendar.Time := Clock;
      begin
         if (Now - Last_Full_Render) > 0.5 then
            Last_Full_Render := Now;
            declare
               RI_Comp : Components_Ptr;
               RI      : Render_Info_Component_T;
            begin
               RI_Comp := Get_Entity_Components (World, To_EID ("render_info"));
               RI := Render_Info_Component_T (
                  Get_Component (RI_Comp.all, To_CID ("RenderInfo")));
               for RX in TUI_Width'First .. Term_Width loop
                  for RY in TUI_Height'First .. Term_Height loop
                     Set_Buffer_Pixel (RI.Backbuffer, RX, RY,
                        (Char             => Character'Val (1),
                         Char_Color       => White,
                         Background_Color => White,
                         Is_Bold          => True));
                  end loop;
               end loop;
               Add_Component (RI_Comp.all, To_CID ("RenderInfo"), RI);
            end;
            Render;
         end if;
      end;

      delay Input_Consume_Delay;
   end loop;

   --  Cleanup
   Input_Reader.Stop;
   Graphics.Restore_Screen;

end Keyboard_Widget;
