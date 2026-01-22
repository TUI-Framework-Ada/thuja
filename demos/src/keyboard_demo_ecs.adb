with Input_Handling; use Input_Handling;
with Graphics; use Graphics;
with ECS; use ECS;
with IDs; use IDs;
with Components; use Components;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

procedure Keyboard_Demo_ECS is

   package SU renames Ada.Strings.Unbounded;

   ------------------------------------------------------------
   --  CONFIGURATION
   ------------------------------------------------------------
   Input_Consume_Delay : constant Duration := 0.0;

   --  Terminal dimensions
   Term_Width  : constant TUI_Width := 60;
   Term_Height : constant TUI_Height := 15;

   --  Keyboard layout constants
   Key_Width   : constant := 3;  --  Width of each key in characters
   Start_Row   : constant TUI_Height := 2;
   Start_Col   : constant TUI_Width := 3;

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

   --  Create special wide key (TAB, ENTER, SPACE)
   procedure Create_Wide_Key_Entity (
      Key_Name : String;
      X : TUI_Width;
      Y : TUI_Height;
      Width : TUI_Width
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
      BG.Background_Color := Gray;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
   end Create_Wide_Key_Entity;

   --  Create a text label entity (for status line, messages, etc.)
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
   begin
      Y := Start_Row;

      --  Row 1: ` 1 2 3 4 5 6 7 8 9 0 - =
      X := Start_Col;
      for I in Row1_Keys'Range loop
         Create_Key_Entity (Row1_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      --  Row 2: Tab Q W E R T Y U I O P [ ] \
      Y := Y + 2;
      X := Start_Col;
      Create_Wide_Key_Entity ("TAB", X, Y, 5);
      X := X + 5;
      for I in Row2_Keys'Range loop
         Create_Key_Entity (Row2_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      --  Row 3: A S D F G H J K L ; ' Enter
      Y := Y + 2;
      X := Start_Col + 2;
      for I in Row3_Keys'Range loop
         Create_Key_Entity (Row3_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;
      Create_Wide_Key_Entity ("ENT", X, Y, 5);

      --  Row 4: Z X C V B N M , . /
      Y := Y + 2;
      X := Start_Col + 4;
      for I in Row4_Keys'Range loop
         Create_Key_Entity (Row4_Keys (I), X, Y);
         X := X + TUI_Width (Key_Width);
      end loop;

      --  Row 5: Space bar
      Y := Y + 2;
      Create_Wide_Key_Entity ("SPACE", Start_Col + 10, Y, 20);

      --  Status line entity
      Create_Text_Entity ("status_line", Start_Col, 13, 50, "", White, Black);

      --  Exit message entity (initially empty, shown on exit)
      Create_Text_Entity ("exit_message", 1, 1, Term_Width, "", White, Black);
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
   end Update_Key_Colors;

   --  Update status line using ECS text component
   procedure Update_Status is
      Status_Text : String (1 .. 30) := [others => ' '];
   begin
      if Pressed_Key >= ' ' and Pressed_Key <= '~' then
         Status_Text (1 .. 11) := "Last key: '";
         Status_Text (12) := Pressed_Key;
         Status_Text (13) := ''';
      elsif Pressed_Key = Character'Val (9) then
         Status_Text (1 .. 14) := "Last key: TAB ";
      elsif Pressed_Key = Character'Val (13) or Pressed_Key = Character'Val (10) then
         Status_Text (1 .. 16) := "Last key: ENTER ";
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

   Event : Input_Event_t;
   Running : Boolean := True;

begin
   --  Initialize
   Graphics.Clear_Screen;

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
            Pressed_Key := Event.Char_Value;

            --  Update colors, status, and re-render
            Update_Key_Colors;
            Update_Status;
            Render;
         end if;
      end if;

      delay Input_Consume_Delay;
   end loop;

   --  Cleanup
   Input_Reader.Stop;

   --  Show exit message using ECS
   Graphics.Clear_Screen;
   Update_Text_Entity ("exit_message", "Keyboard demo (ECS) ended. Thank you!");
   Render;

end Keyboard_Demo_ECS;
