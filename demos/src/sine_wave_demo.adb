with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Unbounded;  --  Unbounded_String type used for widget text content
use Ada.Strings.Unbounded;
with Ada.Numerics;           --  Pi constant
with Ada.Numerics.Elementary_Functions;  --  Sin function
use Ada.Numerics.Elementary_Functions;
with Components;             --  Component type definitions
with Console;                --  Terminal setup: VT-Processing, cursor visibility
with ECS;                    --  Entity Component System: Add_Entity, Add_Component, all systems
with Graphics;               --  Color definitions, pixel types, cursor and screen operations
use Graphics;
with IDs;                    --  Entity and component ID types: To_EID, To_CID
use type IDs.Entity_ID_Vector.Vector;
with Flexbox;                --  Flexbox layout types: Column/Row direction, Flex_Item_Array
with Input_Handling;         --  Keyboard input: Input_Reader, Input_Buffer, Input_Event_t

procedure Sine_Wave_Demo is

   ------------------------------------------------------------------------------
   --  Sine_Wave_Demo.adb
   --
   --  PURPOSE
   --  -------
   --  A real-time sine wave visualiser built on the Thuja TUI framework.
   --  Renders a scrolling sine wave on screen using ASCII characters,
   --  with live parameter control via keyboard input.
   --
   --  VISUAL LAYOUT
   --  -------------
   --  ┌────────────────────────────────────────────────────────────────┐
   --  │  Sine Wave Visualiser   AMP: 1.0   FREQ: 1.0   SPEED: 1.0    │
   --  ├────────────────────────────────────────────────────────────────┤
   --  │  *   *           *   *           *   *           *   *        │
   --  │*       *       *       *       *       *       *       *      │
   --  │          *   *           *   *           *   *           *    │
   --  │            *               *               *               *  │
   --  │                                                               │
   --  ├────────────────────────────────────────────────────────────────┤
   --  │  w/s: amplitude  |  a/d: frequency  |  q/e: speed  |  ESC: quit│
   --  └────────────────────────────────────────────────────────────────┘
   --
   --  KEYBINDINGS
   --  -----------
   --  w / s   Increase / decrease amplitude
   --  a / d   Decrease / increase frequency
   --  q / e   Decrease / increase scroll speed
   --  ESC     Quit the demo
   ------------------------------------------------------------------------------

   --------------------------------------------------------
   -- WAVE PARAMETERS
   -- These control the shape and behaviour of the sine wave.
   -- All are mutable so keyboard input can adjust them live.
   --------------------------------------------------------

   --  Amplitude: how tall the wave is, as a fraction of
   --  the graph widget height. Clamped between 0.1 and 1.0.
   Amplitude : Float := 1.0;

   --  Frequency: how many cycles appear across the screen.
   --  Higher values produce more compressed waves.
   Frequency : Float := 1.0;

   --  Speed: how fast the wave scrolls left each frame.
   --  Higher values make the wave move faster.
   Speed : Float := 1.0;

   --  Phase offset: incremented each frame to create the
   --  scrolling illusion. Never resets — just keeps growing.
   Phase : Float := -Ada.Numerics.Pi / 2.0;

   --  Step sizes for each parameter adjustment keypress
   Amp_Step   : constant Float := 0.1;
   Freq_Step  : constant Float := 0.1;
   Speed_Step : constant Float := 0.1;

   --  Parameter bounds
   Amp_Min   : constant Float := 0.1;
   Amp_Max   : constant Float := 1.0;
   Freq_Min  : constant Float := 0.1;
   Freq_Max  : constant Float := 5.0;
   Speed_Min : constant Float := 0.1;
   Speed_Max : constant Float := 5.0;

   --------------------------------------------------------
   -- GRAPH DIMENSIONS
   -- Graph_Width  : number of columns in the graph widget
   -- Graph_Height : number of rows in the graph widget
   --               (24 total - 1 title - 1 status = 22)
   --------------------------------------------------------
   Graph_Width  : constant Positive := 80;
   Graph_Height : constant Positive := 22;

   Should_Quit : Boolean := False;

   --------------------------------------------------------
   -- ECS ENTITY STORAGE
   --------------------------------------------------------
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --------------------------------------------------------
   E_RenderInfo : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root       : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_TitleBar   : constant IDs.Entity_Id := IDs.To_EID ("TitleBar");
   E_Graph      : constant IDs.Entity_Id := IDs.To_EID ("Graph");
   E_StatusBar  : constant IDs.Entity_Id := IDs.To_EID ("StatusBar");

   C_RenderInfo : constant ECS.Components_Ptr :=
     ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root       : constant ECS.Components_Ptr :=
     ECS.Add_Entity (Entities_PO, E_Root);
   C_TitleBar   : constant ECS.Components_Ptr :=
     ECS.Add_Entity (Entities_PO, E_TitleBar);
   C_Graph      : constant ECS.Components_Ptr :=
     ECS.Add_Entity (Entities_PO, E_Graph);
   C_StatusBar  : constant ECS.Components_Ptr :=
     ECS.Add_Entity (Entities_PO, E_StatusBar);

   --------------------------------------------------------
   -- HELPER: Float_To_String
   -- Converts a Float to a short display string with one
   -- decimal place. Used to build the title bar text.
   --------------------------------------------------------
   function Float_To_String (F : Float) return String is
      Rounded  : constant Integer := Integer (F * 10.0);
      Int_Part : constant Integer := Rounded / 10;
      Dec_Part : constant Integer := abs (Rounded mod 10);
   begin
      return
        Integer'Image (Int_Part) (2 .. Integer'Image (Int_Part)'Last)
        & "."
        & Integer'Image (Dec_Part) (2 .. Integer'Image (Dec_Part)'Last);
   end Float_To_String;

   --------------------------------------------------------
   -- HELPER: Build_Graph_Text
   -- Builds the full display string for the graph widget.
   -- For each column calculates the sine value and maps
   -- it to a row within the top half of the graph height.
   -- Places '*' at the wave position only.
   --------------------------------------------------------
   function Build_Graph_Text return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;

      type Grid_T is
        array (0 .. Graph_Height - 1, 0 .. Graph_Width - 1) of Boolean;
      Grid : Grid_T := (others => (others => False));

      Samples_Per_Col : constant Positive := 8;
   begin
      for Col in 0 .. Graph_Width - 1 loop
         for S in 0 .. Samples_Per_Col - 1 loop
            declare
               X         : constant Float :=
                 (Float (Col) + Float (S) / Float (Samples_Per_Col))
                 / Float (Graph_Width);
               Angle     : constant Float :=
                 2.0 * Ada.Numerics.Pi * Frequency * X + Phase;
               --  Sin returns -1.0 to 1.0, we only use 0.0 to 1.0
               --  so clamp negative values to 0
               Raw_Value : constant Float := Amplitude * Sin (Angle);
               Value     : constant Float :=
                 (if Raw_Value < 0.0 then 0.0 else Raw_Value);
               --  Map 0.0..1.0 to Graph_Height-1..0
               --  so 1.0 is at the top and 0.0 is at the bottom
               Row       : Integer :=
                 Graph_Height - 1 - Integer (Value * Float (Graph_Height - 1));
            begin
               if Row < 0 then
                  Row := 0;
               elsif Row > Graph_Height - 1 then
                  Row := Graph_Height - 1;
               end if;
               Grid (Row, Col) := True;
            end;
         end loop;
      end loop;

      --  Build the display string row by row from the grid
      for Row in 0 .. Graph_Height - 1 loop
         for Col in 0 .. Graph_Width - 1 loop
            if Grid (Row, Col) then
               Append (Result, '*');
            else
               Append (Result, ' ');
            end if;
         end loop;
         Append (Result, Character'Val (10));
      end loop;

      return Result;
   end Build_Graph_Text;

   --------------------------------------------------------
   -- HELPER: Build_Title_Text
   -- Builds the title bar string showing current parameter
   -- values so the user can see live feedback.
   --------------------------------------------------------
   function Build_Title_Text return Unbounded_String is
   begin
      return
        To_Unbounded_String
          ("  Sine Wave Visualiser"
           & "   AMP: "
           & Float_To_String (Amplitude)
           & "   FREQ: "
           & Float_To_String (Frequency)
           & "   SPEED: "
           & Float_To_String (Speed));
   end Build_Title_Text;

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   --------------------------------------------------------

   Comp_RenderInfo : constant Components.Render_Info_Component_T :=
     (Terminal_Width       => 80,
      Terminal_Height      => 24,
      Prev_Terminal_Width  => 80,
      Prev_Terminal_Height => 24,
      Framebuffer_1        =>
        (Width => 80, Height => 24, Data => new Pixel_Array),
      Framebuffer_2        =>
        (Width => 80, Height => 24, Data => new Pixel_Array),
      Drawing_FB           => new Graphics.Protected_DB,
      Backbuffer           =>
        (Width => 80, Height => 24, Data => new Pixel_Array));

   Comp_Root_Widget : constant Components.Widget_Component_T :=
     (Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      =>
        IDs.Entity_ID_Vector.To_Vector (E_TitleBar, 1) & E_Graph & E_StatusBar,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True);

   Comp_Root_Marker : constant Components.Root_Widget_Component_T :=
     (null record);

   Comp_Root_Flex : constant Components.Flex_Layout_Component_T :=
     (Flex_Container =>
        (Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 3,
         Items      =>
           new Flexbox.Flex_Item_Array'
             (
              --  Title bar: fixed 1 row
              1 =>
                (Related_Entity => E_TitleBar,
                 Flex_Basis     => 1,
                 Flex_Grow      => 0.0,
                 Flex_Shrink    => 0.0,
                 Computed_Size  => 1,
                 Cross_Size     => 80,
                 Position_X     => 0,
                 Position_Y     => 0),
              --  Graph: grows to fill remaining space
              2 =>
                (Related_Entity => E_Graph,
                 Flex_Basis     => 0,
                 Flex_Grow      => 1.0,
                 Flex_Shrink    => 0.0,
                 Computed_Size  => 22,
                 Cross_Size     => 80,
                 Position_X     => 0,
                 Position_Y     => 0),
              --  Status bar: fixed 1 row
              3 =>
                (Related_Entity => E_StatusBar,
                 Flex_Basis     => 1,
                 Flex_Grow      => 0.0,
                 Flex_Shrink    => 0.0,
                 Computed_Size  => 1,
                 Cross_Size     => 80,
                 Position_X     => 0,
                 Position_Y     => 0))),
      Is_Dirty       => True);

   --  Title bar: blue background, shows live parameters
   Comp_TitleBar_Widget : constant Components.Widget_Component_T :=
     (Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True);

   Comp_TitleBar_BG : constant Components.Background_Color_Component_T :=
     (Background_Color => Graphics.Blue);

   Comp_TitleBar_Text : Components.Text_Component_T :=
     (Text             => Null_Unbounded_String,
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => True,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False);

   Comp_TitleBar_PositionMode :
     constant Components.Position_Mode_Component_T :=
       (Mode => Components.Flex);

   --  Graph widget: gray background, displays the sine wave
   Comp_Graph_Widget : constant Components.Widget_Component_T :=
     (Position_X    => 1,
      Position_Y    => 2,
      Size_Width    => 80,
      Size_Height   => 22,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 22, Data => new Pixel_Array),
      Has_Focus     => True,
      Is_Visible    => True,
      Is_Enabled    => True);

   Comp_Graph_BG : constant Components.Background_Color_Component_T :=
     (Background_Color => Graphics.Gray);

   Comp_Graph_Text : Components.Text_Component_T :=
     (Text             => Null_Unbounded_String,
      Text_Color       => Graphics.Cyan,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False);

   Comp_Graph_PositionMode : constant Components.Position_Mode_Component_T :=
     (Mode => Components.Flex);

   --  Status bar: blue background, shows keybindings
   Comp_StatusBar_Widget : constant Components.Widget_Component_T :=
     (Position_X    => 1,
      Position_Y    => 23,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True);

   Comp_StatusBar_BG : constant Components.Background_Color_Component_T :=
     (Background_Color => Graphics.Blue);

   Comp_StatusBar_Text : constant Components.Text_Component_T :=
     (Text             =>
        To_Unbounded_String
          ("  w/s: amplitude  |  a/d: frequency  |  q/e: speed  |  ESC: quit"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False);

   Comp_StatusBar_PositionMode :
     constant Components.Position_Mode_Component_T :=
       (Mode => Components.Flex);

begin
   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   ECS.Add_Component
     (C_RenderInfo.all, IDs.To_CID ("RenderInfo"), Comp_RenderInfo);

   ECS.Add_Component
     (C_Root.all, IDs.To_CID ("WidgetComponent"), Comp_Root_Widget);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("RootWidget"), Comp_Root_Marker);
   ECS.Add_Component
     (C_Root.all, IDs.To_CID ("FlexLayoutComponent"), Comp_Root_Flex);

   ECS.Add_Component
     (C_TitleBar.all, IDs.To_CID ("WidgetComponent"), Comp_TitleBar_Widget);
   ECS.Add_Component
     (C_TitleBar.all,
      IDs.To_CID ("BackgroundColorComponent"),
      Comp_TitleBar_BG);
   ECS.Add_Component
     (C_TitleBar.all, IDs.To_CID ("TextComponent"), Comp_TitleBar_Text);
   ECS.Add_Component
     (C_TitleBar.all, IDs.To_CID ("PositionMode"), Comp_TitleBar_PositionMode);

   ECS.Add_Component
     (C_Graph.all, IDs.To_CID ("WidgetComponent"), Comp_Graph_Widget);
   ECS.Add_Component
     (C_Graph.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Graph_BG);
   ECS.Add_Component
     (C_Graph.all, IDs.To_CID ("TextComponent"), Comp_Graph_Text);
   ECS.Add_Component
     (C_Graph.all, IDs.To_CID ("PositionMode"), Comp_Graph_PositionMode);

   ECS.Add_Component
     (C_StatusBar.all, IDs.To_CID ("WidgetComponent"), Comp_StatusBar_Widget);
   ECS.Add_Component
     (C_StatusBar.all,
      IDs.To_CID ("BackgroundColorComponent"),
      Comp_StatusBar_BG);
   ECS.Add_Component
     (C_StatusBar.all, IDs.To_CID ("TextComponent"), Comp_StatusBar_Text);
   ECS.Add_Component
     (C_StatusBar.all,
      IDs.To_CID ("PositionMode"),
      Comp_StatusBar_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- START INPUT READER
   --------------------------------------------------------
   Input_Handling.Input_Reader.Start;

   --------------------------------------------------------
   -- MAIN LOOP
   -- Runs indefinitely until ESC is pressed.
   -- Each frame:
   --   1. Process input to adjust wave parameters
   --   2. Advance the phase offset to scroll the wave
   --   3. Rebuild the graph and title text components
   --   4. Run all ECS systems to render the frame
   --------------------------------------------------------
   loop
      --------------------------------------------------------
      -- PROCESS INPUT
      --------------------------------------------------------
      declare
         Event     : Input_Handling.Input_Event_t;
         Got_Input : Boolean := True;
      begin
         while Got_Input loop
            Input_Handling.Input_Buffer.Consume (Event);

            if Event.Char_Value = Character'Val (0) then
               Got_Input := False;

            else
               case Event.Cmd is
                  when Input_Handling.Quit =>
                     Should_Quit := True;

                  when others              =>
                     case Event.Char_Value is

                        --  w: increase amplitude

                        when 'w'    =>
                           if Amplitude + Amp_Step <= Amp_Max then
                              Amplitude := Amplitude + Amp_Step;
                           end if;

                        --  s: decrease amplitude

                        when 's'    =>
                           if Amplitude - Amp_Step >= Amp_Min then
                              Amplitude := Amplitude - Amp_Step;
                           end if;

                        --  d: increase frequency

                        when 'd'    =>
                           if Frequency + Freq_Step <= Freq_Max then
                              Frequency := Frequency + Freq_Step;
                           end if;

                        --  a: decrease frequency

                        when 'a'    =>
                           if Frequency - Freq_Step >= Freq_Min then
                              Frequency := Frequency - Freq_Step;
                           end if;

                        --  e: increase speed

                        when 'e'    =>
                           if Speed + Speed_Step <= Speed_Max then
                              Speed := Speed + Speed_Step;
                           end if;

                        --  q: decrease speed

                        when 'q'    =>
                           if Speed - Speed_Step >= Speed_Min then
                              Speed := Speed - Speed_Step;
                           end if;

                        when others =>
                           null;
                     end case;
               end case;
            end if;
         end loop;
      end;

      exit when Should_Quit;

      --------------------------------------------------------
      -- ADVANCE PHASE
      -- Increment the phase offset each frame by the speed
      -- value scaled to keep the scrolling smooth at 30 FPS.
      --------------------------------------------------------
      Phase := Phase - (Speed * 0.15);

      --------------------------------------------------------
      -- REBUILD TEXT COMPONENTS
      --------------------------------------------------------
      Comp_Graph_Text.Text := Build_Graph_Text;
      Comp_TitleBar_Text.Text := Build_Title_Text;

      Entities_PO.Claim_Writing (Entities_Ptr);
      ECS.Add_Component
        (C_Graph.all, IDs.To_CID ("TextComponent"), Comp_Graph_Text);
      ECS.Add_Component
        (C_TitleBar.all, IDs.To_CID ("TextComponent"), Comp_TitleBar_Text);
      Entities_PO.Release_Writing;

      --------------------------------------------------------
      -- SYSTEMS
      --------------------------------------------------------
      ECS.TerminalResizeSystem (Entities_PO);
      ECS.FlexLayoutSystem (Entities_PO);
      ECS.WidgetBackgroundSystem (Entities_PO);
      ECS.TextRenderSystem (Entities_PO);
      ECS.BufferCopySystem (Entities_PO);
      ECS.BufferDrawSystem (Entities_PO);
      ECS.DoubleBufferFlagSystem (Entities_PO);

      delay Duration (0.016); -- ~30 FPS
   end loop;

   --------------------------------------------------------
   -- SHUTDOWN
   --------------------------------------------------------
   Input_Handling.Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Sine_Wave_Demo;