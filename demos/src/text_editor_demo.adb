with Ada.Wide_Wide_Text_IO;
with Components;
with Console;
with ECS;
with Graphics;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
use Graphics;
with IDs;
use type IDs.Entity_ID_Vector.Vector;
with Flexbox;
with Input_Handling;
with Text_Editor;
use Text_Editor;
with Scroll;

procedure Text_Editor_Demo is

   ------------------------------------------------------------------------------
   --  Text_Editor_Demo.adb
   --
   --  PURPOSE
   --  -------
   --  A modal text editor built on the Thuja TUI framework, inspired by the
   --  editing model of Vim. Demonstrates interactive keyboard input,
   --  stateful mode switching, and dynamic text rendering within an ECS
   --  widget hierarchy.
   --
   --  VISUAL LAYOUT
   --  -------------
   --  ┌────────────────────────────────────────────────────────────────┐
   --  │ Thuja Text Editor                          (Title Bar)         │
   --  ├────────────────────────────────────────────────────────────────┤
   --  │  1  │ User typed text here...                                  │
   --  │  2  │ More text that wraps at the widget boundary              │
   --  │  ~  │                                                          │
   --  │  ~  │                                                          │
   --  ├────────────────────────────────────────────────────────────────┤
   --  │ NORMAL  press 'i' to insert text, ESC to quit  (Status Bar)   │
   --  └────────────────────────────────────────────────────────────────┘
   --
   --  MODES
   --  -----
   --  NAVIGATION  Default mode. Move with w/a/s/d. Empty lines show "~".
   --              Cursor remembers its column when moving across shorter
   --              lines (sticky column behaviour). View scrolls to keep
   --              cursor always visible.
   --
   --  INSERT      Entered by pressing 'i'. Characters are inserted at the
   --              cursor position. Text wraps automatically at the widget
   --              boundary. Enter creates a new line. ESC returns to
   --              Navigation mode.
   --
   --  KEYBINDINGS
   --  -----------
   --  i           Enter Insert mode at the current cursor position
   --  w           Move cursor up (Navigation mode)
   --  s           Move cursor down (Navigation mode)
   --  a           Move cursor left (Navigation mode)
   --  d           Move cursor right (Navigation mode)
   --  Tab         Insert 4 spaces at cursor position (Insert mode)
   --  Backspace   Delete character before cursor (Insert mode)
   --  Enter       Insert a new line at cursor (Insert mode)
   --  ESC         Return to Navigation mode / quit from Navigation mode
   --
   --  FEATURES DEMONSTRATED
   --  ---------------------
   --  • Modal editing with Navigation and Insert states
   --  • Line numbering: active lines show sequential numbers, empty
   --    lines display "~" in the gutter
   --  • Sticky column cursor: remembers intended column across shorter lines
   --  • Vertical scrolling: view follows cursor automatically
   --  • Text wrapping at widget boundary
   --  • Vector-of-lines internal buffer for clean line-aware operations
   --  • Input_Handling task integration for non-blocking keyboard input
   --  • Three-widget Flex layout: Title Bar / Editor / Status Bar
   ------------------------------------------------------------------------------

   Loop_Count : constant Positive := 6000;

   --  ECS Entity storage
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- SCROLL STATE
   -- Owned by the demo, passed into Scroll.Update and
   -- Build_Editor_Text each frame.
   --------------------------------------------------------
   Scroll_Offset : Natural  := 0;
   Visible_Rows  : constant Positive := 22;

   Should_Quit : Boolean := False;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --------------------------------------------------------
   E_RenderInfo : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root       : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_TitleBar   : constant IDs.Entity_Id := IDs.To_EID ("TitleBar");
   E_Editor     : constant IDs.Entity_Id := IDs.To_EID ("Editor");
   E_StatusBar  : constant IDs.Entity_Id := IDs.To_EID ("StatusBar");

   C_RenderInfo : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root       : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_TitleBar   : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_TitleBar);
   C_Editor     : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Editor);
   C_StatusBar  : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_StatusBar);

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   --------------------------------------------------------

   Comp_RenderInfo : constant Components.Render_Info_Component_T := (
      Terminal_Width       => 80,
      Terminal_Height      => 24,
      Prev_Terminal_Width  => 80,
      Prev_Terminal_Height => 24,
      Framebuffer_1        => (Width => 80, Height => 24, Data => new Pixel_Array),
      Framebuffer_2        => (Width => 80, Height => 24, Data => new Pixel_Array),
      Drawing_FB           => new Graphics.Protected_DB,
      Backbuffer           => (Width => 80, Height => 24, Data => new Pixel_Array)
   );

   Comp_Root_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_TitleBar, 1)
                       & E_Editor & E_StatusBar,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   Comp_Root_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 3,
         Items      => new Flexbox.Flex_Item_Array'(
            1 => (
               Related_Entity => E_TitleBar,
               Flex_Basis     => 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            2 => (
               Related_Entity => E_Editor,
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 22,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            3 => (
               Related_Entity => E_StatusBar,
               Flex_Basis     => 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   Comp_TitleBar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_TitleBar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   Comp_TitleBar_Text : constant Components.Text_Component_T := (
      Text             => To_Unbounded_String ("  Thuja Text Editor"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => True,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_TitleBar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   Comp_Editor_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 2,
      Size_Width    => 80,
      Size_Height   => 22,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 22, Data => new Pixel_Array),
      Has_Focus     => True,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Editor_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Gray
   );

   Comp_Editor_Text : Components.Text_Component_T := (
      Text             => Ada.Strings.Unbounded.Null_Unbounded_String,
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_Editor_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   Comp_StatusBar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 23,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_StatusBar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   Comp_StatusBar_Text : Components.Text_Component_T := (
      Text             => Ada.Strings.Unbounded.Null_Unbounded_String,
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_StatusBar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

begin
   --  Initialise the text editor buffer
   Text_Editor.Initialise;

   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"),              Comp_RenderInfo);

   ECS.Add_Component (C_Root.all,       IDs.To_CID ("WidgetComponent"),          Comp_Root_Widget);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("RootWidget"),               Comp_Root_Marker);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("FlexLayoutComponent"),      Comp_Root_Flex);

   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("WidgetComponent"),          Comp_TitleBar_Widget);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("BackgroundColorComponent"), Comp_TitleBar_BG);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("TextComponent"),            Comp_TitleBar_Text);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("PositionMode"),             Comp_TitleBar_PositionMode);

   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("WidgetComponent"),          Comp_Editor_Widget);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("BackgroundColorComponent"), Comp_Editor_BG);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("TextComponent"),            Comp_Editor_Text);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("PositionMode"),             Comp_Editor_PositionMode);

   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("WidgetComponent"),          Comp_StatusBar_Widget);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("BackgroundColorComponent"), Comp_StatusBar_BG);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("TextComponent"),            Comp_StatusBar_Text);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("PositionMode"),             Comp_StatusBar_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- START INPUT READER
   --------------------------------------------------------
   Input_Handling.Input_Reader.Start;

   --------------------------------------------------------
   -- MAIN LOOP
   --------------------------------------------------------
   for Loop_Index in 1 .. Loop_Count loop

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

            elsif Text_Editor.Mode = Text_Editor.Insert then
               case Event.Cmd is

                  when Input_Handling.Quit =>
                     Text_Editor.Mode := Text_Editor.Navigation;
                     Text_Editor.Clamp_Col;

                  when Input_Handling.Enter =>
                     Text_Editor.Handle_Enter;

                  when Input_Handling.Tab =>
                     Text_Editor.Handle_Tab;

                  when others =>
                     if Event.Char_Value = Character'Val (127)
                        or else Event.Char_Value = Character'Val (8)
                     then
                        Text_Editor.Handle_Backspace;
                     elsif Event.Char_Value >= ' ' then
                        Text_Editor.Insert_Char (Event.Char_Value);
                     end if;

               end case;

            else
               --  NAVIGATION MODE
               case Event.Cmd is
                  when Input_Handling.Quit =>
                     Should_Quit := True;
                  when others =>
                     Text_Editor.Handle_Navigation (Event.Char_Value);
               end case;
            end if;
         end loop;
      end;

      exit when Should_Quit;

      --------------------------------------------------------
      -- UPDATE SCROLL
      -- Called after input processing so Scroll_Offset always
      -- reflects the latest cursor position before rendering.
      --------------------------------------------------------
      Scroll.Update
         (Current_Line  => Text_Editor.Current_Line,
          Total_Lines   => Natural (Text_Editor.Lines.Length),
          Visible_Rows  => Visible_Rows,
          Scroll_Offset => Scroll_Offset);

      --------------------------------------------------------
      -- REBUILD TEXT COMPONENTS
      --------------------------------------------------------
      Comp_Editor_Text.Text    :=
         Text_Editor.Build_Editor_Text (Scroll_Offset, Visible_Rows);
      Comp_StatusBar_Text.Text := Text_Editor.Status_Text;

      Entities_PO.Claim_Writing (Entities_Ptr);
      ECS.Add_Component (C_Editor.all,    IDs.To_CID ("TextComponent"), Comp_Editor_Text);
      ECS.Add_Component (C_StatusBar.all, IDs.To_CID ("TextComponent"), Comp_StatusBar_Text);
      Entities_PO.Release_Writing;

      --------------------------------------------------------
      -- SYSTEMS
      --------------------------------------------------------
      ECS.TerminalResizeSystem   (Entities_PO);
      ECS.FlexLayoutSystem       (Entities_PO);
      ECS.WidgetBackgroundSystem (Entities_PO);
      ECS.TextRenderSystem       (Entities_PO);
      ECS.BufferCopySystem       (Entities_PO);
      ECS.BufferDrawSystem       (Entities_PO);
      ECS.DoubleBufferFlagSystem (Entities_PO);

      delay Duration (0.033);
   end loop;

   --------------------------------------------------------
   -- SHUTDOWN
   --------------------------------------------------------
   Input_Handling.Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Text_Editor_Demo;
