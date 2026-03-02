-- =======================================================================
-- This widget demonstrates a text editor with numbered lines only on
-- lines with which the user has input denoted by incrementing numbers.
-- Lines without user input will be marked as "~", the user will be able
-- to navigate these lines with the arrow keys "up", "down" to navigate
-- vertically and "left", "right" to navigate text horizontally. 
-- Pressing "i" to insert text, pressing "esc" to exit text inserting 
-- mode will put the user back in navigation mode to navigate with arrow keys.
--
-- Additional keybinding ideas: "a" to enter text insertion mode after
-- the cursor and "shift + i" to start at beginning of current line and
-- "shift + a" to start at the end of the current line.
-- =======================================================================
with Ada.Text_IO;
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

procedure Text_Editor_Demo is

   Loop_Count : constant Positive := 6000; -- ~3 minutes at 30 FPS

   --  ECS Entity storage
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --------------------------------------------------------
   E_RenderInfo  : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root        : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_TitleBar    : constant IDs.Entity_Id := IDs.To_EID ("TitleBar");
   E_Editor      : constant IDs.Entity_Id := IDs.To_EID ("Editor");
   E_StatusBar   : constant IDs.Entity_Id := IDs.To_EID ("StatusBar");

   C_RenderInfo  : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root        : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_TitleBar    : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_TitleBar);
   C_Editor      : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Editor);
   C_StatusBar   : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_StatusBar);

   --------------------------------------------------------
   -- EDITOR STATE
   -- We manage text as a plain Unbounded_String and rebuild
   -- the TextComponent each frame. This is simpler than a
   -- custom ECS component and fits the existing framework.
   --------------------------------------------------------

   --  The editor width is the widget width minus 2 (1 char padding each side)
   Editor_Width     : constant Positive := 78;

   --  All typed text lives here as one flat string
   Editor_Text      : Unbounded_String := Null_Unbounded_String;

   --  Are we in insert mode (typed 'i') or normal mode?
   Insert_Mode      : Boolean := False;

   --  Should the main loop exit?
   Should_Quit      : Boolean := False;

   --------------------------------------------------------
   -- HELPER: Word-wrap a flat string into a fixed-width string
   -- The TextRenderSystem expects a single Unbounded_String.
   -- We embed newlines so each wrapped line starts at the
   -- correct column when the system renders row by row.
   -- Returns a string where every line is exactly Width chars
   -- wide (padded with spaces), with a pipe cursor appended
   -- at the insertion point when In_Insert is True.
   --------------------------------------------------------
   function Wrap_Text (
      Raw       : Unbounded_String;
      Width     : Positive;
      In_Insert : Boolean
   ) return Unbounded_String is
      --  Append cursor marker to raw text before wrapping
      Full   : constant String :=
         To_String (Raw) & (if In_Insert then "|" else "");
      Result : Unbounded_String := Null_Unbounded_String;
      Pos    : Natural := Full'First;
      Col    : Natural := 0;
   begin
      --  Walk every character and insert a newline whenever
      --  we hit the column boundary
      while Pos <= Full'Last loop
         Append (Result, Full (Pos));
         Col := Col + 1;
         if Col = Width then
            Append (Result, Character'Val (10));  -- LF
            Col := 0;
         end if;
         Pos := Pos + 1;
      end loop;
      return Result;
   end Wrap_Text;

   --------------------------------------------------------
   -- HELPER: Build the status bar label
   --------------------------------------------------------
   function Status_Text (In_Insert : Boolean) return Unbounded_String is
   begin
      if In_Insert then
         return To_Unbounded_String (
            "-- INSERT --   press ESC to exit insert mode"
         );
      else
         return To_Unbounded_String (
            "NORMAL   press 'i' to insert text, ESC to quit"
         );
      end if;
   end Status_Text;

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

   --  Root: full screen, Column flex, three children:
   --  TitleBar (1 row) | Editor (grows) | StatusBar (1 row)
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
            --  Title bar: fixed 1 row
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
            --  Editor: takes all remaining rows
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
            --  Status bar: fixed 1 row at bottom
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

   --  Title bar: white text on blue, fixed 1 row
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

   --  Editor: gray background, grows to fill space
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

   --  Text component: rebuilt every frame from Editor_Text
   Comp_Editor_Text : Components.Text_Component_T := (
      Text             => Wrap_Text (Editor_Text, Editor_Width, Insert_Mode),
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

   --  Status bar: shows current mode
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
      Text             => Status_Text (Insert_Mode),
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
   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"),           Comp_RenderInfo);

   ECS.Add_Component (C_Root.all,       IDs.To_CID ("WidgetComponent"),      Comp_Root_Widget);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("RootWidget"),           Comp_Root_Marker);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("FlexLayoutComponent"),  Comp_Root_Flex);

   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("WidgetComponent"),      Comp_TitleBar_Widget);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("BackgroundColorComponent"), Comp_TitleBar_BG);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("TextComponent"),        Comp_TitleBar_Text);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("PositionMode"),         Comp_TitleBar_PositionMode);

   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("WidgetComponent"),      Comp_Editor_Widget);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("BackgroundColorComponent"), Comp_Editor_BG);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("TextComponent"),        Comp_Editor_Text);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("PositionMode"),         Comp_Editor_PositionMode);

   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("WidgetComponent"),      Comp_StatusBar_Widget);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("BackgroundColorComponent"), Comp_StatusBar_BG);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("TextComponent"),        Comp_StatusBar_Text);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("PositionMode"),         Comp_StatusBar_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- START INPUT READER TASK
   --------------------------------------------------------
   Input_Handling.Input_Reader.Start;

   --------------------------------------------------------
   -- MAIN LOOP
   --------------------------------------------------------
   for Loop_Index in 1 .. Loop_Count loop

      --------------------------------------------------------
      -- PROCESS ALL PENDING INPUT EVENTS THIS FRAME
      -- We drain the entire queue each frame so fast typists
      -- don't fall behind.
      --------------------------------------------------------
      declare
         Event     : Input_Handling.Input_Event_t;
         Got_Input : Boolean := True;
      begin
         while Got_Input loop
            Input_Handling.Input_Buffer.Consume (Event);

            --  NUL means queue was empty, stop draining
            if Event.Char_Value = Character'Val (0) then
               Got_Input := False;

            elsif Insert_Mode then
               --  INSERT MODE: ESC exits, Backspace deletes, else append
               case Event.Cmd is
                  when Input_Handling.Quit =>
                     --  ESC pressed: leave insert mode, don't quit
                     Insert_Mode := False;

                  when Input_Handling.Enter =>
                     --  Enter: append a newline character
                     Append (Editor_Text, Character'Val (10));

                  when others =>
                     --  Printable character or backspace
                     if Event.Char_Value = Character'Val (127) then
                        --  Backspace: remove last character if any
                        if Length (Editor_Text) > 0 then
                           Delete (Editor_Text,
                                   Length (Editor_Text),
                                   Length (Editor_Text));
                        end if;
                     elsif Event.Char_Value >= ' ' then
                        --  Printable: append to buffer
                        Append (Editor_Text, Event.Char_Value);
                     end if;
               end case;

            else
               --  NORMAL MODE: 'i' enters insert, ESC quits
               case Event.Cmd is
                  when Input_Handling.Quit =>
                     Should_Quit := True;

                  when others =>
                     if Event.Char_Value = 'i' then
                        Insert_Mode := True;
                     end if;
               end case;
            end if;
         end loop;
      end;

      exit when Should_Quit;

      --------------------------------------------------------
      -- REBUILD TEXT COMPONENTS FROM CURRENT STATE
      -- We update the component records then write them back
      -- into ECS so the render systems see fresh data.
      --------------------------------------------------------
      Comp_Editor_Text.Text   := Wrap_Text (Editor_Text, Editor_Width, Insert_Mode);
      Comp_StatusBar_Text.Text := Status_Text (Insert_Mode);

      Entities_PO.Claim_Writing (Entities_Ptr);
      ECS.Add_Component (C_Editor.all,    IDs.To_CID ("TextComponent"), Comp_Editor_Text);
      ECS.Add_Component (C_StatusBar.all, IDs.To_CID ("TextComponent"), Comp_StatusBar_Text);
      Entities_PO.Release_Writing;

      --------------------------------------------------------
      -- SYSTEMS
      --------------------------------------------------------
      ECS.TerminalResizeSystem  (Entities_PO);
      ECS.FlexLayoutSystem      (Entities_PO);
      ECS.WidgetBackgroundSystem(Entities_PO);
      ECS.TextRenderSystem      (Entities_PO);
      ECS.BufferCopySystem      (Entities_PO);
      ECS.BufferDrawSystem      (Entities_PO);
      ECS.DoubleBufferFlagSystem(Entities_PO);

      delay Duration (0.033); -- ~30 FPS
   end loop;

   --------------------------------------------------------
   -- SHUTDOWN
   --------------------------------------------------------
   Input_Handling.Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Text_Editor_Demo;