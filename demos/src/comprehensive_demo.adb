------------------------------------------------------------------------------
--  Comprehensive_Demo.adb
--
--  PURPOSE
--  -------
--  Demonstrates ALL new features added to the Thuja TUI framework:
--    1. Terminal Resize Detection (TerminalResizeSystem)
--    2. Widget Movement API (Move_Widget, Move_Widget_By)
--    3. Flexbox Layout with Position Modes (Flex, Absolute, Relative)
--    4. Progress Bar Rendering
--    5. Multi-threaded rendering pipeline
--
--  VISUAL LAYOUT
--  -------------
--  ┌────────────────────────────────────────────────────────────────┐
--  │ HEADER (Blue, 3 rows tall, flex positioned)                   │
--  ├────────────────────────────────────────────────────────────────┤
--  │ [████████████████████████████       ] 75%  (Progress Bar)      │
--  ├────────────────────────────────────────────────────────────────┤
--  │ SIDEBAR       │ CONTENT AREA                                   │
--  │ (Green)       │ (Red with moving yellow dot)                   │
--  │ 20 cols wide  │ Grows to fill remaining space                  │
--  │               │                                                 │
--  └────────────────────────────────────────────────────────────────┘
--
--  FEATURES DEMONSTRATED
--  ---------------------
--  • Header uses Flex positioning (controlled by FlexLayoutSystem)
--  • Progress bar shows animated progress from 0% to 100%
--  • Sidebar uses Flex positioning with fixed width
--  • Content area uses Flex positioning and grows to fill space
--  • Yellow dot uses ABSOLUTE positioning and moves independently
--  • Terminal resize triggers layout recalculation automatically
--  • Multi-threaded rendering at 30 FPS
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Components;
with ECS;
with Graphics;
with Ada.Strings;
with Ada.Strings.Unbounded;
use Graphics;
with IDs;
use type IDs.Entity_ID_Vector.Vector;
with Flexbox;

procedure Comprehensive_Demo is

   --  Number of frames to run
   Loop_Count : constant Positive := 300;  -- 10 seconds at 30 FPS

   --  ECS Entity storage
   Entities_PO : ECS.Entity_Components_PO;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --------------------------------------------------------
   E_RenderInfo   : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root         : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_Header       : constant IDs.Entity_Id := IDs.To_EID ("Header");
   E_ProgressBar  : constant IDs.Entity_Id := IDs.To_EID ("ProgressBar");
   E_Sidebar      : constant IDs.Entity_Id := IDs.To_EID ("Sidebar");
   E_Content      : constant IDs.Entity_Id := IDs.To_EID ("Content");
   E_MovingDot    : constant IDs.Entity_Id := IDs.To_EID ("MovingDot");
   --------------------------------------------------------

   --  Register entities
   C_RenderInfo   : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root         : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_Header       : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Header);
   C_ProgressBar  : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_ProgressBar);
   C_Sidebar      : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Sidebar);
   C_Content      : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Content);
   C_MovingDot    : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_MovingDot);

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   --------------------------------------------------------

   --  Render Info: Terminal state and framebuffers
   Comp_RenderInfo : constant Components.Render_Info_Component_T := (
      Terminal_Width      => 80,
      Terminal_Height     => 24,
      Prev_Terminal_Width => 80,
      Prev_Terminal_Height => 24,
      Framebuffer_1       => (Width => 80, Height => 24, Data => new Pixel_Array),
      Framebuffer_2       => (Width => 80, Height => 24, Data => new Pixel_Array),
      Drawing_FB          => new Graphics.Protected_DB,
      Backbuffer          => (Width => 80, Height => 24, Data => new Pixel_Array)
   );

   --  Root Widget: Main container using Column layout
   Comp_Root_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_Header, 1) & E_ProgressBar & E_Sidebar & E_Content & E_MovingDot,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   --  Root Flexbox Layout: Vertical stacking (Column)
   Comp_Root_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 4,  -- Header, ProgressBar, Sidebar Row (combined), Content
         Items      => new Flexbox.Flex_Item_Array'(
            --  HEADER: Fixed 3 rows
            1 => (
               Related_Entity => E_Header,
               Flex_Basis     => 3,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 3,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            --  PROGRESS BAR: Fixed 1 row
            2 => (
               Related_Entity => E_ProgressBar,
               Flex_Basis     => 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            --  SIDEBAR: Fixed 10 rows (half of remaining)
            3 => (
               Related_Entity => E_Sidebar,
               Flex_Basis     => 10,
               Flex_Grow      => 0.5,
               Flex_Shrink    => 0.0,
               Computed_Size  => 10,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            --  CONTENT: Grows to fill remaining space
            4 => (
               Related_Entity => E_Content,
               Flex_Basis     => 0,
               Flex_Grow      => 0.5,
               Flex_Shrink    => 0.0,
               Computed_Size  => 10,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   --  Header Widget: Blue background
   Comp_Header_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 3,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 3, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Header_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   Comp_Header_Text : constant Components.Text_Component_T := (
      Text          => Ada.Strings.Unbounded.To_Unbounded_String ("THUJA TUI FRAMEWORK - FEATURE DEMO"),
      Text_Color    => Graphics.White,
      Offset_X      => 1,
      Offset_Y      => 2,
      Is_Bold       => True,
      Is_Italic     => True,
      Is_Underline  => False,
      Is_Strikethrough => False
   );

   Comp_Header_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --  Progress Bar Widget
   Comp_ProgressBar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 4,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_ProgressBar : Components.Progress_Bar_Component_T := (
      Value         => 0.0,
      Filled_Char   => '=',
      Empty_Char    => ' ',
      Border_Left   => '[',
      Border_Right  => ']',
      Filled_Color  => Graphics.Cyan,
      Empty_Color   => Graphics.Gray,
      Show_Percentage => True
   );

   Comp_ProgressBar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Black
   );

   Comp_ProgressBar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --  Sidebar Widget: Green, 20 columns wide
   Comp_Sidebar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 5,
      Size_Width    => 20,
      Size_Height   => 10,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 20, Height => 10, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Sidebar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Green
   );

   Comp_Sidebar_Text : constant Components.Text_Component_T := (
      Text          => Ada.Strings.Unbounded.To_Unbounded_String ("SIDEBAR"),
      Text_Color    => Graphics.Black,
      Offset_X      => 1,
      Offset_Y      => 2,
      Is_Bold       => True,
      Is_Italic     => False,
      Is_Underline  => False,
      Is_Strikethrough => False
   );

   Comp_Sidebar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --  Content Area Widget: Red, grows to fill space
   Comp_Content_Widget : constant Components.Widget_Component_T := (
      Position_X    => 21,
      Position_Y    => 5,
      Size_Width    => 60,
      Size_Height   => 10,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 60, Height => 10, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Content_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Red
   );

   Comp_Content_Text : constant Components.Text_Component_T := (
      Text          => Ada.Strings.Unbounded.To_Unbounded_String ("CONTENT AREA - Watch the yellow dot or the progress bar maybe!"),
      Text_Color    => Graphics.White,
      Offset_X      => 1,
      Offset_Y      => 2,
      Is_Bold       => False,
      Is_Italic     => False,
      Is_Underline  => True,
      Is_Strikethrough => False
   );

   Comp_Content_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --  Moving Dot Widget: Yellow, uses ABSOLUTE positioning
   Comp_MovingDot_Widget : constant Components.Widget_Component_T := (
      Position_X    => 40,
      Position_Y    => 12,
      Size_Width    => 1,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 1, Height => 1, Data => new Pixel_Array),
      Has_Focus     => True,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_MovingDot_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Black
   );

   Comp_MovingDot_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Absolute  -- KEY: Not controlled by Flexbox!
   );

   --  Movement state for the dot
   Dot_X : Graphics.TUI_Width := 40;
   Dot_Y : Graphics.TUI_Height := 12;
   Dot_DX : Integer := 1;
   Dot_DY : Integer := 1;

   --  Progress bar animation state
   Progress : Float := 0.0;

   --  Thread stopping flag
   protected Thread_Flag is
      procedure Stop;
      function Check return Boolean;
      private
         Should_End : Boolean := False;
   end Thread_Flag;

   protected body Thread_Flag is
      procedure Stop is
      begin
         Should_End := True;
      end Stop;

      function Check return Boolean is
      begin
         return Should_End;
      end Check;
   end Thread_Flag;

   --  Render thread declaration
   task Render_Thread;

   task body Render_Thread is
   begin
      Graphics.Clear_Screen;
      loop
         --  Run draw system (automatically claims access)
         ECS.BufferDrawSystem (Entities_PO);

         --  30 FPS
         delay Duration (1.0 / 30.0);

         --  Stop render thread when Thread_Flag set
         exit when Thread_Flag.Check;
      end loop;
   end Render_Thread;

begin

   -- VT_PROCESSING with new Set_Cursor_Visible
   -- NOTE: Hide_Cursor/Show_Cursor cannot be mixed with Set_Cursor_Visible otherwise
   -- it may over all just be ignored and show cursor regardless of command
   -- If running windows demo utilize Set_Cursor_Visible otherwise utilize
   -- Hide_Cursor/Show_Cursor on Linux? Confirm this with Linux users.
   Graphics.Enable_VT_Processing;
   Graphics.Set_Cursor_Visible (False);
   -- Graphics.Hide_Cursor;
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --------------------------------------------------------

   --  RenderInfo
   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"), Comp_RenderInfo);

   --  Root
   ECS.Add_Component (C_Root.all, IDs.To_CID ("WidgetComponent"), Comp_Root_Widget);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("RootWidget"), Comp_Root_Marker);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("FlexLayoutComponent"), Comp_Root_Flex);

   --  Header
   ECS.Add_Component (C_Header.all, IDs.To_CID ("WidgetComponent"), Comp_Header_Widget);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Header_BG);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("TextComponent"), Comp_Header_Text);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("PositionMode"), Comp_Header_PositionMode);

   --  Progress Bar
   ECS.Add_Component (C_ProgressBar.all, IDs.To_CID ("WidgetComponent"), Comp_ProgressBar_Widget);
   ECS.Add_Component (C_ProgressBar.all, IDs.To_CID ("ProgressBarComponent"), Comp_ProgressBar);
   ECS.Add_Component (C_ProgressBar.all, IDs.To_CID ("BackgroundColorComponent"), Comp_ProgressBar_BG);
   ECS.Add_Component (C_ProgressBar.all, IDs.To_CID ("PositionMode"), Comp_ProgressBar_PositionMode);

   --  Sidebar
   ECS.Add_Component (C_Sidebar.all, IDs.To_CID ("WidgetComponent"), Comp_Sidebar_Widget);
   ECS.Add_Component (C_Sidebar.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Sidebar_BG);
   ECS.Add_Component (C_Sidebar.all, IDs.To_CID ("TextComponent"), Comp_Sidebar_Text);
   ECS.Add_Component (C_Sidebar.all, IDs.To_CID ("PositionMode"), Comp_Sidebar_PositionMode);

   --  Content
   ECS.Add_Component (C_Content.all, IDs.To_CID ("WidgetComponent"), Comp_Content_Widget);
   ECS.Add_Component (C_Content.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Content_BG);
   ECS.Add_Component (C_Content.all, IDs.To_CID ("TextComponent"), Comp_Content_Text);
   ECS.Add_Component (C_Content.all, IDs.To_CID ("PositionMode"), Comp_Content_PositionMode);

   --  Moving Dot
   ECS.Add_Component (C_MovingDot.all, IDs.To_CID ("WidgetComponent"), Comp_MovingDot_Widget);
   ECS.Add_Component (C_MovingDot.all, IDs.To_CID ("BackgroundColorComponent"), Comp_MovingDot_BG);
   ECS.Add_Component (C_MovingDot.all, IDs.To_CID ("PositionMode"), Comp_MovingDot_PositionMode);

   --------------------------------------------------------
   -- MAIN LOOP
   --------------------------------------------------------
   for Loop_Index in 1 .. Loop_Count loop
         --  SYSTEM 1: Terminal Resize Detection (NEW FEATURE!)
         ECS.TerminalResizeSystem (Entities_PO);

         --  SYSTEM 2: Flexbox Layout (positions flex children)
         ECS.FlexLayoutSystem (Entities_PO);

         --  UPDATE PROGRESS BAR (Animate from 0% to 100%)
         Progress := Float (Loop_Index) / Float (Loop_Count);
         Comp_ProgressBar.Value := Progress;
         ECS.Add_Component (C_ProgressBar.all, IDs.To_CID ("ProgressBarComponent"), Comp_ProgressBar);

         --  MOVE THE DOT (keep it constrained inside the SIDEBAR)
         --  Compute next position then clamp/bounce against the live Sidebar widget bounds
         declare
            Entity_List_Write : ECS.Entity_Components_Ptr;
            Target_Comps : ECS.Components_Ptr;
            Target_Widget : Components.Widget_Component_T;
            Min_X, Min_Y, Max_X, Max_Y : Integer;
            Proposed_X, Proposed_Y : Integer;
            New_Dot_X, New_Dot_Y : Integer;
         begin
            -- Advance proposed position using integer arithmetic
            Proposed_X := Integer (Dot_X) + Dot_DX;
            Proposed_Y := Integer (Dot_Y) + Dot_DY;

            -- Snapshot ECS to read the Sidebar widget
            Entities_PO.Claim_Writing (Entity_List_Write);

            Target_Comps := ECS.Get_Entity_Components (Entity_List_Write.all, E_Sidebar);
            -- Try to read the sidebar widget; fall back to root if anything fails
            begin
               Target_Widget := Components.Widget_Component_T (
                  ECS.Get_Component (Target_Comps.all, IDs.To_CID ("WidgetComponent"))
               );
               Min_X := Integer (Target_Widget.Position_X);
               Min_Y := Integer (Target_Widget.Position_Y);
               Max_X := Min_X + Integer (Target_Widget.Size_Width) - 1;
               Max_Y := Min_Y + Integer (Target_Widget.Size_Height) - 1;
            exception
               when others =>
                  Min_X := Integer (Comp_Root_Widget.Position_X);
                  Min_Y := Integer (Comp_Root_Widget.Position_Y);
                  Max_X := Min_X + Integer (Comp_Root_Widget.Size_Width) - 1;
                  Max_Y := Min_Y + Integer (Comp_Root_Widget.Size_Height) - 1;
            end;

            -- Bounce and clamp: ensure the entire moving widget fits inside target bounds
            -- If the moving widget has width/height > 1, account for that by using its size
            declare
               Mov_Comps : constant ECS.Components_Ptr := ECS.Get_Entity_Components (Entity_List_Write.all, E_MovingDot);
               Mov_W : Integer := 1;
               Mov_H : Integer := 1;
            begin
               -- Try to read moving widget size; if it fails assume 1x1
               begin
                  Mov_W := Integer (Components.Widget_Component_T (
                     ECS.Get_Component (Mov_Comps.all, IDs.To_CID ("WidgetComponent"))
                  ).Size_Width);
                  Mov_H := Integer (Components.Widget_Component_T (
                     ECS.Get_Component (Mov_Comps.all, IDs.To_CID ("WidgetComponent"))
                  ).Size_Height);
               exception
                  when others =>
                     Mov_W := 1;
                     Mov_H := 1;
               end;

               -- Adjust max allowed top-left so the whole mover fits
               Max_X := Max_X - Mov_W + 1;
               Max_Y := Max_Y - Mov_H + 1;

               -- Start with proposed
               New_Dot_X := Proposed_X;
               New_Dot_Y := Proposed_Y;

               -- Horizontal bounce
               if New_Dot_X > Max_X then
                  New_Dot_X := Max_X;
                  Dot_DX := -Dot_DX;
               elsif New_Dot_X < Min_X then
                  New_Dot_X := Min_X;
                  Dot_DX := -Dot_DX;
               end if;

               -- Vertical bounce
               if New_Dot_Y > Max_Y then
                  New_Dot_Y := Max_Y;
                  Dot_DY := -Dot_DY;
               elsif New_Dot_Y < Min_Y then
                  New_Dot_Y := Min_Y;
                  Dot_DY := -Dot_DY;
               end if;

               -- Persist dot coords (typed)
               Dot_X := Graphics.TUI_Width (New_Dot_X);
               Dot_Y := Graphics.TUI_Height (New_Dot_Y);

               -- Apply movement using Move_Widget API
               --  This method *can* add a component, so writing access is required
               ECS.Move_Widget (Entity_List_Write.all, E_MovingDot, Dot_X, Dot_Y);
            end;

            Entities_PO.Release_Writing;
         end;

         --  SYSTEM 3-7: Rendering pipeline
         ECS.WidgetBackgroundSystem (Entities_PO);
         ECS.TextRenderSystem (Entities_PO);
         ECS.ProgressBarRenderSystem (Entities_PO);  -- NEW FEATURE!
         ECS.BufferCopySystem (Entities_PO);
         ECS.DoubleBufferFlagSystem (Entities_PO);

         delay Duration (0.033);  -- ~30 FPS
   end loop;

   --  Stop render thread before applying manual screen updates
   Thread_Flag.Stop;

   Graphics.Clear_Screen;
   Ada.Text_IO.Put_Line ("==============================================");
   Ada.Text_IO.Put_Line ("  THUJA FRAMEWORK DEMO COMPLETE!");
   Ada.Text_IO.Put_Line ("==============================================");
   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line ("Features demonstrated:");
   Ada.Text_IO.Put_Line ("  - Terminal resize detection");
   Ada.Text_IO.Put_Line ("  - Flexbox layout (Column direction)");
   Ada.Text_IO.Put_Line ("  - Position modes (Flex vs Absolute)");
   Ada.Text_IO.Put_Line ("  - Widget movement API");
   Ada.Text_IO.Put_Line ("  - Animated progress bars");
   Ada.Text_IO.Put_Line ("  - Multi-threaded rendering");
   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line ("Thank you for using Thuja!");

   -- VT_PROCESSING with new Set_Cursor_Visible
   Graphics.Set_Cursor_Visible (True);
   --Graphics.Show_Cursor;
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Comprehensive_Demo;
