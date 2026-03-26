with Ada.Wide_Wide_Text_IO;   --  Wide character text output, required for flushing terminal output
with Components;              --  Component type definitions (Widget, Background, Text, etc.)
with Console;                 --  Terminal setup: VT-Processing, cursor visibility
with ECS;                     --  Entity Component System: Add_Entity, Add_Component, all systems
with Graphics;                --  Color definitions, pixel types, cursor and screen operations
with Ada.Strings.Unbounded;   --  Unbounded_String type used for widget text content
use Ada.Strings.Unbounded;    --  Makes To_Unbounded_String and string operations directly visible
use Graphics;                 --  Makes color names (Blue, Red, Green etc.) directly visible
with IDs;                     --  Entity and component ID types: To_EID, To_CID
with Flexbox;                 --  Flexbox layout types: Column/Row direction, Flex_Start, Stretch, Flex_Item_Array
use type IDs.Entity_ID_Vector.Vector;  --  Enables & operator for building Children vectors
--  Uncomment the line below to enable input handling
--  with Input_Handling;  --  Keyboard input: Input_Reader task, Input_Buffer, Input_Event_t, Command_t

procedure Example_Demo is

   ------------------------------------------------------------------------------
   --  Example_Demo.adb
   --
   --  PURPOSE
   --  -------
   --  A reference demo for third party developers showing how to build a
   --  multi-widget layout using the Thuja TUI framework. This demo is
   --  intentionally simple and heavily commented so it can be used as a
   --  starting point for any new demo or application built on Thuja.
   --
   --  VISUAL LAYOUT
   --  -------------
   --  ┌────────────────────────────────────────────────────────────────┐
   --  │ HEADER WIDGET  (Blue, full width, flex column)                 │
   --  ├─────────────────────────────┬──────────────────────────────────┤
   --  │ LEFT WIDGET                 │ RIGHT WIDGET                     │
   --  │ (Green, flex row child)     │ (Red, flex row child)            │
   --  │ Grows to fill left half     │ Grows to fill right half         │
   --  └─────────────────────────────┴──────────────────────────────────┘
   --
   --  LAYOUT EXPLAINED
   --  ----------------
   --  The Root widget uses a Column flex layout with two children:
   --    1. Header widget  — fixed height, stretches full width
   --    2. Row container  — grows to fill remaining space, contains:
   --         a. Left widget  — grows to fill left half (Flex_Grow 0.5)
   --         b. Right widget — grows to fill right half (Flex_Grow 0.5)
   --
   --  HOW TO USE THIS DEMO AS A REFERENCE
   --  ------------------------------------
   --  1. Copy this file and rename the procedure to your demo name
   --  2. Add your entity definitions in the ENTITY DEFINITIONS section
   --  3. Register entities with ECS.Add_Entity
   --  4. Define your component data records in COMPONENT DEFINITIONS
   --  5. Register components with ECS.Add_Component inside Claim_Writing
   --  6. Add your updated logic inside the MAIN LOOP
   --  7. Call the appropriate ECS systems each frame in order
   --  8. Uncomment input handling if your demo requires keyboard input
   ------------------------------------------------------------------------------

   --  Number of frames to run before the demo exits automatically.
   --  At 30 FPS, 300 frames = 10 seconds. Increase for longer demos.
   --  This is optional, this was used primarily for testing.
   --  Loop_Count : constant Positive := 300;

   --  ECS Entity storage: the central protected object that holds all
   --  entities and their components. All reads and writes go through here.
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --
   -- An Entity_Id is simply a unique string name tag that
   -- identifies a visual or logical element in the scene.
   -- Every widget, the render info, and any other ECS
   -- object needs its own Entity_Id.
   --
   -- Naming convention: prefix with E_ to distinguish
   -- entity IDs from component pointers (C_ prefix).
   --------------------------------------------------------
   E_RenderInfo   : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root         : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_Header       : constant IDs.Entity_Id := IDs.To_EID ("Header");
   E_Row          : constant IDs.Entity_Id := IDs.To_EID ("Row");
   E_Left         : constant IDs.Entity_Id := IDs.To_EID ("Left");
   E_Right        : constant IDs.Entity_Id := IDs.To_EID ("Right");

   --  Register entities with the ECS. Add_Entity returns a Components_Ptr
   --  which is used later to attach component data to the entity.
   --  Every entity must be registered before components can be added to it.
   C_RenderInfo   : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root         : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_Header       : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Header);
   C_Row          : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Row);
   C_Left         : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Left);
   C_Right        : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Right);

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   --
   -- Components are plain data records. They hold no logic
   -- of their own — they are pure data. The ECS systems
   -- (FlexLayoutSystem, TextRenderSystem, etc.) read these
   -- records each frame and perform all the actual work.
   --
   -- Every widget needs at minimum:
   --   • Widget_Component_T    — position, size, children
   --   • Position_Mode_Component_T — Flex or Absolute
   --
   -- Optional components add visual behaviour:
   --   • Background_Color_Component_T — fills the widget
   --   • Text_Component_T             — renders text
   --------------------------------------------------------

   --  RenderInfo: required by the rendering pipeline.
   --  Tells the renderer the terminal dimensions and
   --  provides two framebuffers for double buffering.
   --  Always create this with your actual terminal size.
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

   --  Root Widget: the invisible full-screen container.
   --  Position_X/Y should always be 1,1 for the root.
   --  Size_Width/Height should match your terminal size.
   --  Children lists which entities are direct flex children
   --  of this widget — the order here determines layout order.
   Comp_Root_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_Header, 1) & E_Row,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   --  Root_Widget_Component_T is a marker component that tells
   --  FlexLayoutSystem this is the top of the widget tree.
   --  Every demo must have exactly one entity with this component.
   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   --  Root Flex Layout: Column direction stacks children top to bottom.
   --  Item_Count must match the number of entries in Items.
   --  Header has Flex_Grow 0.0 so it stays at its Flex_Basis height.
   --  Row has Flex_Grow 1.0 so it expands to fill all remaining space.
   Comp_Root_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 2,
         Items      => new Flexbox.Flex_Item_Array'(
            --  Header: fixed at 3 rows tall, does not grow or shrink
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
            --  Row container: grows to fill all space below the header
            2 => (
               Related_Entity => E_Row,
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 21,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   --------------------------------------------------------
   -- HEADER WIDGET
   -- A full-width bar across the top of the screen.
   -- Uses Flex positioning so FlexLayoutSystem controls
   -- its position and size automatically each frame.
   --------------------------------------------------------
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

   --  Background_Color_Component_T fills the widget with a solid color.
   --  Available colors are defined in the Graphics package.
   Comp_Header_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   --  Text_Component_T renders a string inside the widget.
   --  Offset_X/Y are relative to the widget's own top-left corner.
   --  Use Is_Bold, Is_Italic, Is_Underline to style the text.
   Comp_Header_Text : constant Components.Text_Component_T := (
      Text             => To_Unbounded_String ("  Thuja Framework | Example Demo"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 2,
      Is_Bold          => True,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   --  Position_Mode_Component_T tells the layout system how to
   --  position this widget. Flex means FlexLayoutSystem controls
   --  it. Absolute means you control Position_X/Y manually.
   Comp_Header_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --------------------------------------------------------
   -- ROW CONTAINER WIDGET
   -- An invisible container that holds the left and right
   -- widgets side by side using a Row flex direction.
   -- It has no background color or text — its only job is
   -- to arrange its children horizontally.
   --------------------------------------------------------
   Comp_Row_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 4,
      Size_Width    => 80,
      Size_Height   => 21,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_Left, 1) & E_Right,
      Render_Buffer => (Width => 80, Height => 21, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Row_Marker : constant Components.Root_Widget_Component_T := (null record);

   --  Row flex layout: Row direction places children left to right.
   --  Both children have Flex_Grow 0.5 so they share the width equally.
   Comp_Row_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 21,
         Direction  => Flexbox.Row,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 2,
         Items      => new Flexbox.Flex_Item_Array'(
            --  Left widget: grows to fill left half of the row
            1 => (
               Related_Entity => E_Left,
               Flex_Basis     => 0,
               Flex_Grow      => 0.5,
               Flex_Shrink    => 0.0,
               Computed_Size  => 40,
               Cross_Size     => 21,
               Position_X     => 0,
               Position_Y     => 0
            ),
            --  Right widget: grows to fill right half of the row
            2 => (
               Related_Entity => E_Right,
               Flex_Basis     => 0,
               Flex_Grow      => 0.5,
               Flex_Shrink    => 0.0,
               Computed_Size  => 40,
               Cross_Size     => 21,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   Comp_Row_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --------------------------------------------------------
   -- LEFT WIDGET
   -- Green background, sits in the left half of the row.
   --------------------------------------------------------
   Comp_Left_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 4,
      Size_Width    => 40,
      Size_Height   => 21,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 40, Height => 21, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Left_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Green
   );

   Comp_Left_Text : constant Components.Text_Component_T := (
      Text             => To_Unbounded_String ("  Left Widget"),
      Text_Color       => Graphics.Black,
      Offset_X         => 1,
      Offset_Y         => 2,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_Left_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --------------------------------------------------------
   -- RIGHT WIDGET
   -- Red background, sits in the right half of the row.
   --------------------------------------------------------
   Comp_Right_Widget : constant Components.Widget_Component_T := (
      Position_X    => 41,
      Position_Y    => 4,
      Size_Width    => 40,
      Size_Height   => 21,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 40, Height => 21, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Right_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Red
   );

   Comp_Right_Text : constant Components.Text_Component_T := (
      Text             => To_Unbounded_String ("  Right Widget"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 2,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_Right_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   --------------------------------------------------------
   -- INPUT HANDLING STATE
   -- Uncomment the block below if your demo needs keyboard
   -- input. Should_Quit is used to exit the main loop early
   -- when the user presses ESC.
   --------------------------------------------------------
   -- Should_Quit : Boolean := False;

begin

   --------------------------------------------------------
   -- TERMINAL SETUP
   -- Enable_VT_Processing allows ANSI/CSI escape codes to
   -- work correctly on Windows via the Win32 API.
   -- Set_Cursor_Visible hides the terminal cursor during
   -- rendering to prevent visual flickering.
   -- Save_Cursor_Position and Restore_Cursor_Position
   -- return the cursor to where it was before the demo ran.
   --------------------------------------------------------
   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --
   -- Claim_Writing gives exclusive write access to the ECS.
   -- All Add_Component calls must happen inside a
   -- Claim_Writing / Release_Writing pair.
   --
   -- Add_Component takes three arguments:
   --   1. The entity's component bag (C_Name.all)
   --   2. A component ID string key e.g. "WidgetComponent"
   --   3. The component data record
   --
   -- The string key is how ECS systems look up the data
   -- they need each frame. Use the exact keys shown below
   -- as the systems depend on these specific strings.
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   --  RenderInfo entity: always register this first
   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"),              Comp_RenderInfo);

   --  Root entity: needs WidgetComponent, RootWidget marker and FlexLayoutComponent
   ECS.Add_Component (C_Root.all, IDs.To_CID ("WidgetComponent"),               Comp_Root_Widget);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("RootWidget"),                    Comp_Root_Marker);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("FlexLayoutComponent"),           Comp_Root_Flex);

   --  Header entity: widget + background + text + position mode
   ECS.Add_Component (C_Header.all, IDs.To_CID ("WidgetComponent"),             Comp_Header_Widget);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("BackgroundColorComponent"),    Comp_Header_BG);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("TextComponent"),               Comp_Header_Text);
   ECS.Add_Component (C_Header.all, IDs.To_CID ("PositionMode"),                Comp_Header_PositionMode);

   --  Row container entity: widget + RootWidget marker + flex layout + position mode
   --  Note: the RootWidget marker is used here too because the row container
   --  acts as a nested flex root for its own children.
   ECS.Add_Component (C_Row.all, IDs.To_CID ("WidgetComponent"),                Comp_Row_Widget);
   ECS.Add_Component (C_Row.all, IDs.To_CID ("RootWidget"),                     Comp_Row_Marker);
   ECS.Add_Component (C_Row.all, IDs.To_CID ("FlexLayoutComponent"),            Comp_Row_Flex);
   ECS.Add_Component (C_Row.all, IDs.To_CID ("PositionMode"),                   Comp_Row_PositionMode);

   --  Left widget entity: widget + background + text + position mode
   ECS.Add_Component (C_Left.all, IDs.To_CID ("WidgetComponent"),               Comp_Left_Widget);
   ECS.Add_Component (C_Left.all, IDs.To_CID ("BackgroundColorComponent"),      Comp_Left_BG);
   ECS.Add_Component (C_Left.all, IDs.To_CID ("TextComponent"),                 Comp_Left_Text);
   ECS.Add_Component (C_Left.all, IDs.To_CID ("PositionMode"),                  Comp_Left_PositionMode);

   --  Right widget entity: widget + background + text + position mode
   ECS.Add_Component (C_Right.all, IDs.To_CID ("WidgetComponent"),              Comp_Right_Widget);
   ECS.Add_Component (C_Right.all, IDs.To_CID ("BackgroundColorComponent"),     Comp_Right_BG);
   ECS.Add_Component (C_Right.all, IDs.To_CID ("TextComponent"),                Comp_Right_Text);
   ECS.Add_Component (C_Right.all, IDs.To_CID ("PositionMode"),                 Comp_Right_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- START INPUT READER
   -- Uncomment the line below if your demo uses keyboard
   -- input. The Input_Reader task runs in the background
   -- and populates Input_Buffer with keypresses each frame.
   --------------------------------------------------------
   --  Input_Handling.Input_Reader.Start;

   --------------------------------------------------------
   -- MAIN LOOP
   --
   -- Each iteration of this loop is one frame. The systems
   -- must always be called in the order shown below:
   --
   --   1. TerminalResizeSystem   — detect terminal resize
   --   2. FlexLayoutSystem       — compute widget positions
   --   3. WidgetBackgroundSystem — fill widget backgrounds
   --   4. TextRenderSystem       — render text into widgets
   --   5. BufferCopySystem       — composite into framebuffer
   --   6. BufferDrawSystem       — push framebuffer to terminal
   --   7. DoubleBufferFlagSystem — swap front and back buffers
   --
   -- If you need to update component data each frame (e.g.
   -- changing text or colors based on input), do it between
   -- step 2 and step 3 inside a Claim_Writing block.
   --------------------------------------------------------
   -- If you wish to utilize the Loop_Count above use this loop
   -- condition instead below instead of just the 'loop'
   -- for Loop_Index in 1 .. Loop_Count loop

   loop

      --------------------------------------------------------
      -- INPUT PROCESSING
      -- Uncomment the block below to handle keyboard input.
      -- Drain the full input queue each frame so fast typists
      -- do not fall behind the display.
      --------------------------------------------------------
      --  declare
      --     Event     : Input_Handling.Input_Event_t;
      --     Got_Input : Boolean := True;
      --  begin
      --     while Got_Input loop
      --        Input_Handling.Input_Buffer.Consume (Event);
      --        if Event.Char_Value = Character'Val (0) then
      --           Got_Input := False;
      --        else
      --           case Event.Cmd is
      --              when Input_Handling.Quit =>
      --                 Should_Quit := True;
      --              when others =>
      --                 null; --  Handle other keys here
      --           end case;
      --        end if;
      --     end loop;
      --  end;

      --  exit when Should_Quit;

      --------------------------------------------------------
      -- UPDATE COMPONENTS EACH FRAME (if needed)
      -- If your demo needs to change widget data each frame
      -- (e.g. update text content, change colors, move a
      -- widget), do it here inside a Claim_Writing block
      -- before the render systems run.
      --
      -- Example:
      --   Comp_Left_Text.Text := To_Unbounded_String ("Updated!");
      --   Entities_PO.Claim_Writing (Entities_Ptr);
      --   ECS.Add_Component (C_Left.all,
      --      IDs.To_CID ("TextComponent"), Comp_Left_Text);
      --   Entities_PO.Release_Writing;
      --------------------------------------------------------

      --  SYSTEM 1: Detect if the terminal has been resized
      --  and update RenderInfo dimensions accordingly
      ECS.TerminalResizeSystem (Entities_PO);

      --  SYSTEM 2: Compute final positions and sizes for all
      --  flex-positioned widgets based on their flex config
      ECS.FlexLayoutSystem (Entities_PO);

      --  SYSTEM 3: Fill each widget's render buffer with its
      --  background color component
      ECS.WidgetBackgroundSystem (Entities_PO);

      --  SYSTEM 4: Write each widget's text component into
      --  its render buffer at the specified offset
      ECS.TextRenderSystem (Entities_PO);

      --  SYSTEM 5: Composite all widget render buffers into
      --  the main framebuffer in the correct draw order
      ECS.BufferCopySystem (Entities_PO);

      --  SYSTEM 6: Compare the framebuffer against the back
      --  buffer and push only changed pixels to the terminal
      ECS.BufferDrawSystem (Entities_PO);

      --  SYSTEM 7: Swap the front and back buffers ready
      --  for the next frame
      ECS.DoubleBufferFlagSystem (Entities_PO);

      --  Target approximately 30 FPS
      delay Duration (0.033);
   end loop;

   --------------------------------------------------------
   -- SHUTDOWN
   -- Stop the input reader if it was started, restore the
   -- cursor and cursor position, and flush the output.
   --------------------------------------------------------
   --  Input_Handling.Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Example_Demo;