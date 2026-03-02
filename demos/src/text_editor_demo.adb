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
use Graphics;
with IDs;
use type IDs.Entity_ID_Vector.Vector;
with Flexbox;

procedure Text_Editor_Demo is

   Loop_Count : constant Positive := 300; -- 10 seconds at 30 FPS

   --  ECS Entity storage
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   -- Every visual element needs an Entity_Id (a unique name tag)
   --------------------------------------------------------
   E_RenderInfo : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root       : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_Editor     : constant IDs.Entity_Id := IDs.To_EID ("Editor");

   --  Register entities with the ECS - this gives us a pointer
   --  to each entity's component bag
   C_RenderInfo : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root       : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_Editor     : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Editor);

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   -- Components are plain data records attached to entities.
   -- The systems (FlexLayoutSystem, TextRenderSystem, etc.)
   -- read these records each frame and do the actual work.
   --------------------------------------------------------

   --  RenderInfo: tells the renderer how big the terminal is
   --  and gives it two framebuffers to work with (double buffering)
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
   --  Its Children list tells FlexLayoutSystem which widgets to arrange.
   --  We list E_Editor as its only child.
   Comp_Root_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_Editor, 1),
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   --  Root Flex Layout: arranges children in a Column (top to bottom).
   --  We have one child (E_Editor) that grows to fill all available space.
   Comp_Root_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 1,
         Items      => new Flexbox.Flex_Item_Array'(
            --  Editor: Flex_Grow => 1.0 means "take all remaining space"
            1 => (
               Related_Entity => E_Editor,
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 24,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   --  Editor Widget: this is the blue box you see on screen.
   --  Position/Size here are initial values; FlexLayoutSystem will
   --  overwrite them each frame based on the Root flex config above.
   Comp_Editor_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => True,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   --  Blue background for the editor
   Comp_Editor_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   --  Welcome text rendered at the top-left of the editor widget.
   --  Offset_X/Y are relative to the widget's own top-left corner.
   --  Offset_Y => 1 puts us on the first row of the widget.
   Comp_Editor_Text : constant Components.Text_Component_T := (
      Text             => Ada.Strings.Unbounded.To_Unbounded_String ("Welcome to Thuja text editor"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   --  Position mode: Flex means FlexLayoutSystem controls where this widget sits.
   --  Use Absolute if you want to place it manually with Position_X/Y instead.
   Comp_Editor_PositionMode : constant Components.Position_Mode_Component_T := (
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
   -- Add_Component attaches a data record to an entity.
   -- The string key (e.g. "WidgetComponent") is how systems
   -- look up the data they need each frame.
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"),       Comp_RenderInfo);

   ECS.Add_Component (C_Root.all, IDs.To_CID ("WidgetComponent"),        Comp_Root_Widget);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("RootWidget"),             Comp_Root_Marker);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("FlexLayoutComponent"),    Comp_Root_Flex);

   ECS.Add_Component (C_Editor.all, IDs.To_CID ("WidgetComponent"),      Comp_Editor_Widget);
   ECS.Add_Component (C_Editor.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Editor_BG);
   ECS.Add_Component (C_Editor.all, IDs.To_CID ("TextComponent"),        Comp_Editor_Text);
   ECS.Add_Component (C_Editor.all, IDs.To_CID ("PositionMode"),         Comp_Editor_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- MAIN LOOP
   -- Each frame: detect resize → layout → render → present
   --------------------------------------------------------
   for Loop_Index in 1 .. Loop_Count loop

      --  Recalculate layout if terminal was resized
      ECS.TerminalResizeSystem (Entities_PO);

      --  Position all Flex children based on their parent's Flex config
      ECS.FlexLayoutSystem (Entities_PO);

      --  Paint each widget's Render_Buffer with its background color
      ECS.WidgetBackgroundSystem (Entities_PO);

      --  Write text into each widget's Render_Buffer
      ECS.TextRenderSystem (Entities_PO);

      --  Composite all Render_Buffers into the main framebuffer
      ECS.BufferCopySystem (Entities_PO);

      --  Swap front/back buffers and push to terminal
      ECS.BufferDrawSystem (Entities_PO);
      ECS.DoubleBufferFlagSystem (Entities_PO);

      delay Duration (0.033); -- ~30 FPS
   end loop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Text_Editor_Demo;