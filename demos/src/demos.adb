------------------------------------------------------------------------------
--  Demos.adb
--
--  PURPOSE
--  -------
--  This demo validates the Flexbox-based layout system for the Thuja
--  terminal UI framework using a real ECS-driven rendering pipeline.
--
--  WHAT THIS DEMO DOES
--  -------------------
--  • Sets up an Entity-Component-System (ECS) with four entities:
--      1) Render Info entity (terminal dimensions & buffers)
--      2) Root Widget entity (Flexbox container)
--      3) Green Header widget
--      4) Red Content widget
--
--  • Attaches a Flex_Layout_Component to the Root Widget, defining a
--    Flexbox container with:
--      - Direction  => Column  (vertical stacking)
--      - Align      => Stretch (full-width children)
--      - Justify    => Flex_Start
--
--  • Demonstrates Flexbox behavior in a terminal environment:
--      - The Green widget acts as a fixed-height header (Flex_Basis = 5)
--      - The Red widget grows to fill remaining vertical space
--      - Layout is recalculated dynamically every frame
--
--  • Runs the FlexLayoutSystem BEFORE all rendering systems to ensure
--    widget positions and sizes are computed prior to drawing.
--
--  EXPECTED VISUAL RESULT
--  ----------------------
--  ┌──────────────────────────────────────────────────────────────┐
--  │ GREEN HEADER (5 rows tall, full width)                         │
--  ├──────────────────────────────────────────────────────────────┤
--  │                                                              │
--  │ RED CONTENT AREA (fills remaining height)                     │
--  │                                                              │
--  └──────────────────────────────────────────────────────────────┘
--
--  This file serves as a proof-of-work that CSS-style Flexbox concepts
--  (direction, basis, grow, align, justify) can be successfully
--  applied to a terminal UI using Ada.
------------------------------------------------------------------------------


with Ada.Text_IO;
with Components;
with ECS;
with Graphics; use Graphics;
with IDs;
use type IDs.Entity_ID_Vector.Vector;
-- FLEXBOX INTEGRATION: Import Flexbox to configure the layout
with Flexbox;

procedure Demos is

   --  Number of times main loop should iterate
   Loop_Count : constant Positive := 100;

   --  Variables for Thuja
   Entities_PO : ECS.Entity_Components_PO;

   --------------------------------------------------------
   -- 1. DEFINE ENTITIES
   --------------------------------------------------------
   E1_ID : constant IDs.Entity_Id := IDs.To_EID ("Render info");
   E2_ID : constant IDs.Entity_Id := IDs.To_EID ("Root widget");
   E3_ID : constant IDs.Entity_Id := IDs.To_EID ("Green Sidebar");
   E4_ID : constant IDs.Entity_Id := IDs.To_EID ("Red Content");

   --  Register entities
   E1_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E1_ID);
   E2_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E2_ID);
   E3_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E3_ID);
   E4_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E4_ID);

   --------------------------------------------------------
   -- 2. DEFINE COMPONENTS
   --------------------------------------------------------

   -- E1: Render Info (Standard)
   E1_RIC : constant Components.Render_Info_Component_T := (
      Terminal_Width       => 80,
      Terminal_Height      => 24,
      Prev_Terminal_Width  => 80,
      Prev_Terminal_Height => 24,
      Framebuffer_1 => (Width => 80, Height => 24, Data => new Pixel_Array),
      Framebuffer_2 => (Width => 80, Height => 24, Data => new Pixel_Array),
      Drawing_FB => new Graphics.Protected_DB,
      Backbuffer => (Width => 80, Height => 24, Data => new Pixel_Array)
   );

   -- E2: Root Widget (The Container)
   -- FIX: Added Is_Visible and Is_Enabled
   E2_WC : constant Components.Widget_Component_T := (
      Position_X => 1, Position_Y => 1,
      Size_Width => 80, Size_Height => 24,
      Children => IDs.Entity_ID_Vector.To_Vector (E3_ID, 1) & E4_ID, -- Render both children
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus => False,
      Is_Visible => True,    -- Required Field
      Is_Enabled => True     -- Required Field
   );

   -- We can use others => <> here if the type has defaults for everything else
   E2_RWC : constant Components.Root_Widget_Component_T := (others => <>);

   -- FLEXBOX INTEGRATION: Define the Layout Logic
   -- PROOF OF WORK:
   -- 1. Changed Direction to 'Column' (Vertical stacking)
   -- 2. Changed Flex_Basis of Item 1 to '5'. In Column mode, this now means HEIGHT.
   -- 3. Green box should now be a header bar across the top.
   E2_FC : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,        -- CHANGED: Stack items vertically
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,       -- CHANGED: Stretch items horizontally (width)
         Item_Count => 2,
         Items      => new Flexbox.Flex_Item_Array'(
            -- ITEM 1 (The Green Header)
            1 => (
               Related_Entity => E3_ID,
               Flex_Basis     => 5,     -- CHANGED: Fixed HEIGHT of 5 rows
               Flex_Grow      => 0.0,   -- Do not grow
               Flex_Shrink    => 0.0,
               Computed_Size  => 5,     -- Safe default
               Cross_Size     => 80,    -- Safe default (Will be overwritten by Stretch)
               Position_X     => 0,
               Position_Y     => 0
            ),
            -- ITEM 2 (The Red Content Body)
            2 => (
               Related_Entity => E4_ID,
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,   -- Grow to fill remaining HEIGHT (24 - 5 = 19)
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 1,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   -- E3: Green Box (The Sidebar)
   -- FIX: Added Is_Visible and Is_Enabled
   E3_WC : constant Components.Widget_Component_T := (
      Position_X => 1, Position_Y => 1,
      Size_Width => 1, Size_Height => 1,
      Has_Focus => True,
      Render_Buffer => (Width => 20, Height => 24, Data => new Pixel_Array),
      Children => IDs.Entity_ID_Vector.Empty_Vector,
      Is_Visible => True,    -- Required Field
      Is_Enabled => True     -- Required Field
   );
   E3_BCC : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Green
   );

   -- E4: Red Box (The Content Area)
   -- FIX: Added Is_Visible and Is_Enabled
   E4_WC : constant Components.Widget_Component_T := (
      Position_X => 1, Position_Y => 1,
      Size_Width => 1, Size_Height => 1,
      Has_Focus => False,
      Render_Buffer => (Width => 60, Height => 24, Data => new Pixel_Array),
      Children => IDs.Entity_ID_Vector.Empty_Vector,
      Is_Visible => True,    -- Required Field
      Is_Enabled => True     -- Required Field
   );
   E4_BCC : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Red
   );


   --  Render thread declaration
   --  The main body doesn't end, so it's fine that this doesn't either
   task Render_Thread;

   task body Render_Thread is
   begin
      Graphics.Clear_Screen;
      loop
         ECS.BufferDrawSystem (Entities_PO);

         --  30 FPS
         delay Duration (1.0 / 30.0);
      end loop;
   end Render_Thread;
begin

   ECS.Add_Component (E1_C.all, IDs.To_CID ("RenderInfo"), E1_RIC);

   -- Root Widget Components
   ECS.Add_Component (E2_C.all, IDs.To_CID ("WidgetComponent"), E2_WC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("RootWidget"), E2_RWC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("FlexLayoutComponent"), E2_FC);

   -- Child 1 (Green Sidebar)
   ECS.Add_Component (E3_C.all, IDs.To_CID ("WidgetComponent"), E3_WC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("BackgroundColorComponent"), E3_BCC);

   -- Child 2 (Red Content)
   ECS.Add_Component (E4_C.all, IDs.To_CID ("WidgetComponent"), E4_WC);
   ECS.Add_Component (E4_C.all, IDs.To_CID ("BackgroundColorComponent"), E4_BCC);

   Graphics.Clear_Screen;

   --------------------------------------------------------
   -- MAIN LOOP
   --------------------------------------------------------
   for Loop_Index in Positive'First .. Loop_Count loop
      ECS.FlexLayoutSystem (Entities_PO);
      ECS.WidgetBackgroundSystem (Entities_PO);
      ECS.TextRenderSystem (Entities_PO);
      ECS.BufferCopySystem (Entities_PO);
      ECS.DoubleBufferFlagSystem (Entities_PO);

      delay Duration (0.1);
   end loop;

   Graphics.Clear_Screen;
   Ada.Text_IO.Put_Line ("Thank you for using the Thuja demo");
end Demos;
