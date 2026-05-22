------------------------------------------------------------------------------
--  Resize_And_Movement_Demo.adb
--
--  PURPOSE
--  -------
--  Simple demonstration of:
--    • Terminal resize detection (TerminalResizeSystem)
--    • Widget movement API (Move_Widget, Move_Widget_By)
--
--  WHAT YOU'LL SEE
--  ---------------
--  A single red widget bouncing around the screen.
--  When you resize the terminal, the layout automatically adjusts.
--
--  INSTRUCTIONS
--  ------------
--  1. Run the demo
--  2. Try resizing your terminal window
--  3. Watch the red box bounce and adapt to the new size
------------------------------------------------------------------------------

with Ada.Text_IO;
with Components;
with ECS;
with Graphics;
with Ada.Strings.Unbounded;
use Graphics;
with IDs;
procedure Resize_And_Movement_Demo is

   Loop_Count : constant Positive := 200;

   Entities_PO : ECS.Entity_Components_PO;

   --  Entities
   E_RenderInfo : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root       : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_Bouncer    : constant IDs.Entity_Id := IDs.To_EID ("Bouncer");

   C_RenderInfo : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root       : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_Bouncer    : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Bouncer);

   --  Components
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
      Children      => IDs.Entity_ID_Vector.To_Vector (E_Bouncer, 1),
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   Comp_Bouncer_Widget : constant Components.Widget_Component_T := (
      Position_X    => 10,
      Position_Y    => 6,
      Size_Width    => 3,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 3, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Bouncer_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Red
   );

   Comp_Bouncer_Text : constant Components.Text_Component_T := (
      Text             => Ada.Strings.Unbounded.To_Unbounded_String ("B"),
      Text_Color       => Graphics.White,
      Offset_X         => 2,
      Offset_Y         => 1,
      Is_Bold          => True,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   --  Use ABSOLUTE positioning so it's not controlled by Flexbox
   Comp_Bouncer_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Absolute
   );

   --  Movement state
   X  : Graphics.TUI_Width := 10;
   Y  : Graphics.TUI_Height := 6;
   DX : Integer := 1;
   DY : Integer := 1;

   --  Render thread
   task Render_Thread;

   task body Render_Thread is
      Entity_List : ECS.Entity_Components_Ptr;
   begin
      Graphics.Clear_Screen;
      loop
         Entities_PO.Claim_Reading (Entity_List);
         ECS.BufferDrawSystem (Entities_PO);
         Entities_PO.Release_Reading;
         delay Duration (1.0 / 30.0);
      end loop;
   end Render_Thread;

begin

   --  Register components
   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"), Comp_RenderInfo);
   
   ECS.Add_Component (C_Root.all, IDs.To_CID ("WidgetComponent"), Comp_Root_Widget);
   ECS.Add_Component (C_Root.all, IDs.To_CID ("RootWidget"), Comp_Root_Marker);
   
   ECS.Add_Component (C_Bouncer.all, IDs.To_CID ("WidgetComponent"), Comp_Bouncer_Widget);
   ECS.Add_Component (C_Bouncer.all, IDs.To_CID ("BackgroundColorComponent"), Comp_Bouncer_BG);
   ECS.Add_Component (C_Bouncer.all, IDs.To_CID ("TextComponent"), Comp_Bouncer_Text);
   ECS.Add_Component (C_Bouncer.all, IDs.To_CID ("PositionMode"), Comp_Bouncer_PositionMode);

   Graphics.Clear_Screen;
   Ada.Text_IO.Put_Line ("Resize & Movement Demo Started!");
   Ada.Text_IO.Put_Line ("Try resizing your terminal window...");
   delay 2.0;
   Graphics.Clear_Screen;

   --  Main loop
   for Loop_Index in 1 .. Loop_Count loop
      declare
         Entity_List : ECS.Entity_Components_Ptr;
      begin
         --  FEATURE 1: Detect terminal resize
         ECS.TerminalResizeSystem (Entities_PO);

         Entities_PO.Claim_Writing (Entity_List);

         -- If the terminal is smaller than the bouncer, shrink the widget and its render buffer to fit
         declare
            RI_Comps : constant ECS.Components_Ptr := ECS.Get_Entity_Components (Entity_List.all, E_RenderInfo);
            RenderInfo_C : Components.Render_Info_Component_T renames Components.Render_Info_Component_T (
               ECS.Get_Component_Ptr (RI_Comps, "RenderInfo").all);
            B_Comps : constant ECS.Components_Ptr := ECS.Get_Entity_Components (Entity_List.all, E_Bouncer);
            B_Widget : Components.Widget_Component_T;
            NewW_Int : Integer;
            NewH_Int : Integer;
         begin
            if ECS.Has_Component (B_Comps.all, IDs.To_CID ("WidgetComponent")) then
               B_Widget := Components.Widget_Component_T (
                  ECS.Get_Component (B_Comps.all, IDs.To_CID ("WidgetComponent"))
               );
               -- compute new size limited by terminal
               NewW_Int := Integer (B_Widget.Size_Width);
               NewH_Int := Integer (B_Widget.Size_Height);
               if NewW_Int > Integer (RenderInfo_C.Terminal_Width) then
                  NewW_Int := Integer (RenderInfo_C.Terminal_Width);
               end if;
               if NewH_Int > Integer (RenderInfo_C.Terminal_Height) then
                  NewH_Int := Integer (RenderInfo_C.Terminal_Height);
               end if;
               if NewW_Int < 1 then
                  NewW_Int := 1;
               end if;
               if NewH_Int < 1 then
                  NewH_Int := 1;
               end if;
               if NewW_Int /= Integer (B_Widget.Size_Width) or else NewH_Int /= Integer (B_Widget.Size_Height) then
                  B_Widget.Size_Width := Graphics.TUI_Width (NewW_Int);
                  B_Widget.Size_Height := Graphics.TUI_Height (NewH_Int);
                  B_Widget.Render_Buffer := Create_Buffer (B_Widget.Size_Width, B_Widget.Size_Height);
                  ECS.Add_Component (B_Comps.all, IDs.To_CID ("WidgetComponent"), B_Widget);
               end if;
            end if;
         end;

         -- Compute movement constrained to current terminal size so widget cannot escape
         declare
            RI_Comps : constant ECS.Components_Ptr := ECS.Get_Entity_Components (Entity_List.all, E_RenderInfo);
            RenderInfo_C : Components.Render_Info_Component_T renames Components.Render_Info_Component_T (
               ECS.Get_Component_Ptr (RI_Comps, "RenderInfo").all);
            W : constant Integer := Integer (RenderInfo_C.Terminal_Width);
            H : constant Integer := Integer (RenderInfo_C.Terminal_Height);
            B_Comps2 : constant ECS.Components_Ptr := ECS.Get_Entity_Components (Entity_List.all, E_Bouncer);
            BW : Integer;
            BH : Integer;
            Max_X_Int : Integer;
            Max_Y_Int : Integer;
            NextX : Integer;
            NextY : Integer;
         begin
            -- Determine bouncer's current size from ECS (it may have been resized at runtime)
            if ECS.Has_Component (B_Comps2.all, IDs.To_CID ("WidgetComponent")) then
               declare
                  Live_B_Widget : constant Components.Widget_Component_T := Components.Widget_Component_T (
                     ECS.Get_Component (B_Comps2.all, IDs.To_CID ("WidgetComponent"))
                  );
               begin
                  BW := Integer (Live_B_Widget.Size_Width);
                  BH := Integer (Live_B_Widget.Size_Height);
               end;
            else
               BW := Integer (Comp_Bouncer_Widget.Size_Width);
               BH := Integer (Comp_Bouncer_Widget.Size_Height);
            end if;

            -- compute max positions based on current widget size
            Max_X_Int := W - BW + 1;
            Max_Y_Int := H - BH + 1;

            -- Prepare next positions
            NextX := Integer (X) + DX;
            NextY := Integer (Y) + DY;

            -- Ensure min bounds
            if Max_X_Int < 1 then
               Max_X_Int := 1;
            end if;
            if Max_Y_Int < 1 then
               Max_Y_Int := 1;
            end if;

            -- Clamp and flip directions without overshoot
            if NextX < 1 then
               NextX := 1;
               DX := 1;
            elsif NextX > Max_X_Int then
               NextX := Max_X_Int;
               DX := -1;
            end if;

            if NextY < 1 then
               NextY := 1;
               DY := 1;
            elsif NextY > Max_Y_Int then
               NextY := Max_Y_Int;
               DY := -1;
            end if;

            -- Apply clamped positions
            X := Graphics.TUI_Width (NextX);
            Y := Graphics.TUI_Height (NextY);
         end;

         --  Apply movement
         ECS.Move_Widget (Entity_List.all, E_Bouncer, X, Y);
         Entities_PO.Release_Writing;

         --  Render
         ECS.WidgetBackgroundSystem (Entities_PO);
         ECS.TextRenderSystem (Entities_PO);
         ECS.BufferCopySystem (Entities_PO);
         ECS.DoubleBufferFlagSystem (Entities_PO);

         delay Duration (0.05);
      end;
   end loop;

   Graphics.Clear_Screen;
   Ada.Text_IO.Put_Line ("Demo Complete!");
   Ada.Text_IO.Put_Line ("Features shown:");
   Ada.Text_IO.Put_Line ("  - Terminal resize detection");
   Ada.Text_IO.Put_Line ("  - Widget movement (Move_Widget)");
   Ada.Text_IO.Put_Line ("  - Absolute positioning mode");

end Resize_And_Movement_Demo;
