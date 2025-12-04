--  Multi Progress Bar Demo
--  Demonstrates multiple Thuja Progress Bar widgets with different styles
--
--  This demo shows:
--  - Multiple progress bars updating at different rates
--  - Different color schemes
--  - Various sizes

with Ada.Text_IO;
with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;
with Thuja; use Thuja;

procedure Multi_Progress_Demo is

   --  Entity storage
   Entities : Entity_Components;

   --  Demo timing
   Frame_Duration : constant Duration := 0.05;  --  50ms per frame
   Total_Frames   : constant Positive := 200;   --  10 seconds total

   --  Entity IDs
   Render_Info_ID : constant Entity_Id := To_EID ("RenderInfo");
   Root_ID        : constant Entity_Id := To_EID ("RootWidget");
   Progress1_ID   : constant Entity_Id := To_EID ("Download");
   Progress2_ID   : constant Entity_Id := To_EID ("Install");
   Progress3_ID   : constant Entity_Id := To_EID ("Configure");

   --  Terminal dimensions
   Term_Width  : constant TUI_Width := 70;
   Term_Height : constant TUI_Height := 15;

   procedure Setup_Render_Info is
      Comp_Ptr : Components_Ptr;
      RI_C     : Render_Info_Component_T;
   begin
      Comp_Ptr := Add_Entity (Entities, Render_Info_ID);
      RI_C.Terminal_Width := Term_Width;
      RI_C.Terminal_Height := Term_Height;
      RI_C.Framebuffer := Create_Buffer (Term_Width, Term_Height);
      RI_C.Backbuffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (Comp_Ptr.all, To_CID ("RenderInfo"), RI_C);
   end Setup_Render_Info;

   procedure Setup_Root_Widget is
      Comp_Ptr : Components_Ptr;
      Widget_C : Widget_Component_T;
      Root_C   : Root_Widget_Component_T;
   begin
      Comp_Ptr := Add_Entity (Entities, Root_ID);
      Widget_C.Position_X := TUI_Width'First;
      Widget_C.Position_Y := TUI_Height'First;
      Widget_C.Size_Width := Term_Width;
      Widget_C.Size_Height := Term_Height;
      Widget_C.Is_Visible := True;
      Widget_C.Is_Enabled := True;
      Widget_C.Render_Buffer := Create_Buffer (Term_Width, Term_Height);

      --  Add all progress bars as children
      Widget_C.Children.Append (Progress1_ID);
      Widget_C.Children.Append (Progress2_ID);
      Widget_C.Children.Append (Progress3_ID);

      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
                     Background_Color_Component_T'(Background_Color => Black));
   end Setup_Root_Widget;

   procedure Run_Render_Systems is
   begin
      WidgetBackgroundSystem (Entities);
      ProgressBarRenderSystem (Entities);
      BufferCopySystem (Entities);
      BufferDrawSystem (Entities);
   end Run_Render_Systems;

   --  Progress state for staggered animation
   P1_Value : Float := 0.0;
   P2_Value : Float := 0.0;
   P3_Value : Float := 0.0;

begin
   Clear_Screen;
   Ada.Text_IO.Put_Line ("=== Multi Progress Bar Demo ===");
   Ada.Text_IO.Put_Line ("Simulating a multi-stage installation...");
   Ada.Text_IO.New_Line;

   --  Setup infrastructure
   Setup_Render_Info;
   Setup_Root_Widget;

   --  Create progress bars with different styles
   --  Progress Bar 1: Download (Cyan/Blue theme)
   Create_Progress_Bar
     (Entity_List  => Entities,
      E_ID         => Progress1_ID,
      Pos_X        => 3,
      Pos_Y        => 3,
      Width        => 55,
      Height       => 1,
      Filled_Color => Cyan,
      Empty_Color  => Steel_Blue,
      BG_Color     => Navy);

   --  Progress Bar 2: Install (Green/Yellow theme)
   Create_Progress_Bar
     (Entity_List  => Entities,
      E_ID         => Progress2_ID,
      Pos_X        => 3,
      Pos_Y        => 6,
      Width        => 55,
      Height       => 1,
      Filled_Color => Lime,
      Empty_Color  => Forest_Green,
      BG_Color     => Navy);

   --  Progress Bar 3: Configure (Orange/Red theme)
   Create_Progress_Bar
     (Entity_List  => Entities,
      E_ID         => Progress3_ID,
      Pos_X        => 3,
      Pos_Y        => 9,
      Width        => 55,
      Height       => 1,
      Filled_Color => Orange,
      Empty_Color  => Maroon,
      BG_Color     => Navy);

   --  Initial render
   Run_Render_Systems;

   --  Animation loop with staggered progress
   for Frame in 1 .. Total_Frames loop
      --  Update progress values (staggered starts)
      --  Download: Full speed from start
      if P1_Value < 1.0 then
         P1_Value := P1_Value + 0.015;
         if P1_Value > 1.0 then
            P1_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress1_ID, P1_Value);
      end if;

      --  Install: Starts when download is at 30%
      if P1_Value >= 0.3 and P2_Value < 1.0 then
         P2_Value := P2_Value + 0.012;
         if P2_Value > 1.0 then
            P2_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress2_ID, P2_Value);
      end if;

      --  Configure: Starts when install is at 50%
      if P2_Value >= 0.5 and P3_Value < 1.0 then
         P3_Value := P3_Value + 0.010;
         if P3_Value > 1.0 then
            P3_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress3_ID, P3_Value);
      end if;

      --  Render frame
      Run_Render_Systems;

      --  Check if all complete
      exit when P1_Value >= 1.0 and P2_Value >= 1.0 and P3_Value >= 1.0;

      delay Frame_Duration;
   end loop;

   --  Final render to ensure 100% is shown
   Set_Progress (Entities, Progress1_ID, 1.0);
   Set_Progress (Entities, Progress2_ID, 1.0);
   Set_Progress (Entities, Progress3_ID, 1.0);
   Run_Render_Systems;

   --  Completion message
   Ada.Text_IO.New_Line (Ada.Text_IO.Positive_Count (Integer (Term_Height) + 2));
   Ada.Text_IO.Put_Line ("All operations complete!");

   --  Reset terminal to normal state (show cursor, reset colors)
   Reset_Terminal;

end Multi_Progress_Demo;