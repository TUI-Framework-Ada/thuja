--  Progress Bar Demo
--  Demonstrates the Thuja Progress Bar widget using the ECS framework
--
--  This demo creates a progress bar that fills over 5 seconds,
--  showcasing the ECS-based rendering pipeline.

with Ada.Text_IO;
with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;
with Thuja; use Thuja;

procedure Progress_Bar_Demo is

   --  Entity storage for the demo
   Entities : Entity_Components;

   --  Demo configuration
   Total_Steps    : constant Positive := 100;
   Total_Duration : constant Duration := 5.0;  --  5 seconds to fill
   Step_Duration  : constant Duration := Total_Duration / Duration (Total_Steps);

   --  Entity IDs
   Render_Info_ID : constant Entity_Id := To_EID ("RenderInfo");
   Root_ID        : constant Entity_Id := To_EID ("RootWidget");
   Progress_ID    : constant Entity_Id := To_EID ("ProgressBar1");

   --  Terminal dimensions for the demo
   Term_Width  : constant TUI_Width := 60;
   Term_Height : constant TUI_Height := 10;

   --  Helper procedure to setup the render infrastructure
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

   --  Helper procedure to setup the root widget
   procedure Setup_Root_Widget is
      Comp_Ptr : Components_Ptr;
      Widget_C : Widget_Component_T;
      Root_C   : Root_Widget_Component_T;
   begin
      Comp_Ptr := Add_Entity (Entities, Root_ID);

      --  Configure root widget to span the terminal
      Widget_C.Position_X := TUI_Width'First;
      Widget_C.Position_Y := TUI_Height'First;
      Widget_C.Size_Width := Term_Width;
      Widget_C.Size_Height := Term_Height;
      Widget_C.Is_Visible := True;
      Widget_C.Is_Enabled := True;
      Widget_C.Render_Buffer := Create_Buffer (Term_Width, Term_Height);

      --  Add progress bar as child of root
      Widget_C.Children.Append (Progress_ID);

      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
                     Background_Color_Component_T'(Background_Color => Navy));
   end Setup_Root_Widget;

   --  Run all render systems in correct order
   procedure Run_Render_Systems is
   begin
      --  First, render widget-specific content
      WidgetBackgroundSystem (Entities);      --  Fill backgrounds
      ProgressBarRenderSystem (Entities);     --  Render progress bars

      --  Then copy buffers and draw to terminal
      BufferCopySystem (Entities);            --  Copy widget buffers to framebuffer
      BufferDrawSystem (Entities);            --  Draw framebuffer to terminal
   end Run_Render_Systems;

begin
   --  Clear terminal and print header
   Clear_Screen;
   Ada.Text_IO.Put_Line ("=== Thuja Progress Bar Demo ===");
   Ada.Text_IO.Put_Line ("Progress bar will fill over 5 seconds...");
   Ada.Text_IO.New_Line;

   --  Setup ECS entities
   Setup_Render_Info;
   Setup_Root_Widget;

   --  Create the progress bar widget using Thuja
   --  Position it centered in the display area
   Create_Progress_Bar
     (Entity_List  => Entities,
      E_ID         => Progress_ID,
      Pos_X        => 5,
      Pos_Y        => 3,
      Width        => 50,
      Height       => 1,
      Filled_Color => Green,
      Empty_Color  => Gray,
      BG_Color     => Black);

   --  Initial render
   Run_Render_Systems;

   --  Animation loop
   for Step in 0 .. Total_Steps loop
      --  Update progress value
      Set_Progress (Entities, Progress_ID, Float (Step) / Float (Total_Steps));

      --  Re-render
      Run_Render_Systems;

      --  Wait for next frame
      delay Step_Duration;
   end loop;

   --  Move cursor below the demo area and print completion message
   Ada.Text_IO.New_Line (Ada.Text_IO.Positive_Count (Integer (Term_Height) + 2));
   Ada.Text_IO.Put_Line ("Demo complete!");

   --  Reset terminal to normal state (show cursor, reset colors)
   Reset_Terminal;

end Progress_Bar_Demo;