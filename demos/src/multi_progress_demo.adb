--  Presentation 2 Demo
--  Demonstrates multiple widgets
--
--  This demo shows:
--  - Multiple progress bars updating at different rates
--  - Different color schemes
--  - Various sizes
--  - A box with animated text

with Ada.Text_IO;
with Graphics; use Graphics;
with ECS; use ECS;
with IDs; use IDs;
with User_Library; use User_Library;

procedure Multi_Progress_Demo is

   --  Entity storage
   Entities : Entity_Components;

   --  Demo timing
   Frame_Duration : constant Duration := 0.05;  --  50ms per frame (20 FPS)
   Total_Frames   : constant Positive := 200;   --  10 seconds total

   --  Entity IDs
   Render_Info_ID : constant Entity_Id := To_EID ("RenderInfo");
   Root_ID        : constant Entity_Id := To_EID ("RootWidget");
   Progress1_ID   : constant Entity_Id := To_EID ("Download");
   Progress2_ID   : constant Entity_Id := To_EID ("Install");
   Progress3_ID   : constant Entity_Id := To_EID ("Configure");
   Animation_ID   : constant Entity_Id := To_EID ("Animation");

   --  Terminal dimensions
   Term_Width  : constant TUI_Width := 70;
   Term_Height : constant TUI_Height := 15;

   --  Progress state for staggered animation
   P1_Value : Float := 0.0;
   P2_Value : Float := 0.0;
   P3_Value : Float := 0.0;

begin
   Hide_Cursor;
   Clear_Screen;

   --  Setup infrastructure
   Setup_Render_Info (Entities, Render_Info_ID, Term_Width, Term_Height);
   Setup_Root_Widget (Entities, Root_ID, Term_Width, Term_Height,
                      [Progress1_ID, Progress2_ID, Progress3_ID, Animation_ID]);

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

   --  Animated text
   Setup_Animation (Entities, Animation_ID);

   --  Initial render
   Run_Systems (Entities);

   --  Main loop, simulating non-system data hooking
   for Frame in 1 .. Total_Frames loop
      --  Update progress values (staggered starts)
      --  Download bar: Full speed from start
      if P1_Value < 1.0 then
         P1_Value := P1_Value + 0.015;
         if P1_Value > 1.0 then
            P1_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress1_ID, P1_Value);
      end if;

      --  Installation bar: Starts when download is at 30%
      if P1_Value >= 0.3 and P2_Value < 1.0 then
         P2_Value := P2_Value + 0.012;
         if P2_Value > 1.0 then
            P2_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress2_ID, P2_Value);
      end if;

      --  Configuration bar: Starts when install is at 50%
      if P2_Value >= 0.5 and P3_Value < 1.0 then
         P3_Value := P3_Value + 0.010;
         if P3_Value > 1.0 then
            P3_Value := 1.0;
         end if;
         Set_Progress (Entities, Progress3_ID, P3_Value);
      end if;

      --  Run systems to update & render
      Run_Systems (Entities);

      --  Check if all complete
      exit when P1_Value >= 1.0 and P2_Value >= 1.0 and P3_Value >= 1.0;

      delay Frame_Duration;
   end loop;

   --  Reset terminal to normal state (show cursor, reset colors)
   Reset_Styling;
   Show_Cursor;

   --  Completion message
   Ada.Text_IO.New_Line (3);
   Ada.Text_IO.Put_Line ("Demo complete!");

end Multi_Progress_Demo;
