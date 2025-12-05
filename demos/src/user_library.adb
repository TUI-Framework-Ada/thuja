with Ada.Calendar;
with Ada.Strings.Unbounded;

package body User_Library is

   procedure RainbowTextSystem (Entity_List : ECS.Entity_Components) is
      Search_Component_IDs : IDs.Component_ID_Vector.Vector;
      Matched_Entities : IDs.Entity_ID_Vector.Vector;
      Component_List : ECS.Components_Ptr;
      Text_C : Components.Text_Component_T;
      RainbowText_C : RainbowTextComponent;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Unused1 : Ada.Calendar.Year_Number;
      Unused2 : Ada.Calendar.Month_Number;
      Unused3 : Ada.Calendar.Day_Number;
      Seconds : Ada.Calendar.Day_Duration;
      Hue : Integer;
      X : Float;
      Rp : Float;
      Gp : Float;
      Bp : Float;
   begin
      Search_Component_IDs.Append (IDs.To_CID ("TextComponent"));
      Search_Component_IDs.Append (IDs.To_CID ("RainbowTextComponent"));
      Matched_Entities := ECS.Get_Entities_Matching (Entity_List, Search_Component_IDs);
      for EID of Matched_Entities loop
         Component_List := ECS.Get_Entity_Components (Entity_List, EID);
         Text_C := Components.Text_Component_T (
            ECS.Get_Component (Component_List.all, IDs.To_CID ("TextComponent"))
                                               );
         RainbowText_C := RainbowTextComponent (
            ECS.Get_Component (Component_List.all, IDs.To_CID ("RainbowTextComponent"))
                                          );

         --  Update text color based on current time
         Ada.Calendar.Split (Now, Unused1, Unused2, Unused3, Seconds);
         Hue := Integer (Float (Seconds) * Float (RainbowText_C.Hue_Change_Speed)) mod 360;
         X := 1.0 - abs (Float'Remainder (Float (Hue) / 60.0, 2.0) - 1.0);
         if Hue < 60 then
               Rp := 1.0; Gp := X; Bp := 0.0;
         elsif Hue < 120 then
               Rp := X; Gp := 1.0; Bp := 0.0;
         elsif Hue < 180 then
               Rp := 0.0; Gp := 1.0; Bp := X;
         elsif Hue < 240 then
               Rp := 0.0; Gp := X; Bp := 1.0;
         elsif Hue < 300 then
               Rp := X; Gp := 0.0; Bp := 1.0;
         elsif Hue <= 360 then
               Rp := 1.0; Gp := 0.0; Bp := X;
         end if;
         pragma Assert (Rp * 255.0 <= 255.0, "RED GOES OVER");
         Text_C.Text_Color.Red := Graphics.u8 (Integer (Rp * 255.0) mod 256);
         Text_C.Text_Color.Green := Graphics.u8 (Integer (Gp * 255.0) mod 256);
         Text_C.Text_Color.Blue := Graphics.u8 (Integer (Bp * 255.0) mod 256);

         --  Update text component
         ECS.Add_Component (
            ECS.Get_Entity_Components (Entity_List, EID).all,
            IDs.To_CID ("TextComponent"),
            Text_C
                           );
      end loop;
   end RainbowTextSystem;



   ---------------------------------------------------------------------------
   --  Progress Bar Widget Creation
   ---------------------------------------------------------------------------

   procedure Create_Progress_Bar
     (Entity_List  : in Out Entity_Components;
      E_ID         : in Entity_Id;
      Pos_X        : in TUI_Width;
      Pos_Y        : in TUI_Height;
      Width        : in TUI_Width;
      Height       : in TUI_Height := 1;
      Filled_Color : in Color_t := Green;
      Empty_Color  : in Color_t := Gray;
      BG_Color     : in Color_t := Black)
   is
      Comp_Ptr : Components_Ptr;
      Widget_C : Widget_Component_T;
      BG_C     : Background_Color_Component_T;
      PB_C     : Progress_Bar_Component_T;
   begin
      --  Add entity and get components pointer
      Comp_Ptr := Add_Entity (Entity_List, E_ID);

      --  Configure Widget Component
      Widget_C.Position_X := Pos_X;
      Widget_C.Position_Y := Pos_Y;
      Widget_C.Size_Width := Width;
      Widget_C.Size_Height := Height;
      Widget_C.Is_Visible := True;
      Widget_C.Is_Enabled := True;
      Widget_C.Has_Focus := False;
      Widget_C.Render_Buffer := Create_Buffer (Width, Height);

      --  Configure Background Color Component
      BG_C.Background_Color := BG_Color;

      --  Configure Progress Bar Component
      PB_C.Value := 0.0;
      PB_C.Filled_Char := '=';
      PB_C.Empty_Char := ' ';
      PB_C.Filled_Color := Filled_Color;
      PB_C.Empty_Color := Empty_Color;
      PB_C.Show_Percentage := True;
      PB_C.Border_Left := '[';
      PB_C.Border_Right := ']';

      --  Add components to entity
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG_C);
      Add_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent"), PB_C);
   end Create_Progress_Bar;

   ---------------------------------------------------------------------------
   --  Progress Bar Operations
   ---------------------------------------------------------------------------

   procedure Set_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Value       : in Float)
   is
      Comp_Ptr : Components_Ptr;
      PB_C     : Progress_Bar_Component_T;
      Clamped  : Float;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List, E_ID);
      if Comp_Ptr = null then
         return;
      end if;

      if not Has_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")) then
         return;
      end if;

      --  Clamp value to 0.0 .. 1.0
      if Value <= 0.0 then
         Clamped := 0.0;
      elsif Value >= 1.0 then
         Clamped := 1.0;
      else
         Clamped := Value;
      end if;

      --  Get current component, update, and store back
      PB_C := Progress_Bar_Component_T (
         Get_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")));
      PB_C.Value := Clamped;
      Add_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent"), PB_C);
   end Set_Progress;

   function Get_Progress
     (Entity_List : in Entity_Components;
      E_ID        : in Entity_Id) return Float
   is
      Comp_Ptr : Components_Ptr;
      PB_C     : Progress_Bar_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List, E_ID);
      if Comp_Ptr = null then
         return 0.0;
      end if;

      if not Has_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")) then
         return 0.0;
      end if;

      PB_C := Progress_Bar_Component_T (
         Get_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")));
      return PB_C.Value;
   end Get_Progress;

   procedure Increment_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Amount      : in Float)
   is
      Current : Float;
   begin
      Current := Get_Progress (Entity_List, E_ID);
      Set_Progress (Entity_List, E_ID, Current + Amount);
   end Increment_Progress;

   procedure Setup_Render_Info (Entities : in out Entity_Components;
                                Render_Info_ID : Entity_Id;
                                Term_Width : TUI_Width;
                                Term_Height : TUI_Height) is
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

   procedure Setup_Root_Widget (Entities : in out Entity_Components;
                                Root_ID : Entity_Id;
                                Term_Width : TUI_Width;
                                Term_Height : TUI_Height;
                                Child_IDs : Entity_ID_Vector.Vector) is
      Comp_Ptr : Components_Ptr;
      Widget_C : Widget_Component_T;
      Root_C   : Root_Widget_Component_T;
      Text_C   : constant Text_Component_T := (
         Text => Ada.Strings.Unbounded.To_Unbounded_String
            ("=== Multi Progress Bar Demo ===" &
               "                                       " &
               "Simulating a multi-stage installation..."),
         Text_Color => Graphics.White);
   begin
      Comp_Ptr := Add_Entity (Entities, Root_ID);
      Widget_C.Position_X := TUI_Width'First;
      Widget_C.Position_Y := TUI_Height'First;
      Widget_C.Size_Width := Term_Width;
      Widget_C.Size_Height := Term_Height;
      Widget_C.Is_Visible := True;
      Widget_C.Is_Enabled := True;
      Widget_C.Render_Buffer := Create_Buffer (Term_Width, Term_Height);

      --  Add all children
      for Child_ID of Child_IDs loop
         Widget_C.Children.Append (Child_ID);
         Widget_C.Children.Append (Child_ID);
         Widget_C.Children.Append (Child_ID);
      end loop;

      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
                     Background_Color_Component_T'(Background_Color => (64, 64, 64)));
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text_C);
   end Setup_Root_Widget;

   procedure Run_Systems (Entities : in out Entity_Components) is
   begin
      RainbowTextSystem (Entities);
      WidgetBackgroundSystem (Entities);
      TextRenderSystem (Entities);
      ProgressBarRenderSystem (Entities);
      BufferCopySystem (Entities);
      BufferDrawSystem (Entities);
   end Run_Systems;

   procedure Setup_Animation (Entities : in out Entity_Components;
                              Animation_ID : Entity_Id) is
      --  Components access val
      Comp_Ptr : constant Components_Ptr := Add_Entity (Entities, Animation_ID);
      --  Components of animation
      Widget_C : constant Widget_Component_T :=
        (Position_X => 45,
         Position_Y => 11,
         Size_Width => 15,
         Size_Height => 4,
         Has_Focus => True,
         others => <>);
      Background_Color_C : constant Background_Color_Component_T :=
        (Background_Color => (96, 96, 96));
      Text_C : constant Text_Component_T :=
        (Text => SU.To_Unbounded_String ("This text will shift through colors!"),
         Text_Color => (128, 0, 0));
      Rainbow_Text_C : constant RainbowTextComponent :=
        (Hue_Change_Speed => 20);
   begin

      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
                     Background_Color_C);
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text_C);
      Add_Component (Comp_Ptr.all, To_CID ("RainbowTextComponent"), Rainbow_Text_C);

   end Setup_Animation;

end User_Library;
