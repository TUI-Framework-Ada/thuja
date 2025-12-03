with Ada.Strings.Unbounded;
with Components;
with ECS;
with Graphics;
with IDs;
with User_Library;

procedure Demos is

   --  Convenience rename
   package SU renames Ada.Strings.Unbounded;

   --  Variables for Thuja

   --  Instance of Entity_Components
   Entities : ECS.Entity_Components;
   --  Instantiate entity IDs for registering with Entities
   E1_ID : constant IDs.Entity_Id := IDs.To_EID ("Render info");
   E2_ID : constant IDs.Entity_Id := IDs.To_EID ("Root widget");
   E3_ID : constant IDs.Entity_Id := IDs.To_EID ("Text box");
   --  Register each entity and store pointers to their Components instances
   E1_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E1_ID);
   E2_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E2_ID);
   E3_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E3_ID);
   --  Components of entity E1 (render info)
   E1_RIC : constant Components.Render_Info_Component_T := (
      Terminal_Width => 80,
      Terminal_Height => 24,
      Framebuffer => (Width => 80, Height => 24, Data => <>),
      Backbuffer => (Width => 80, Height => 24, Data => <>)
                                                           );
   --  Components of entity E2 (root widget)
   E2_WC : constant Components.Widget_Component_T := (
      Size_Width => 80,
      Size_Height => 24,
      Children => [E3_ID],
      others => <>
                                                     );
   --  The compiler doesn't like the redundant "others" assignment here,
   --    but not including will make it never be initialized (or just error)
   E2_RWC : constant Components.Root_Widget_Component_T := (others => <>);
   --  Components of entity E3 (a green box)
   E3_WC : constant Components.Widget_Component_T := (
      Position_X => 5,
      Position_Y => 3,
      Size_Width => 10,
      Size_Height => 5,
      Has_Focus => True,
      others => <>
                                                     );
   E3_BCC : constant Components.Background_Color_Component_T := (
      Background_Color => (64, 64, 64));
   E3_TC : constant Components.Text_Component_T := (
      Text => SU.To_Unbounded_String ("This text will shift through colors!"),
      Text_Color => Graphics.Red
                                                   );
   E3_CC : constant User_Library.RainbowTextComponent := (Hue_Change_Speed => 60);
begin

   --  Continue setup of components

   ECS.Add_Component (E1_C.all, IDs.To_CID ("RenderInfo"), E1_RIC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("WidgetComponent"), E2_WC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("RootWidget"), E2_RWC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("WidgetComponent"), E3_WC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("BackgroundColorComponent"), E3_BCC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("TextComponent"), E3_TC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("RainbowTextComponent"), E3_CC);

   --  Remaining Thuja init
   Graphics.Clear_Screen;

   --  Main loop

   --  Loop forever
   loop

      --  Rainbow text system
      User_Library.RainbowTextSystem (Entities);

      --  Execute systems (in correct order)
      ECS.WidgetBackgroundSystem (Entities);
      ECS.TextRenderSystem (Entities);
      ECS.BufferCopySystem (Entities);
      ECS.BufferDrawSystem (Entities);

      --  30 FPS
      delay Duration (1.0 / 30.0);
   end loop;
end Demos;
