with Ada.Text_IO;
with Components;
with ECS;
with Graphics;
with IDs;

procedure Demos is

   --  Number of times main loop should iterate
   Loop_Count : constant Positive := 20; --  Loop 20 times

   --  Variables for Thuja

   --  Instance of Entity_Components
   Entities : ECS.Entity_Components;
   --  Instantiate entity IDs for registering with Entities
   E1_ID : constant IDs.Entity_Id := IDs.To_EID ("Render info");
   E2_ID : constant IDs.Entity_Id := IDs.To_EID ("Root widget");
   E3_ID : constant IDs.Entity_Id := IDs.To_EID ("Green box");
   --  Register each entity and store pointers to their Components instances
   E1_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E1_ID);
   E2_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E2_ID);
   E3_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E3_ID);
   --  Components of entity E1 (render info)
   E1_RIC : constant Components.Render_Info_Component_T := (Terminal_Width => 80,
                                                   Terminal_Height => 24,
                                                   others => <>);
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
      Background_Color => Graphics.Green);
begin

   --  Continue setup of components
   --  Initialization and assigning of components can be delegated into an independent block (components are updated by copy)
   --  The Entity_Components instance will need to remain visible, and the entity IDs should too
   --  Those last two can be handled by instantiating them statically in a user library to avoid clutter

   --  For now, the component IDs need to be exact values for the systems to acknowledge them
   ECS.Add_Component (E1_C.all, IDs.To_CID ("RenderInfo"), E1_RIC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("WidgetComponent"), E2_WC);
   ECS.Add_Component (E2_C.all, IDs.To_CID ("RootWidget"), E2_RWC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("WidgetComponent"), E3_WC);
   ECS.Add_Component (E3_C.all, IDs.To_CID ("BackgroundColorComponent"), E3_BCC);

   --  Main loop

   for Loop_Index in Positive'First .. Loop_Count loop

      --  Execute systems (in correct order)
      ECS.WidgetBackgroundSystem (Entities);
      ECS.TextRenderSystem (Entities);
      ECS.BufferCopySystem (Entities);
      ECS.BufferDrawSystem (Entities);

      --  Sleep for 0.1 seconds
      delay Duration (0.1);
   end loop;

   --  Print success line on demo end
   Ada.Text_IO.Put_Line ("Thank you for using the Thuja demo");
end Demos;
