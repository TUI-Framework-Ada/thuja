with Components;
with ECS;
with Graphics;
with IDs;

procedure Demos is

   --  Backbuffer starting string
   BB_Text : constant String := "Hello worl ";
   --  Framebuffer starting string
   FB_Text : constant String := "Hello world";

   --  Variables for Thuja

   --  Instance of Entity_Components
   Entities : ECS.Entity_Components;
   --  Instantiate entity IDs for registering with Entities
   E1_ID : constant IDs.Entity_Id := IDs.To_EID ("Render info");
   --  Register each entity and store pointers to their Components instances
   E1_C : constant ECS.Components_Ptr := ECS.Add_Entity (Entities, E1_ID);
   --  Components of entity E1 (render info)
   E1_RIC : Components.Render_Info_Component_T := (
      Terminal_Width => 11,
      Terminal_Height => 1,
      Framebuffer => (Width => 11, Height => 1, Data => <>),
      Backbuffer => (Width => 11, Height => 1, Data => <>)
                                                           );
begin

   --  Continue setup of components
   --  Initialization and assigning of components can be delegated into an independent block (components are updated by copy)
   --  The Entity_Components instance will need to remain visible, and the entity IDs should too
   --  Those last two can be handled by instantiating them statically in a user library to avoid clutter

   --  Assign starting strings
   for BB_Index in Graphics.TUI_Width'First .. BB_Text'Length loop
      Graphics.Set_Buffer_Pixel (E1_RIC.Backbuffer,
                                 BB_Index,
                                 Graphics.TUI_Height'First,
                                 (
                                  BB_Text (Integer (BB_Index)),
                                  Graphics.White,
                                  Graphics.Black,
                                  False
                                 ));
   end loop;

   for FB_Index in Graphics.TUI_Width'First .. FB_Text'Length loop
      Graphics.Set_Buffer_Pixel (E1_RIC.Framebuffer,
                                 FB_Index,
                                 Graphics.TUI_Height'First,
                                 (
                                  FB_Text (Integer (FB_Index)),
                                  Graphics.White,
                                  Graphics.Black,
                                  False
                                 ));
   end loop;

   --  For now, the component IDs need to be exact values for the systems to acknowledge them
   ECS.Add_Component (E1_C.all, IDs.To_CID ("RenderInfo"), E1_RIC);

   --  Run only BufferDrawSystem, and just once
   ECS.BufferDrawSystem (Entities);
   --  There should now be a singular new pixel rendered to the terminal ('d')

end Demos;
