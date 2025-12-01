with Ada.Text_IO; use Ada.Text_IO;
with IDs; use IDs;
with ECS; use ECS;
with Components; use Components;
with Graphics; use Graphics;

procedure demos is

   Entity_List : Entity_Components;

   -- Helper: create an entity with ID
   function Make (Name : String) return Components_Ptr is
   begin
      return Add_Entity (Entity_List, To_EID (Name));
   end Make;

   -- Helper: allocate a widget of given size & position
   function Make_Widget
     (X, Width : TUI_Width; Y, Height : TUI_Height)
      return Widget_Component_T
   is
      W : Widget_Component_T;
   begin
      W.Position_X   := X;
      W.Position_Y   := Y;
      W.Size_Width   := X + Width - 1;
      W.Size_Height  := Y + Height - 1;
      W.Render_Buffer := Create_Buffer (Width, Height);
      return W;
   end Make_Widget;

   -- Background color helper
   function Make_BG (Color : Color_t) return Background_Color_Component_T is
      BG : Background_Color_Component_T;
   begin
      BG.Background_Color := Color;
      return BG;
   end Make_BG;

   Root_Ptr, Parent_Ptr, Child_Ptr : Components_Ptr;

   Root_Widget   : Widget_Component_T;
   Parent_Widget : Widget_Component_T;
   Child_Widget  : Widget_Component_T;

   RI_Ptr : Components_Ptr;
   RI     : Render_Info_Component_T;

begin
   Put_Line ("TC-005: Child Layering Demo");

   --------------------------------------------------------------------
   -- Setup RenderInfo entity
   --------------------------------------------------------------------
   RI_Ptr := Make ("RenderInfo");
   RI.Terminal_Width  := 80;
   RI.Terminal_Height := 30;
   RI.BackBuffer  := Create_Buffer (80, 30);
   RI.FrameBuffer := Create_Buffer (80, 30);

   Add_Component (RI_Ptr.all, To_CID ("RenderInfo"), RI);

   --------------------------------------------------------------------
   -- Create Root widget
   --------------------------------------------------------------------
   Root_Ptr := Make ("Root");

   Root_Widget.Position_X  := 1;
   Root_Widget.Position_Y  := 1;
   Root_Widget.Size_Width  := 80;
   Root_Widget.Size_Height := 30;
   Root_Widget.Render_Buffer := Create_Buffer (80, 30);

   Add_Component (Root_Ptr.all, To_CID ("WidgetComponent"), Root_Widget);
   Add_Component (Root_Ptr.all, To_CID ("RootWidget"), Root_Widget);

   --------------------------------------------------------------------
   -- Create Parent widget (blue background)
   --------------------------------------------------------------------
   Parent_Ptr := Make ("Parent");

   Parent_Widget.Position_X  := 10;
   Parent_Widget.Position_Y  := 5;
   Parent_Widget.Size_Width  := 30;
   Parent_Widget.Size_Height := 10;
   Parent_Widget.Render_Buffer := Create_Buffer (30, 10);

   Add_Component (Parent_Ptr.all, To_CID ("WidgetComponent"), Parent_Widget);
   Add_Component (Parent_Ptr.all, To_CID ("BackgroundColorComponent"),
                  Make_BG (Blue));

   --------------------------------------------------------------------
   -- Create Child widget (red background, overlaps parent)
   --------------------------------------------------------------------
   Child_Ptr := Make ("Child");

   Child_Widget.Position_X  := 16;   -- Overlaps inside parent region
   Child_Widget.Position_Y  := 8;
   Child_Widget.Size_Width  := 12;
   Child_Widget.Size_Height := 5;
   Child_Widget.Render_Buffer := Create_Buffer (12, 5);

   Add_Component (Child_Ptr.all, To_CID ("WidgetComponent"), Child_Widget);
   Add_Component (Child_Ptr.all, To_CID ("BackgroundColorComponent"),
                  Make_BG (Red));

   --------------------------------------------------------------------
   -- Build Hierarchy: Root → Parent → Child
   --------------------------------------------------------------------
   declare
      R : Widget_Component_T :=
        Widget_Component_T (Get_Component (Root_Ptr.all, To_CID ("WidgetComponent")));

      P : Widget_Component_T :=
        Widget_Component_T (Get_Component (Parent_Ptr.all, To_CID ("WidgetComponent")));
   begin
      -- Root owns Parent
      R.Children.Append (To_EID ("Parent"));
      Add_Component (Root_Ptr.all, To_CID ("WidgetComponent"), R);

      -- Parent owns Child
      P.Children.Append (To_EID ("Child"));
      Add_Component (Parent_Ptr.all, To_CID ("WidgetComponent"), P);
   end;

   --------------------------------------------------------------------
   -- Run built-in ECS systems
   --------------------------------------------------------------------
   Graphics.Clear_Screen;

   -- Fill widget backgrounds
   WidgetBackgroundSystem (Entity_List);

   -- Fill text / content if any
   TextRenderSystem (Entity_List);

   -- Copy buffers parent → child → framebuffer
   BufferCopySystem (Entity_List);

   -- Draw changed pixels to terminal
   BufferDrawSystem (Entity_List);

   Put_Line ("TC-005 Complete: Child widget (RED) should be drawn on top of Parent widget (BLUE)");

end demos;
