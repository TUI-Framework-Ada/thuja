  with ECS; use ECS;

package Systems is
   --  Built-in systems
   procedure WidgetBackgroundSystem (Entity_List : Entities);
   procedure TextRenderSystem (Entity_List : Entities);
   procedure BufferCopySystem (Entity_List : Entities);
   procedure BufferDrawSystem (Entity_List : Entities);
end Systems;
