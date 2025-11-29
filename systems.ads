with ECS; use ECS;

package Systems is
   --  Built-in systems
   procedure WidgetBackgroundSystem (Entity_List : Entity_Components);
   procedure TextRenderSystem (Entity_List : Entity_Components);
   procedure BufferCopySystem (Entity_List : Entity_Components);
   procedure BufferDrawSystem (Entity_List : Entity_Components);
end Systems;
