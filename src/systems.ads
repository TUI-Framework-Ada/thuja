with ECS; use ECS;

package Systems is
   --  Built-in systems
   procedure TextRenderSystem (Entity_List : Entities);
   procedure BufferCopySystem (Entity_List : Entities);
   procedure BufferDrawSystem (Entity_List : Entities);
end Systems;
