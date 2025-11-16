with ECS; use ECS;

package Systems is
   -- System_T "interface" (actually an abstract class to make impl easier)
-- type System_T is abstract tagged null record;
-- procedure Run(Self: System_T; Entity_List: Entities) is abstract;

   -- Built-in systems
-- type TextRenderSystem is new System_T;
-- procedure Run(Self: TextRenderSystem; Entity_List: Entities);

-- type BufferCopySystem is new System_T;
-- procedure Run(Self: BufferCopySystem; Entity_List: Entities);

-- type BufferDrawSystem is new System_T;
-- procedure Run(Self: BufferDrawSystem; Entity_List: Entities);

   -- Turns out Ada can't actually do static abstract methods, oh well
   procedure TextRenderSystem(Entity_List: Entities);
   procedure BufferCopySystem(Entity_List: Entities);
   procedure BufferDrawSystem(Entity_List: Entities);
end Systems;
