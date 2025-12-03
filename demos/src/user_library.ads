with Components;
with ECS;

package User_Library is

   type RainbowTextComponent is new Components.Component_T with record
      Hue_Change_Speed : Integer := 0;
   end record;

   procedure RainbowTextSystem (Entity_List : ECS.Entity_Components);

end User_Library;
