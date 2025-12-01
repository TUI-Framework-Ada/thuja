with Components;
with ECS;

package User_Library is

   type Custom_Component is new Components.Component_T with record
      Hue_Change_Speed : Integer := 0;
   end record;

   procedure Custom_System (Entity_List : ECS.Entity_Components);

end User_Library;
