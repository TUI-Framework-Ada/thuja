with Tab_Interface;
with ECS;
with Graphics; use Graphics;

package Tab_Flex is

   type Tab_T is new Tab_Interface.Tab_T with null record;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height);

   overriding
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO);

end Tab_Flex;
