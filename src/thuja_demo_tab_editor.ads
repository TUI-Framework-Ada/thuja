with standardized_tab_interface;
with ECS;
with Graphics; use Graphics;

package Thuja_demo_tab_editor is

   type Tab_T is new standardized_tab_interface.Tab_T with null record;

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

end Thuja_demo_tab_editor;
