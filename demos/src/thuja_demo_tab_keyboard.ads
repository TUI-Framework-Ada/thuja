with Standardized_tab_interface;
with ECS;
with Graphics;        use Graphics;
with Input_Handling;  use Input_Handling;

package Thuja_demo_tab_keyboard is

   type Tab_T is new Standardized_tab_interface.Tab_T with null record;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height);

   overriding
   procedure Update
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height);

   --  Dispatched by the main demo loop when this tab is active.
   procedure Handle_Event
     (World : in out ECS.Entity_Components_PO;
      Event : in Input_Event_t);

end Thuja_demo_tab_keyboard;
