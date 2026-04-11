with standardized_tab_interface;
with ECS;
with Graphics; use Graphics;

package Thuja_demo_tab_htop is

   type Tab_T is new standardized_tab_interface.Tab_T with null record;

   BG_cpu      : constant Color_t := (Red => 10, Green => 20, Blue => 10);
   BG_mem      : constant Color_t := (Red => 20, Green => 15, Blue => 5);
   BG_disk     : constant Color_t := (Red => 5, Green => 15, Blue => 20);
   BG_prochead : constant Color_t := (Red => 15, Green => 15, Blue => 30);
   BG_procbody : constant Color_t := Blue;

   function Img (N : Natural) return String;

   function Pad (S : String; Len : Natural) return String;

   procedure Add_Text (CP : ECS.Components_Ptr; Text : String; Color : Color_t; Bold : Boolean);

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

end Thuja_demo_tab_htop;
