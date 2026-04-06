with ECS;
with Graphics; use Graphics;

package Standardized_tab_interface is

   ----------------------------------------------------------------------------
   --  Tab_Interface
   --
   --  PURPOSE
   --  -------
   --  Defines the abstract interface that every tab in the Thuja demo must
   --  implement. Each tab provides a Create_Entities procedure called once
   --  at startup and an Update procedure called every frame when that tab
   --  is active. Thuja_Demo drives all tabs uniformly through this interface
   --  with no tab-specific logic in the main demo file.
   ----------------------------------------------------------------------------

   type Tab_T is abstract tagged null record;
   type Tab_Access is access all Tab_T'Class;

   --  Called once at startup to create all ECS entities for this tab.
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is abstract;

   --  Called every frame when this tab is the active tab.
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO)
   is abstract;

end Standardized_tab_interface;
