with Components; use Components;
with IDs;        use IDs;
with ECS;        use ECS;
with Sort_Demo;

package body Thuja_demo_tab_sort is

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      CP  : Components_Ptr;
      Page : Tab_Page_Component_T;
   begin
      Page.Tab_Index := 3;
      Sort_Bar_H  := Term_Height - Content_Top - 1;
      Sort_Code_H := Sort_Bar_H;
      Sort_Stat_H := 2;

      CP :=
        Make_Widget
          (World,
           "sort_bars",
           TUI_Width'First,
           Content_Top,
           TUI_Width (Sort_Demo.Bar_W),
           Sort_Bar_H);
      Add_Component (CP.all, To_CID ("TabPage"), Page);

      CP :=
        Make_Widget
          (World,
           "sort_code",
           TUI_Width (Sort_Demo.Bar_W + 1),
           Content_Top,
           TUI_Width (Sort_Demo.Code_W),
           Sort_Code_H);
      Add_Component (CP.all, To_CID ("TabPage"), Page);

      CP :=
        Make_Widget
          (World,
           "sort_stat",
           TUI_Width'First,
           Term_Height - 1,
           Term_Width,
           Sort_Stat_H);
      Add_Component (CP.all, To_CID ("TabPage"), Page);
   end Create_Entities;

   overriding
   procedure Update
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
   begin
      Sort_Demo.Tick;

      World.Claim_Writing (EL);

      CP := Get_Entity_Components (EL.all, To_EID ("sort_bars"));
      if CP /= null then
         declare
            W : Widget_Component_T :=
              Widget_Component_T
                (Get_Component (CP.all, To_CID ("WidgetComponent")));
         begin
            Sort_Demo.Render_Bars
              (W.Render_Buffer,
               TUI_Width (Sort_Demo.Bar_W),
               Sort_Bar_H);
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         end;
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("sort_code"));
      if CP /= null then
         declare
            W : Widget_Component_T :=
              Widget_Component_T
                (Get_Component (CP.all, To_CID ("WidgetComponent")));
         begin
            Sort_Demo.Render_Code
              (W.Render_Buffer,
               TUI_Width (Sort_Demo.Code_W),
               Sort_Code_H);
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         end;
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("sort_stat"));
      if CP /= null then
         declare
            W : Widget_Component_T :=
              Widget_Component_T
                (Get_Component (CP.all, To_CID ("WidgetComponent")));
         begin
            Sort_Demo.Render_Status (W.Render_Buffer, W.Size_Width, Sort_Stat_H);
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         end;
      end if;

      World.Release_Writing;
   end Update;

end Thuja_demo_tab_sort;
