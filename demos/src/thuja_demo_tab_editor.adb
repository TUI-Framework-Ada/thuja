with Components; use Components;
with IDs;        use IDs;
with ECS;        use ECS;
with Text_Editor;
with Ada.Strings.Unbounded;
with Scroll;

package body Thuja_demo_tab_editor is

   package SU renames Ada.Strings.Unbounded;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      CP      : Components_Ptr;
      T       : Text_Component_T;
      Page    : Tab_Page_Component_T;
      Ed_BG   : constant Color_t := (Red => 25, Green => 25, Blue => 25);
      Stat_BG : constant Color_t := Blue;
   begin
      Page.Tab_Index := 1;
      Ed_H := Term_Height - Content_Top - 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "ed_area",
           TUI_Width'First,
           Content_Top,
           Term_Width,
           Ed_H,
           Ed_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Page);
      T.Text :=
        SU.To_Unbounded_String
          (SU.To_String (Text_Editor.Build_Editor_Text (0, Natural (Ed_H))));
      T.Text_Color := White;
      T.Offset_X := 1;
      T.Offset_Y := 1;
      T.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), T);

      CP :=
        Make_Widget_With_BG
          (World,
           "ed_status",
           TUI_Width'First,
           Content_Top + Ed_H,
           Term_Width,
           1,
           Stat_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Page);
      T.Text :=
        SU.To_Unbounded_String (SU.To_String (Text_Editor.Status_Text));
      T.Text_Color := White;
      T.Offset_X := 1;
      T.Offset_Y := 1;
      T.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
   end Create_Entities;

   overriding
   procedure Update
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      EL   : ECS.Entity_Components_Ptr;
      CP   : Components_Ptr;
      T    : Text_Component_T;
   begin
      World.Claim_Writing (EL);

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("ed_area"));
      if CP /= null then
         Scroll.Update
           (Current_Line  => Text_Editor.Current_Line,
            Total_Lines   => Natural (Text_Editor.Lines.Length),
            Visible_Rows  => Positive (Ed_H),
            Scroll_Offset => Scroll_Offset);
         T :=
           Text_Component_T
             (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text :=
           SU.To_Unbounded_String
             (SU.To_String (Text_Editor.Build_Editor_Text (Scroll_Offset, Natural( Ed_H))));
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("ed_status"));
      if CP /= null then
         T :=
           Text_Component_T
             (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text :=
           SU.To_Unbounded_String (SU.To_String (Text_Editor.Status_Text));
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      World.Release_Writing;
   end Update;

end Thuja_demo_tab_editor;
