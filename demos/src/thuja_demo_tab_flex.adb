with Components; use Components;
with IDs;        use IDs;
with ECS;        use ECS;
with Flex_Demo;
with Flexbox;
with Ada.Strings.Unbounded;

package body Thuja_demo_tab_flex is

   package SU renames Ada.Strings.Unbounded;

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      CP         : Components_Ptr;
      Page       : Tab_Page_Component_T;
      Txt        : Text_Component_T;
      Fl         : Flex_Layout_Component_T;
      Dark_BG    : constant Color_t := (Red => 20, Green => 20, Blue => 35);
      Con_BG     : constant Color_t := (Red => 30, Green => 30, Blue => 50);
      Child_H    : constant Natural := Flex_Demo.Con_H / 2;
      Flex_Con_X : constant TUI_Width := 4;
      Flex_Con_Y : constant TUI_Height := Content_Top + 2;

      --  Maximum possible container height — used to pre-allocate child buffers
      Max_Con_H : constant Natural :=
        Natural (Term_Height) - Natural (Content_Top) - 5;

      Child_Colors : constant array (1 .. Flex_Demo.Num_Items) of Color_t :=
        [(Red => 70, Green => 130, Blue => 180),
         (Red => 80, Green => 160, Blue => 80),
         (Red => 180, Green => 120, Blue => 50),
         (Red => 140, Green => 70, Blue => 160)];
   begin
      Page.Tab_Index := 2;

      CP :=
        Make_Widget_With_BG
          (World,
           "flex_status",
           Flex_Con_X,
           Content_Top,
           Term_Width - 4,
           1,
           Dark_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Page);
      Txt.Text :=
        SU.To_Unbounded_String
          ("Justify: Flex_Start    Align: Flex_Start    Width: "
           & Img (Flex_Demo.Con_W)
           & "  Height: "
           & Img (Flex_Demo.Con_H));
      Txt.Text_Color := White;
      Txt.Offset_X := 1;
      Txt.Offset_Y := 1;
      Txt.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), Txt);

      CP :=
        Make_Widget_With_BG
          (World,
           "flex_con",
           Flex_Con_X,
           Flex_Con_Y,
           TUI_Width (Flex_Demo.Con_W),
           TUI_Height (Flex_Demo.Con_H),
           Con_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Page);

      for I in 1 .. Flex_Demo.Num_Items loop
         declare
            Child_CP : constant Components_Ptr :=
              Make_Widget_With_BG
                (World,
                 Flex_Demo.Child_Names (I),
                 Flex_Con_X,
                 Flex_Con_Y,
                 TUI_Width (Flex_Demo.Item_Basis),
                 TUI_Height (Max_Con_H),   --  allocate at max height upfront
                 Child_Colors (I));
         begin
            Add_Component (Child_CP.all, To_CID ("TabPage"), Page);
            Txt.Text := SU.To_Unbounded_String (Img (I));
            Txt.Text_Color := White;
            Txt.Offset_X := TUI_Width (Flex_Demo.Item_Basis / 2);
            Txt.Offset_Y := TUI_Height (Child_H / 2);
            Txt.Is_Bold := True;
            Add_Component (Child_CP.all, To_CID ("TextComponent"), Txt);
         end;
      end loop;

      declare
         Items_Ptr : constant Flexbox.Flex_Item_Array_Ptr :=
           new Flexbox.Flex_Item_Array (1 .. Flex_Demo.Num_Items);
      begin
         for I in 1 .. Flex_Demo.Num_Items loop
            Items_Ptr (I) :=
              (Related_Entity => To_EID (Flex_Demo.Child_Names (I)),
               Flex_Grow      => 0.0,
               Flex_Shrink    => 1.0,
               Flex_Basis     => Flex_Demo.Item_Basis,
               Computed_Size  => Flex_Demo.Item_Basis,
               Position_X     => 0,
               Position_Y     => 0,
               Cross_Size     => Child_H);
         end loop;

         Fl.Flex_Container :=
           (Width      => Flex_Demo.Con_W,
            Height     => Flex_Demo.Con_H,
            Direction  => Flexbox.Row,
            Justify    => Flex_Demo.Current_Justify,
            Align      => Flex_Demo.Current_Align,
            Items      => Items_Ptr,
            Item_Count => Flex_Demo.Num_Items);
         Fl.Is_Dirty := True;
         Add_Component (CP.all, To_CID ("FlexLayoutComponent"), Fl);
      end;

      declare
         Con_W : Widget_Component_T :=
           Widget_Component_T
             (ECS.Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         for I in 1 .. Flex_Demo.Num_Items loop
            Con_W.Children.Append (To_EID (Flex_Demo.Child_Names (I)));
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), Con_W);
      end;
   end Create_Entities;

   overriding
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO)
   is
      EL : ECS.Entity_Components_Ptr;
      CP : Components_Ptr;
   begin
      World.Claim_Writing (EL);

      --  Update flex container dimensions and layout properties
      CP := ECS.Get_Entity_Components (EL.all, To_EID ("flex_con"));
      if CP /= null then
         declare
            W  : Widget_Component_T :=
              Widget_Component_T
                (ECS.Get_Component (CP.all, To_CID ("WidgetComponent")));
            Fl : Flex_Layout_Component_T :=
              Flex_Layout_Component_T
                (ECS.Get_Component (CP.all, To_CID ("FlexLayoutComponent")));
         begin
            W.Size_Width := TUI_Width (Flex_Demo.Con_W);
            W.Size_Height := TUI_Height (Flex_Demo.Con_H);
            W.Render_Buffer := Create_Buffer (W.Size_Width, W.Size_Height);
            Fl.Flex_Container.Width := Flex_Demo.Con_W;
            Fl.Flex_Container.Height := Flex_Demo.Con_H;
            Fl.Flex_Container.Justify := Flex_Demo.Current_Justify;
            Fl.Flex_Container.Align := Flex_Demo.Current_Align;
            Fl.Is_Dirty := True;
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
            Add_Component (CP.all, To_CID ("FlexLayoutComponent"), Fl);
         end;
      end if;

      --  Update status text
      CP := ECS.Get_Entity_Components (EL.all, To_EID ("flex_status"));
      if CP /= null then
         declare
            T : Text_Component_T :=
              Text_Component_T
                (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
         begin
            T.Text :=
              SU.To_Unbounded_String
                ("Justify: "
                 & Flex_Demo.Current_Justify_Name
                 & "   Align: "
                 & Flex_Demo.Current_Align_Name
                 & "   Width: "
                 & Img (Flex_Demo.Con_W)
                 & "  Height: "
                 & Img (Flex_Demo.Con_H));
            Add_Component (CP.all, To_CID ("TextComponent"), T);
         end;
      end if;

      World.Release_Writing;

      --  Run layout so Cross_Size is computed correctly for each align mode
      ECS.FlexLayoutSystem (World);
      ECS.FlexAlignTextSystem (World);

      --  Now reallocate child buffers to match the Cross_Size FlexLayoutSystem set
      declare
         EL2 : ECS.Entity_Components_Ptr;
      begin
         World.Claim_Writing (EL2);
         for I in 1 .. Flex_Demo.Num_Items loop
            declare
               Child_CP : constant Components_Ptr :=
                 ECS.Get_Entity_Components
                   (EL2.all, To_EID (Flex_Demo.Child_Names (I)));
            begin
               if Child_CP /= null then
                  declare
                     Child_W : Widget_Component_T :=
                       Widget_Component_T
                         (ECS.Get_Component
                            (Child_CP.all, To_CID ("WidgetComponent")));
                  begin
                     --  Size_Height was set by FlexLayoutSystem via Cross_Size
                     --  Reallocate buffer to match so background fills correctly
                     Child_W.Render_Buffer :=
                       Create_Buffer (Child_W.Size_Width, Child_W.Size_Height);
                     Add_Component
                       (Child_CP.all, To_CID ("WidgetComponent"), Child_W);
                  end;
               end if;
            end;
         end loop;
         World.Release_Writing;
      end;
   end Update;

end Thuja_demo_tab_flex;

