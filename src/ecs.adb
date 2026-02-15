--==============================================================================
-- ECS.ADB - Entity Component System Implementation
--==============================================================================

with Ada.Calendar; use type Ada.Calendar.Time;
with Ada.Strings.Unbounded;
with Flexbox; use Flexbox;
with IDs; use type IDs.Component_Tag_Vector.Vector;
with Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Selection;

package body ECS is

   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;

   --===========================================================================
   -- HASH FUNCTIONS
   --===========================================================================

   function Hash_Component (Key : Component_Id) return Ada.Containers.Hash_Type is
   begin
      return SU.Hash (SU.Unbounded_String (Key));
   end Hash_Component;

   function Hash_Entity (Key : Entity_Id) return Ada.Containers.Hash_Type is
   begin
      return SU.Hash (SU.Unbounded_String (Key));
   end Hash_Entity;

   --===========================================================================
   -- COMPONENT OPERATIONS
   --===========================================================================

   procedure Add_Component (Self : in out Components;
                            Component : in Component_Id;
                            Component_Struct : in Component_T'Class) is
   begin
      Self.Components_Map.Include (Component, Component_Struct);
   end Add_Component;

   procedure Add_Component (Self : in out Components;
                            Component_Str : in String;
                            Component_Struct : in Component_T'Class) is
   begin
      Add_Component (Self, To_CID (Component_Str), Component_Struct);
   end Add_Component;

   procedure Remove_Component (Self : in out Components;
                               Component : in Component_Id) is
   begin
      Self.Components_Map.Exclude (Component);
   end Remove_Component;

   procedure Remove_Component (Self : in out Components;
                               Component_Str : in String) is
   begin
      Remove_Component (Self, To_CID (Component_Str));
   end Remove_Component;

   procedure Remove_Component (Self : in out Components;
                               Component_Tag : in Ada.Tags.Tag) is
      Component : Component_Id := Get_Component_ID (Self, Component_Tag);
   begin
      Remove_Component (Self, Component);
   end Remove_Component;

   function Get_Component (Self : in out Components;
                           Component : in Component_Id)
                           return Component_T'Class is
   begin
      return Self.Components_Map (Component);
   end Get_Component;

   function Get_Component (Self : in out Components;
                           Component_Str : in String)
                           return Component_T'Class is
   begin
      return Self.Components_Map (To_CID (Component_Str));
   end Get_Component;

   function Get_Component (Self : in Components;
                           Component_Tag : in Ada.Tags.Tag)
                           return Component_T'Class is
   begin
      for Component_Cursor in Self.Components_Map.Iterate loop
         if Self.Components_Map.Element (
            Component_Map_Pkg.Key (Component_Cursor))'Tag = Component_Tag
         then
            return Self.Components_Map.Element (
               Component_Map_Pkg.Key (Component_Cursor));
         end if;
      end loop;
      raise Constraint_Error with "No such component with tag ";
   end Get_Component;

   function Get_Component_ID (Self : in Components;
                              Component_Tag : in Ada.Tags.Tag)
                              return Component_Id is
   begin
      for Component_Cursor in Self.Components_Map.Iterate loop
         if Self.Components_Map.Element (
            Component_Map_Pkg.Key (Component_Cursor))'Tag = Component_Tag
         then
            return Component_Map_Pkg.Key (Component_Cursor);
         end if;
      end loop;
      raise Constraint_Error with "No such component with tag ";
   end Get_Component_ID;

   function Get_Component_IDs (Self : in Components;
                               Component_Tag : in Ada.Tags.Tag)
                               return Component_ID_Vector.Vector is
      Result : Component_ID_Vector.Vector;
   begin
      for Component_Cursor in Self.Components_Map.Iterate loop
         if Self.Components_Map.Element (
            Component_Map_Pkg.Key (Component_Cursor))'Tag = Component_Tag
         then
            Result.Append (Component_Map_Pkg.Key (Component_Cursor));
         end if;
      end loop;

      return Result;
   end Get_Component_IDs;

   function Get_Component_Ptr (Self : Components_Ptr;
                               Component_Key : Component_Id)
                               return Component_Class_Ptr is
      Map : Component_Map renames Self.all.Components_Map;
   begin
      return Map.Reference (Component_Key).Element;
   end Get_Component_Ptr;

   function Get_Component_Ptr (Self : Components_Ptr;
                               Component_Str : String)
                               return Component_Class_Ptr is
   begin
      return Get_Component_Ptr (Self, To_CID (Component_Str));
   end Get_Component_Ptr;

   function Get_Component_Ptr (Self : Components_Ptr;
                                     Component_Tag : Ada.Tags.Tag)
                                     return Component_Class_Ptr is
   begin
      return Get_Component_Ptr (Self, Get_Component_ID (Self.all, Component_Tag));
   end Get_Component_Ptr;

   function Has_Component (Self : in Components;
                           Component : in Component_Id) return Boolean is
   begin
      return Self.Components_Map.Contains (Component);
   end Has_Component;

   function Has_Component (Self : in Components;
                           Component_Str : in String) return Boolean is
   begin
      return Has_Component (Self, To_CID (Component_Str));
   end Has_Component;

   function Has_Component (Self : in Components;
                           Component_Tag : in Ada.Tags.Tag) return Boolean is
   begin
      for Component_Cursor in Self.Components_Map.Iterate loop
         if Self.Components_Map.Element (
            Component_Map_Pkg.Key (Component_Cursor)
         )'Tag = Component_Tag then
            return True;
         end if;
      end loop;
      return False;
   end Has_Component;

   --===========================================================================
   -- ENTITY PROTECTED OBJECT
   --===========================================================================

   protected body Entity_Components_PO is

      entry Claim_Reading (Entity_List : in out Entity_Components_Ptr)
        when not Write_Using is
      begin
         Read_Using := Read_Using + 1;
         Entity_List := Entities'Unchecked_Access;
      end Claim_Reading;

      entry Claim_Writing (Entity_List : in out Entity_Components_Ptr)
        when (Read_Using = 0) and (not Write_Using) is
      begin
         Write_Using := True;
         Entity_List := Entities'Unchecked_Access;
      end Claim_Writing;

      procedure Release_Reading is
      begin
         Read_Using := Read_Using - 1;
      end Release_Reading;

      procedure Release_Writing is
      begin
         Write_Using := False;
      end Release_Writing;

   end Entity_Components_PO;

   --===========================================================================
   -- ENTITY OPERATIONS
   --===========================================================================

   function Add_Entity (Self : in out Entity_Components_PO; Id : Entity_Id) return Components_Ptr is
      Entity_List : Entity_Components_Ptr;
      New_Components : Components_Ptr;
   begin
      Self.Claim_Writing (Entity_List);

      if Entity_List.Contains (Id) then
         New_Components := Entity_List (Id);
      else
         New_Components := new Components;
         Entity_List.Insert (Id, New_Components);
      end if;

      Self.Release_Writing;
      return New_Components;
   end Add_Entity;

   procedure Remove_Entity (Self : in out Entity_Components_PO; Id : Entity_Id) is
      Entity_List : Entity_Components_Ptr;
   begin
      Self.Claim_Writing (Entity_List);
      if Entity_List.Contains (Id) then
         Entity_List.Delete (Id);

         declare
            Search_Component_IDs : Component_ID_Vector.Vector;
            Matched_Entities : Entity_ID_Vector.Vector;
            Component_List : Components_Ptr;
         begin
            Search_Component_IDs.Append (To_CID ("WidgetComponent"));
            Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);
            for EID of Matched_Entities loop
               Component_List := Get_Entity_Components (Entity_List.all, EID);
               declare
                  Widget_C : Widget_Component_T renames Widget_Component_T (
                     Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
               begin
                  if Widget_C.Children.Contains (Id) then
                     Widget_C.Children.Delete (Widget_C.Children.Find_Index (Id));
                  end if;
               end;
            end loop;
         end;
      end if;
      Self.Release_Writing;
   end Remove_Entity;

   function Get_Entity_Components (Self : Entity_Components; Id : Entity_Id)
      return Components_Ptr
   is
   begin
      if Self.Contains (Id) then
         return Self.Element (Id);
      else
         return null;
      end if;
   end Get_Entity_Components;

   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector
   is
      Result : Entity_ID_Vector.Vector;
      Checking_Entity : Entity_Id;
      Matching : Boolean;
   begin
      for Entity_Cursor in Self.Iterate loop
         Matching := True;
         Checking_Entity := Entity_Map.Key (Entity_Cursor);

         for Component_Cursor in Required.Iterate loop
            if not (Has_Component(
               Entity_Map.Element (Self, Checking_Entity).all,
               Component_ID_Vector.Element (
                  Required, Component_ID_Vector.To_Index (Component_Cursor)
               )
            )) then
               Matching := False;
               exit;
            end if;
         end loop;

         if Matching then
            Result.Append (Checking_Entity);
         end if;
      end loop;

      return Result;
   end Get_Entities_Matching;

   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_Tag_Vector.Vector)
      return Entity_ID_Vector.Vector
   is
      Result : Entity_ID_Vector.Vector;
      Checking_Entity : Entity_Id;
      Matching : Boolean;
   begin
      for Entity_Cursor in Self.Iterate loop
         Matching := True;
         Checking_Entity := Entity_Map.Key (Entity_Cursor);

         for Component_Cursor in Required.Iterate loop
            if not (Has_Component(
               Entity_Map.Element (Self, Checking_Entity).all,
               Component_Tag_Vector.Element (
                  Required, Component_Tag_Vector.To_Index (Component_Cursor)
               )
            )) then
               Matching := False;
               exit;
            end if;
         end loop;

         if Matching then
            Result.Append (Checking_Entity);
         end if;
      end loop;

      return Result;
   end Get_Entities_Matching;

   --===========================================================================
   -- SYSTEM: TERMINAL RESIZE DETECTION
   --===========================================================================

   procedure TerminalResizeSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List           : Entity_Components_Ptr;
      Search_Component_Tags : Component_Tag_Vector.Vector;
      Matched_Entities      : Entity_ID_Vector.Vector;

      RI_Components : Components_Ptr;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);

      Search_Component_Tags.Append (Render_Info_Component_T'Tag);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for RI_Entity_ID of Matched_Entities loop
         RI_Components := Get_Entity_Components (Entity_List.all, RI_Entity_ID);
         declare
            RI : Render_Info_Component_T renames Render_Info_Component_T (
              Get_Component_Ptr (RI_Components, Render_Info_Component_T'Tag).all);
         begin
            if RI.Terminal_Width /= TUI_Width (RI.Prev_Terminal_Width) or
              RI.Terminal_Height /= TUI_Height (RI.Prev_Terminal_Height)
            then
               Mark_All_Flex_Dirty (Entity_List.all);

               RI.Prev_Terminal_Width := Natural (RI.Terminal_Width);
               RI.Prev_Terminal_Height := Natural (RI.Terminal_Height);
            end if;
         end;
      end loop;
      Entity_List_PO.Release_Reading;
   end TerminalResizeSystem;

   --===========================================================================
   -- HELPER: MARK ALL FLEX LAYOUTS DIRTY
   --===========================================================================

   procedure Mark_All_Flex_Dirty (Entity_List : in out Entity_Components) is
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Component_Tag_Vector.To_Vector (Flex_Layout_Component_T'Tag, 1);
      Matched_Entities      : Entity_ID_Vector.Vector;

      Flex_Components : Components_Ptr;
   begin
      Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_Tags);

      for Flex_Entity_ID of Matched_Entities loop
         Flex_Components := Get_Entity_Components (Entity_List, Flex_Entity_ID);
         declare
            Flex_C : Flex_Layout_Component_T renames Flex_Layout_Component_T (
              Get_Component_Ptr (Flex_Components, Flex_Layout_Component_T'Tag).all);
         begin
            Flex_C := Flex_Layout_Component_T (
               Get_Component (Flex_Components.all, Flex_Layout_Component_T'Tag)
            );

            Flex_C.Is_Dirty := True;
         end;
      end loop;
   end Mark_All_Flex_Dirty;

   --===========================================================================
   -- SYSTEM: FLEXBOX LAYOUT
   --===========================================================================

   procedure FlexLayoutSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Flex_Layout_Component_T'Tag &
        Widget_Component_T'Tag;
      Matched_Entities     : Entity_ID_Vector.Vector;

      Parent_Comps         : Components_Ptr;

      Child_Comps          : Components_Ptr;
      Child_Id             : Entity_Id;

      Child_Pos_Mode       : Position_Mode_Component_T;
      Skip_Child           : Boolean;

      Calc_X, Calc_Y, Calc_W, Calc_H : Integer;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for Parent_EID of Matched_Entities loop
         Parent_Comps := Get_Entity_Components (Entity_List.all, Parent_EID);
         declare
            Flex_C : Flex_Layout_Component_T renames Flex_Layout_Component_T (
              Get_Component_Ptr (Parent_Comps, Flex_Layout_Component_T'Tag).all);
            Parent_Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Parent_Comps, Widget_Component_T'Tag).all);
         begin

            Flex_C.Flex_Container.Width := Integer (Parent_Widget_C.Size_Width);
            Flex_C.Flex_Container.Height := Integer (Parent_Widget_C.Size_Height);

            if Flex_C.Is_Dirty then
               Flexbox.Layout (Flex_C.Flex_Container);
               Flex_C.Is_Dirty := False;
            end if;

            if Flex_C.Flex_Container.Items /= null then
               for I in 1 .. Flex_C.Flex_Container.Item_Count loop

                  Child_Id := Flex_C.Flex_Container.Items (I).Related_Entity;
                  Child_Comps := Get_Entity_Components (Entity_List.all, Child_Id);

                  if Child_Comps /= null and then
                    Has_Component (Child_Comps.all, Widget_Component_T'Tag) then

                     Skip_Child := False;

                     if Has_Component (Child_Comps.all, Position_Mode_Component_T'Tag) then
                        Child_Pos_Mode := Position_Mode_Component_T (
                           Get_Component (Child_Comps.all, Position_Mode_Component_T'Tag)
                        );

                        if Child_Pos_Mode.Mode /= Flex then
                           Skip_Child := True;
                        end if;
                     end if;

                     if not Skip_Child then
                        declare
                           Child_Widget_C : Widget_Component_T renames Widget_Component_T (
                             Get_Component_Ptr (Child_Comps, Widget_Component_T'Tag).all);
                        begin

                        Calc_X := Integer(Parent_Widget_C.Position_X) +
                          Flex_C.Flex_Container.Items(I).Position_X;
                        Calc_Y := Integer(Parent_Widget_C.Position_Y) +
                          Flex_C.Flex_Container.Items(I).Position_Y;

                        Child_Widget_C.Position_X := TUI_Width(Calc_X);
                        Child_Widget_C.Position_Y := TUI_Height(Calc_Y);

                        if Flex_C.Flex_Container.Direction = Row then
                           Calc_W := Integer'Max(1, Flex_C.Flex_Container.Items(I).Computed_Size);
                           Calc_H := Integer'Max(1, Flex_C.Flex_Container.Items(I).Cross_Size);
                        else
                           Calc_H := Integer'Max(1, Flex_C.Flex_Container.Items(I).Computed_Size);
                           Calc_W := Integer'Max(1, Flex_C.Flex_Container.Items(I).Cross_Size);
                        end if;

                        Child_Widget_C.Size_Width := TUI_Width(Calc_W);
                        Child_Widget_C.Size_Height := TUI_Height(Calc_H);
                        end;
                     end if;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end FlexLayoutSystem;

   --===========================================================================
   -- SYSTEM: WIDGET BACKGROUND RENDERING
   --===========================================================================

   procedure WidgetBackgroundSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Widget_Component_T'Tag &
        Background_Color_Component_T'Tag;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      BGColor : Color_t;
      Px : Pixel_t;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);
         declare
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
            BGColor_C : Background_Color_Component_T renames Background_Color_Component_T (
              Get_Component_Ptr (Component_List, Background_Color_Component_T'Tag).all);
         begin
            BGColor := BGColor_C.Background_Color;

            for Pos_W in TUI_Width'First .. Widget_C.Size_Width loop
               for Pos_H in TUI_Height'First .. Widget_C.Size_Height loop
                  Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
                  Px.Char := ' ';
                  Px.Background_Color := BGColor;
                  Set_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H, Px);
               end loop;
            end loop;

            --  Focus indicator: asterisk at top-left when widget is focused and selectable
            if Widget_C.Has_Focus
              and then Has_Component (Component_List.all, Selectable_Component_T'Tag)
            then
               Px := Get_Buffer_Pixel (Widget_C.Render_Buffer,
                                       TUI_Width'First, TUI_Height'First);
               Px.Char := '*';
               Set_Buffer_Pixel (Widget_C.Render_Buffer,
                                 TUI_Width'First, TUI_Height'First, Px);
            end if;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end WidgetBackgroundSystem;

   --===========================================================================
   -- SYSTEM: TEXT RENDERING
   --===========================================================================

   procedure TextRenderSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Widget_Component_T'Tag &
        Text_Component_T'Tag;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Pos_W : TUI_Width;
      Pos_H : TUI_Height;
      Text : SU.Unbounded_String;
      Char : Character;
      Px : Pixel_t;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);

         declare
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);

            Text_C : Text_Component_T renames Text_Component_T (
              Get_Component_Ptr (Component_List, Text_Component_T'Tag).all);
         begin
            Text := Text_C.Text;

            Pos_W := Text_C.Offset_X;
            Pos_H := Text_C.Offset_Y;

            for Text_Index in Positive'First .. SU.Length(Text) loop
               Char := SU.Element (Text, Text_Index);
               Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
               Px.Char := Char;
               Px.Char_Color := Text_C.Text_Color;

               Px.Is_Bold           := Text_C.Is_Bold;
               Px.Is_Italic         := Text_C.Is_Italic;
               Px.Is_Underline      := Text_C.Is_Underline;
               Px.Is_Strikethrough  := Text_C.Is_Strikethrough;

               Set_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H, Px);

               Pos_W := Pos_W + 1;
               if Pos_W > Widget_C.Size_Width then
                  Pos_W := 1;
                  Pos_H := Pos_H + 1;
               end if;
               exit when Pos_H > Widget_C.Size_Height;
            end loop;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end TextRenderSystem;

   --===========================================================================
   -- SYSTEM: PROGRESS BAR RENDERING
   --===========================================================================

   procedure ProgressBarRenderSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List           : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
                             Widget_Component_T'Tag &
                             Progress_Bar_Component_T'Tag;
      Matched_Entities      : Entity_ID_Vector.Vector;
      Comp_Ptr              : Components_Ptr;
      BG_C                  : Background_Color_Component_T;
      Px                    : Pixel_t;
      Bar_Width             : Natural;
      Filled_Cells          : Natural;
      Percent               : Natural;
      Percent_Str           : String (1 .. 4);
      Pos_Index             : Natural;
      Current_Char          : Character;
      Has_BG                : Boolean;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Comp_Ptr := Get_Entity_Components (Entity_List.all, EID);

         declare
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Comp_Ptr, Widget_Component_T'Tag).all);
            PB_C : Progress_Bar_Component_T renames Progress_Bar_Component_T (
              Get_Component_Ptr (Comp_Ptr, Progress_Bar_Component_T'Tag).all);
         begin

            Has_BG := Has_Component (Comp_Ptr.all, Background_Color_Component_T'Tag);
            if Has_BG then
               BG_C := Background_Color_Component_T (
                  Get_Component (Comp_Ptr.all, Background_Color_Component_T'Tag));
            end if;

            if PB_C.Show_Percentage then
               if Natural (Widget_C.Size_Width) > 7 then
                  Bar_Width := Natural (Widget_C.Size_Width) - 7;
               else
                  Bar_Width := 1;
               end if;
            else
               if Natural (Widget_C.Size_Width) > 2 then
                  Bar_Width := Natural (Widget_C.Size_Width) - 2;
               else
                  Bar_Width := 1;
               end if;
            end if;

            Filled_Cells := Natural (PB_C.Value * Float (Bar_Width));
            if Filled_Cells > Bar_Width then
               Filled_Cells := Bar_Width;
            end if;

            Percent := Natural (PB_C.Value * 100.0);
            if Percent > 100 then
               Percent := 100;
            end if;

            declare
               Pct_Img : constant String := Natural'Image (Percent);
            begin
               if Percent < 10 then
                  Percent_Str := "  " & Pct_Img (Pct_Img'Last) & "%";
               elsif Percent < 100 then
                  Percent_Str := " " & Pct_Img (Pct_Img'First + 1 .. Pct_Img'Last) & "%";
               else
                  Percent_Str := Pct_Img (Pct_Img'First + 1 .. Pct_Img'Last) & "%";
               end if;
            end;

            Pos_Index := 0;
            for X in TUI_Width'First .. Widget_C.Size_Width loop
               Pos_Index := Pos_Index + 1;
               Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, X, TUI_Height'First);

               if Has_BG then
                  Px.Background_Color := BG_C.Background_Color;
               end if;

               if Pos_Index = 1 then
                  Current_Char := PB_C.Border_Left;
                  Px.Char_Color := White;
               elsif Pos_Index = Natural (Widget_C.Size_Width) - 4 and PB_C.Show_Percentage then
                  Current_Char := ' ';
                  Px.Char_Color := White;
               elsif Pos_Index > Natural (Widget_C.Size_Width) - 4 and PB_C.Show_Percentage then
                  declare
                     Pct_Pos : constant Natural := Pos_Index - (Natural (Widget_C.Size_Width) - 4);
                  begin
                     if Pct_Pos <= 4 then
                        Current_Char := Percent_Str (Pct_Pos);
                     else
                        Current_Char := ' ';
                     end if;
                  end;
                  Px.Char_Color := White;
               elsif Pos_Index = Natural (Widget_C.Size_Width) - 5 + 1 and not PB_C.Show_Percentage then
                  Current_Char := PB_C.Border_Right;
                  Px.Char_Color := White;
               elsif Pos_Index = Bar_Width + 2 then
                  Current_Char := PB_C.Border_Right;
                  Px.Char_Color := White;
               elsif Pos_Index > 1 and Pos_Index <= Bar_Width + 1 then
                  declare
                     Bar_Pos : constant Natural := Pos_Index - 1;
                  begin
                     if Bar_Pos <= Filled_Cells then
                        Current_Char := PB_C.Filled_Char;
                        Px.Char_Color := PB_C.Filled_Color;
                     else
                        Current_Char := PB_C.Empty_Char;
                        Px.Char_Color := PB_C.Empty_Color;
                     end if;
                  end;
               else
                  Current_Char := ' ';
                  Px.Char_Color := White;
               end if;

               Px.Char := Current_Char;
               Set_Buffer_Pixel (Widget_C.Render_Buffer, X, TUI_Height'First, Px);
            end loop;

            if Widget_C.Size_Height > TUI_Height'First then
               for Y in TUI_Height'First + 1 .. Widget_C.Size_Height loop
                  for X in TUI_Width'First .. Widget_C.Size_Width loop
                     Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, X, Y);
                     Px.Char := ' ';
                     if Has_BG then
                        Px.Background_Color := BG_C.Background_Color;
                     end if;
                     Set_Buffer_Pixel (Widget_C.Render_Buffer, X, Y, Px);
                  end loop;
               end loop;
            end if;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end ProgressBarRenderSystem;

   --===========================================================================
   -- SYSTEM: BUFFER COPY (COMPOSITION)
   --===========================================================================

   procedure BufferCopySystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;

      procedure RecursiveBufferCopy (Framebuffer : in out Buffer_T;
                                     Root : Widget_Component_T;
                                     Parent : Widget_Component_T) is
         Child_Component_List : Components_Ptr;
         Root_Left, Root_Right, Parent_X : TUI_Width;
         Root_Top, Root_Bottom, Parent_Y : TUI_Height;
      begin
         Root_Left := Root.Position_X;
         Root_Right := Root.Position_X + Root.Size_Width - TUI_Width (1);
         Root_Top := Root.Position_Y;
         Root_Bottom := Root.Position_Y + Root.Size_Height - TUI_Height (1);

         for Pos_W in TUI_Width'First .. Parent.Size_Width loop
            for Pos_H in TUI_Height'First .. Parent.Size_Height loop
               Parent_X := Parent.Position_X + Pos_W - TUI_Width (1);
               Parent_Y := Parent.Position_Y + Pos_H - TUI_Height (1);

               if (Parent_X < Root_Left) or
                 (Parent_X > Root_Right) or
                 (Parent_Y < Root_Top) or
                 (Parent_Y > Root_Bottom) then
                  exit;
               end if;

               Set_Buffer_Pixel (
                  Framebuffer,
                  Parent_X, Parent_Y,
                  Get_Buffer_Pixel (Parent.Render_Buffer, Pos_W, Pos_H)
                         );
            end loop;
         end loop;

         for Child_Entity_ID of Parent.Children loop
            Child_Component_List := Get_Entity_Components (
               Entity_List.all, Child_Entity_ID
                                                          );
            declare
               Child_Widget : Widget_Component_T renames Widget_Component_T (
                  Get_Component_Ptr (Child_Component_List, Widget_Component_T'Tag).all);
            begin
               RecursiveBufferCopy (Framebuffer, Parent, Child_Widget);
            end;
         end loop;
      end RecursiveBufferCopy;

      RI_Component_Tags : constant Component_Tag_Vector.Vector :=
        Component_Tag_Vector.To_Vector (Render_Info_Component_T'Tag, 1);
      Root_Component_Tags : constant Component_Tag_Vector.Vector :=
        Root_Widget_Component_T'Tag &
        Widget_Component_T'Tag;
      Matched_RIs : Entity_ID_Vector.Vector;
      Matched_Roots : Entity_ID_Vector.Vector;
      RI_Components : Components_Ptr;
      Root_Components : Components_Ptr;
      Rendering_To_FB_2 : Boolean;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_RIs := Get_Entities_Matching (Entity_List.all, RI_Component_Tags);
      Matched_Roots := Get_Entities_Matching (Entity_List.all, Root_Component_Tags);

      for RI_Entity_ID of Matched_RIs loop
         RI_Components := Get_Entity_Components (Entity_List.all, RI_Entity_ID);
         declare
            RenderInfo_C : Render_Info_Component_T renames Render_Info_Component_T (
              Get_Component_Ptr (RI_Components, Render_Info_Component_T'Tag).all);
         begin
            RenderInfo_C.Drawing_FB.all.Read (Rendering_To_FB_2);

            for R_Entity_ID of Matched_Roots loop
               Root_Components := Get_Entity_Components (Entity_List.all, R_Entity_ID);
               declare
                  Root : Widget_Component_T renames Widget_Component_T (
                    Get_Component_Ptr (Root_Components, Widget_Component_T'Tag).all);
               begin

                  if Rendering_To_FB_2 then
                     RecursiveBufferCopy (RenderInfo_C.Framebuffer_2, Root, Root);
                  else
                     RecursiveBufferCopy (RenderInfo_C.Framebuffer_1, Root, Root);
                  end if;
               end;
            end loop;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end BufferCopySystem;

   --===========================================================================
   -- SYSTEM: BUFFER DRAW (TERMINAL OUTPUT)
   --===========================================================================

   procedure BufferDrawSystem (Entity_List_PO : in out Entity_Components_PO) is

      package GFX renames Graphics;

      function Trim (S : String) return String is (S (S'First + 1 .. S'Last));
      function FG (P : Pixel_t) return String is
        (GFX.CSI & "38;2;" & Trim (P.Char_Color.Red'Image) & ";"
             & Trim (P.Char_Color.Green'Image) & ";"
             & Trim (P.Char_Color.Blue'Image) & "m");
      function BG (P : Pixel_t) return String is
        (GFX.CSI & "48;2;" & Trim (P.Background_Color.Red'Image) & ";"
             & Trim (P.Background_Color.Green'Image) & ";"
             & Trim (P.Background_Color.Blue'Image) & "m");
      function Bold (P : Pixel_t) return String is
        (GFX.CSI & (if P.Is_Bold then "1m" else "22m"));
      function Italic (P : Pixel_t) return String is
        (GFX.CSI & (if P.Is_Italic then "3m" else "23m"));
      function Underline (P : Pixel_t) return String is
        (GFX.CSI & (if P.Is_Underline then "4m" else "24m"));
      function Strikethrough (P : Pixel_t) return String is
        (GFX.CSI & (if P.Is_Strikethrough then "9m" else "29m"));
      function Move (Row : TUI_Height; Col : TUI_Width) return String is
        (GFX.CSI & Trim (Row'Image) & ";" & Trim (Col'Image) & "H");
      Reset : constant String := GFX.CSI & "0m";
      Entity_List : Entity_Components_Ptr;
      Search_Components : constant Component_Tag_Vector.Vector :=
        Component_Tag_Vector.To_Vector (Render_Info_Component_T'Tag, 1);
      Matched_Entities : Entity_ID_Vector.Vector;
      RI_Component_List : Components_Ptr;
      FB_Pixel : Pixel_t;
      Drawing_From_FB_1 : Boolean;

      type Drawing_Ptr is access all Buffer_T;
      Drawing : Drawing_Ptr;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Components);

      -- PROTECTED RENDER LOOP
      for EID of Matched_Entities loop
         RI_Component_List := Get_Entity_Components (Entity_List.all, EID);
         declare
            --  Obtain a view to the component allowing direct modification
            RI : Render_Info_Component_T renames
              Render_Info_Component_T
                (Get_Component_Ptr
                   (RI_Component_List, Render_Info_Component_T'Tag).all);
         begin
            RI.Drawing_FB.all.Wait (Drawing_From_FB_1);
            -- Change Drawing to point to the correct framebuffer. For Skye if you want see if it can work with protected object fields.
            Drawing :=
              (if Drawing_From_FB_1
               then RI.Framebuffer_1'Unchecked_Access
               else RI.Framebuffer_2'Unchecked_Access);

            --  Begin comparing FB to BB and drawing
            --  Batched output with differential cursor/format tracking
            declare
               Frame_Output : SU.Unbounded_String;
               --  Cursor position tracking (skip Move for consecutive pixels)
               Last_X : TUI_Width := TUI_Width'First;
               Last_Y : TUI_Height := TUI_Height'First;
               First_Pixel : Boolean := True;
               --  Format state tracking (skip unchanged ANSI codes)
               Cur_FG     : Color_t := (0, 0, 0);
               Cur_BG     : Color_t := (0, 0, 0);
               Cur_Bold   : Boolean := False;
               Cur_Italic : Boolean := False;
               Cur_ULine  : Boolean := False;
               Cur_Strike : Boolean := False;
               Fmt_Set    : Boolean := False;
            begin
               for Y in TUI_Height'First .. RI.Terminal_Height loop
                  for X in TUI_Width'First .. RI.Terminal_Width loop
                     if Get_Buffer_Pixel (Drawing.all, X, Y)
                       /= Get_Buffer_Pixel (RI.Backbuffer, X, Y)
                     then
                        FB_Pixel := Get_Buffer_Pixel (Drawing.all, X, Y);

                        --  Only move cursor when not at expected position
                        if First_Pixel or else Y /= Last_Y
                           or else Integer (X) /= Integer (Last_X) + 1
                        then
                           SU.Append (Frame_Output, Move (Y, X));
                        end if;

                        --  Only emit format codes that differ from current state
                        if not Fmt_Set or else FB_Pixel.Char_Color /= Cur_FG then
                           SU.Append (Frame_Output, FG (FB_Pixel));
                           Cur_FG := FB_Pixel.Char_Color;
                        end if;
                        if not Fmt_Set or else FB_Pixel.Background_Color /= Cur_BG then
                           SU.Append (Frame_Output, BG (FB_Pixel));
                           Cur_BG := FB_Pixel.Background_Color;
                        end if;
                        if not Fmt_Set or else FB_Pixel.Is_Bold /= Cur_Bold then
                           SU.Append (Frame_Output, Bold (FB_Pixel));
                           Cur_Bold := FB_Pixel.Is_Bold;
                        end if;
                        if not Fmt_Set or else FB_Pixel.Is_Italic /= Cur_Italic then
                           SU.Append (Frame_Output, Italic (FB_Pixel));
                           Cur_Italic := FB_Pixel.Is_Italic;
                        end if;
                        if not Fmt_Set or else FB_Pixel.Is_Underline /= Cur_ULine then
                           SU.Append (Frame_Output, Underline (FB_Pixel));
                           Cur_ULine := FB_Pixel.Is_Underline;
                        end if;
                        if not Fmt_Set or else FB_Pixel.Is_Strikethrough /= Cur_Strike then
                           SU.Append (Frame_Output, Strikethrough (FB_Pixel));
                           Cur_Strike := FB_Pixel.Is_Strikethrough;
                        end if;
                        Fmt_Set := True;

                        --  Emit character (no per-pixel Reset)
                        SU.Append (Frame_Output, "" & FB_Pixel.Char);

                        Last_X := X;
                        Last_Y := Y;
                        First_Pixel := False;

                        Set_Buffer_Pixel (RI.Backbuffer, X, Y, FB_Pixel);
                     end if;
                  end loop;
               end loop;

               if SU.Length (Frame_Output) > 0 then
                  SU.Append (Frame_Output, Reset);
                  Ada.Text_IO.Put (SU.To_String (Frame_Output));
               end if;
            end;

            --  Release RenderInfo
            RI.Drawing_FB.all.Post;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end BufferDrawSystem;

   --===========================================================================
   -- SYSTEM: DOUBLE BUFFER SWAP
   --===========================================================================

   procedure DoubleBufferFlagSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Component_Tag_Vector.To_Vector (Render_Info_Component_T'Tag, 1);
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Drawing_From_FB_1 : Boolean;
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);

         declare
            Render_Info : Render_Info_Component_T renames Render_Info_Component_T (
              Get_Component_Ptr (Component_List, Render_Info_Component_T'Tag).all);
         begin
            Render_Info.Drawing_FB.all.Wait (Drawing_From_FB_1);
            Render_Info.Drawing_FB.all.Swap;
            Render_Info.Drawing_FB.all.Post;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end DoubleBufferFlagSystem;

   --===========================================================================
   -- SYSTEM: SELECTION (TAB CYCLING)
   --===========================================================================

   procedure SelectionSystem (Entity_List_PO : in out Entity_Components_PO;
                              Tab_Pressed : in Boolean) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Widget_Component_T'Tag &
        Selectable_Component_T'Tag;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;

      --  Simple sorted list of selectable entities
      Max_Selectables : constant := 64;
      type Selectable_Info is record
         EID   : Entity_Id;
         Order : Natural;
      end record;
      Selectables : array (1 .. Max_Selectables) of Selectable_Info;
      Count : Natural := 0;
      Current_Focus : Natural := 0;

      --  Temp for insertion sort
      Temp : Selectable_Info;
      J    : Natural;
   begin
      if not Tab_Pressed then
         return;
      end if;

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      --  Build list of enabled selectable entities
      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);
         declare
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
            Sel_C : Selectable_Component_T renames Selectable_Component_T (
              Get_Component_Ptr (Component_List, Selectable_Component_T'Tag).all);
         begin
            if Widget_C.Is_Enabled and Count < Max_Selectables then
               Count := Count + 1;
               Selectables (Count) := (EID => EID, Order => Sel_C.Tab_Order);
               if Widget_C.Has_Focus then
                  Current_Focus := Count;
               end if;
            end if;
         end;
      end loop;

      --  Sort by Tab_Order (insertion sort, small N)
      for I in 2 .. Count loop
         Temp := Selectables (I);
         J := I - 1;
         while J >= 1 and then Selectables (J).Order > Temp.Order loop
            Selectables (J + 1) := Selectables (J);
            J := J - 1;
         end loop;
         Selectables (J + 1) := Temp;
      end loop;

      --  Find Current_Focus in sorted order (index may have shifted)
      Current_Focus := 0;
      for I in 1 .. Count loop
         Component_List := Get_Entity_Components (Entity_List.all, Selectables (I).EID);
         declare
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
         begin
            if Widget_C.Has_Focus then
               Current_Focus := I;
            end if;
         end;
      end loop;

      if Count = 0 then
         Entity_List_PO.Release_Reading;
         return;
      end if;

      --  Compute next focus index
      declare
         Next_Focus : Natural;
      begin
         if Current_Focus = 0 or Current_Focus >= Count then
            Next_Focus := 1;
         else
            Next_Focus := Current_Focus + 1;
         end if;

         --  Clear all Has_Focus, then set the next one
         for I in 1 .. Count loop
            Component_List := Get_Entity_Components (Entity_List.all, Selectables (I).EID);
            declare
               Widget_C : Widget_Component_T renames Widget_Component_T (
                 Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
            begin
               Widget_C.Has_Focus := (I = Next_Focus);
            end;
         end loop;

         --  Swap widget command table based on new focus
         Component_List := Get_Entity_Components (Entity_List.all, Selectables (Next_Focus).EID);
         if Has_Component (Component_List.all, Command_Set_Component_T'Tag) then
            declare
               Cmd_Set : Command_Set_Component_T renames Command_Set_Component_T (
                 Get_Component_Ptr (Component_List, Command_Set_Component_T'Tag).all);
            begin
               Selection.Activate_Widget_Commands (Cmd_Set.Commands);
            end;
         else
            Selection.Deactivate_Widget_Commands;
         end if;
      end;

      Entity_List_PO.Release_Reading;
   end SelectionSystem;

   --===========================================================================
   -- HELPER: WIDGET POSITIONING
   --===========================================================================

   procedure Move_Widget (Entity_List : in out Entity_Components;
                          Widget_Entity : Entity_Id;
                          New_X : TUI_Width;
                          New_Y : TUI_Height) is
      Comps     : Components_Ptr;
      Pos_Mode  : Position_Mode_Component_T;
   begin
      Comps := Get_Entity_Components (Entity_List, Widget_Entity);

      if Comps = null then
         return;
      end if;

      if not Has_Component (Comps.all, Widget_Component_T'Tag) then
         return;
      end if;

      Pos_Mode.Mode := Absolute;
      Add_Component (Comps.all, To_CID ("PositionMode"), Pos_Mode);

      declare
         Widget : Widget_Component_T renames Widget_Component_T (
            Get_Component_Ptr (Comps, Widget_Component_T'Tag).all);
      begin
         Widget.Position_X := New_X;
         Widget.Position_Y := New_Y;
      end;
   end Move_Widget;

   procedure Move_Widget_By (Entity_List : in out Entity_Components;
                             Widget_Entity : Entity_Id;
                             Delta_X : Integer;
                             Delta_Y : Integer) is
      Comps : Components_Ptr;
      New_X : Integer;
      New_Y : Integer;
   begin
      Comps := Get_Entity_Components (Entity_List, Widget_Entity);

      if Comps = null then
         return;
      end if;

      if not Has_Component (Comps.all, Widget_Component_T'Tag) then
         return;
      end if;

      declare
         Widget : Widget_Component_T renames Widget_Component_T (
            Get_Component_Ptr (Comps, Widget_Component_T'Tag).all);
      begin
         New_X := Integer(Widget.Position_X) + Delta_X;
         New_Y := Integer(Widget.Position_Y) + Delta_Y;
      end;

      New_X := Integer'Max(Integer(TUI_Width'First), New_X);
      New_Y := Integer'Max(Integer(TUI_Height'First), New_Y);

      Move_Widget (Entity_List, Widget_Entity, TUI_Width(New_X), TUI_Height(New_Y));
   end Move_Widget_By;

   procedure CalendarDisplaySystem (Entity_List_PO : in out Entity_Components_PO) is
      function Trim (S : String) return String is (
         S (S'First + 1 .. S'Last)
      );
      function Pad (S : String; C : Character := '0') return String is (
         (if S'Length = 1 then String (C) else "") & S
      );
      function Weekday_Pad (S : String; N : in out Natural) return String is
      begin
         N := N + 1;
         if N mod 7 = 0 then
            return S;
         else
            return S & " ";
         end if;
      end Weekday_Pad;
      Entity_List : Entity_Components_Ptr;
      --  Search for widgets with text and calendar components
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Widget_Component_T'Tag &
        Text_Component_T'Tag &
        Calendar_Component_T'Tag;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;

      --  Values to strings
      Weekdays : constant array (Natural) of String := ["Sunday",
                                                        "Monday",
                                                        "Tuesday",
                                                        "Wednesday",
                                                        "Thursday",
                                                        "Friday",
                                                        "Saturday"];
      Months : constant array (Natural) of String := ["Jan", "Feb", "Mar",
                                                      "Apr", "May", "Jun",
                                                      "Jul", "Aug", "Sep",
                                                      "Oct", "Nov", "Dec"];

      --  Values for Rata Die calculation of weekdays
      --  Earliest date possible with Ada.Calendar.Time
      Rata_Die_Date : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1901, 1, 1);
      --  Weekday range 0-6, Sunday is 0
      Rata_Die_Weekday : constant Natural := 2; --  Tuesday, Jan. 1, 1901
   begin

      --  Wait for inclusive lock for entity list
      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);
         declare
            --  Obtain views to the components
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, Widget_Component_T'Tag).all);
            Text_C : Text_Component_T renames Text_Component_T (
              Get_Component_Ptr (Component_List, Text_Component_T'Tag).all);
            Calendar_C : Calendar_Component_T renames Calendar_Component_T (
              Get_Component_Ptr (Component_List, Calendar_Component_T'Tag).all);

            Text_Width : constant Integer := Integer (Widget_C.Size_Width - Text_C.Offset_X) + 1;
            Text_Height : constant Integer := Integer (Widget_C.Size_Height - Text_C.Offset_Y) + 1;
            Date_Weekday : constant Natural := Natural (
              (Ada.Calendar.Time_Of (Calendar_C.Year,
                                     Calendar_C.Month,
                                     Calendar_C.Day
                                    ) - Rata_Die_Date) / 86400) mod 7 + Rata_Die_Weekday;
            Weekday : String;
         begin

            if Calendar_C.Display_Mode = Month_Page
              and Text_Width >= 20 and Text_Height >= 8 then
               --  Month_Page mode selected and enough size for it

               --  Make Date_Weekday for the first of the month
               Date_Weekday := (Date_Weekday - Calendar_C.Day + 1) mod 7;

               --  Year, row 1, left aligned
               Text_C.Text := SU.To_Unbounded_String (Trim (Calendar_C.Year'Image));
               --  Padding. 7 = length of year + length of month abbreviation
               Text_C.Text := Text_C.Text & (" " * (Text_Width - 7));
               --  Month, row 1, right aligned
               Text_C.Text := Text_C.Text * Months (Natural (Calendar_C.Month) - 1);

               --  Row 2, spacer
               Text_C.Text := Text_C.Text * ("-" * Text_Width);

               --  Row 3, weekday abbreviations
               for Weekday_I in Weekdays'First .. Weekdays'Last loop
                  Weekday := Weekdays (Weekday_I);
                  Weekday := Weekday (Weekday'First .. Weekday'First + 1);
                  Text_C.Text := Text_C.Text & Weekday;
                  if Weekday_I /= Weekdays'Last then
                     Text_C.Text := Text_C.Text & " ";
                  end if;
               end loop;

               --  Month-day section
               declare
                  Weekday_Pos : Natural := 0;
                  Month_End : constant Ada.Calendar.Time :=
                    (if Calendar_C.Month = 12 then
                      Ada.Calendar.Time_Of (Calendar_C.Year + 1, 1, 1)
                     else
                      Ada.Calendar.Time_Of (Calendar_C.Year, Calendar_C.Month, 1)
                    ) - Duration (1 * 24 * 60 * 60);
                  Day_Count : constant Ada.Calendar.Day_Number := Month_End.Day;
               begin
                  --  Row 4, padding to align month days to weekdays
                  for Padding_Index in 1 .. Date_Weekday loop
                     Text_C.Text := Text_C.Text & Weekday_Pad ("  ", Weekday_Pos);
                  end loop;

                  --  Rows 4-8, month days
                  for Month_Day in 1 .. Natural (Day_Count) loop
                     Text_C.Text := Text_C.Text * Weekday_Pad (Pad (Trim (Month_Day'Image), ' '), Weekday_Pos);
                  end loop;
               end;
            else
               --  Not enough size or Date_String mode selected

               --  Initial string section
               Text_C.Text := SU.To_Unbounded_String (
                 Pad (Trim (Calendar_C.Year'Image) & "/") &
                 Pad (Trim (Calendar_C.Month'Image) & "/") &
                 Pad (Trim (Calendar_C.Day'Image) & ", "));

               --  Weekday
               Text_C.Text := Text_C.Text & Weekdays (Date_Weekday);
            end if;
         end;
      end loop;

      --  Release lock on entity list
      Entity_List_PO.Release_Reading;
   end CalendarDisplaySystem;

end ECS;
