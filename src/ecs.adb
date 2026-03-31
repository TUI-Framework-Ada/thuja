--==============================================================================
-- ECS.ADB - Entity Component System Implementation
--==============================================================================

with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Characters.Conversions;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Flexbox; use Flexbox;
with Graphics;
with IDs; use type IDs.Component_Tag_Vector.Vector;
with Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Ada.Wide_Wide_Text_IO;
with Selection;

package body ECS is

   package SU renames Ada.Strings.Unbounded;

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

   -- NEW
   function Make_Widget -- Function to create a widget entity with basic components
      (
      -- World is the container that holds all entities and their components. The widget will be added to this world.
      World : in out Entity_Components_PO; -- The world to which the widget will be added
      Name : String; -- Name of the widget
      x : TUI_Width; y : TUI_Height; -- x/y position of the widget
      W : TUI_Width; H : TUI_Height -- width/height of the widget

      ) 
      return Components_Ptr is

      CP : constant Components_Ptr := Add_Entity (World, To_EID (Name)); -- Makes the widget an entity in the world and gets a pointer to its components
      WC : Widget_Component_T; -- Create a widget component to hold the widget's properties
   begin
      WC.Position_X := x; -- Set the widget's x position
      WC.Position_Y := y; -- Set the widget's y position
      WC.Size_Width := W; -- Set the widget's width
      WC.Size_Height := H; -- Set the widget's height
      WC.Has_Focus := False; -- By default, the widget does not have focus
      WC.Render_Buffer := Create_Buffer (W, H); -- Create a render buffer for the widget based on its size

      Add_Component (CP.all, To_CID ("WidgetComponent"), WC);

      return CP; -- Return the pointer to the entity's components
   end Make_Widget;


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
               if Char = Character'Val (16#09#) then --  \t
                  --  Find next nearest index moduloing by 4 to 1
                  declare
                     Dist_To_Next_Tab : constant Natural := 4 - (Natural (Pos_W) - 1) mod 4;
                     Tab_End_Index : constant Positive := Positive (Pos_W) + Dist_To_Next_Tab;
                     End_On_New_Line : constant Boolean := Tab_End_Index > Positive (TUI_Width'Last)
                       or else TUI_Width (Tab_End_Index) > Widget_C.Size_Width;
                     Post_Loop_Index : constant TUI_Width := TUI_Width'Min ((if End_On_New_Line
                       then TUI_Width'Last
                       else TUI_Width (Tab_End_Index)), Widget_C.Size_Width);
                     Loop_Last_Index : constant TUI_Width := (
                       if End_On_New_Line
                         then Post_Loop_Index
                         else (if Post_Loop_Index /= TUI_Width'First
                           then Post_Loop_Index - 1
                           else Post_Loop_Index));
                  begin
                     for Space_Index in Pos_W .. Loop_Last_Index loop
                        Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Space_Index, Pos_H);
                        Px.Char := ' ';
                        Px.Char_Color := Text_C.Text_Color;

                        Px.Is_Underline     := Text_C.Is_Underline;
                        Px.Is_Strikethrough := Text_C.Is_Strikethrough;

                        Set_Buffer_Pixel (Widget_C.Render_Buffer, Space_Index, Pos_H, Px);
                     end loop;

                     Pos_W := Loop_Last_Index;
                  end;
               elsif Char = Character'Val (16#0A#) then --  \n
                  declare
                     Loop_Last_Index : constant TUI_Width := TUI_Width'Min (TUI_Width'Last, Widget_C.Size_Width);
                  begin
                     for Space_Index in Pos_W .. Loop_Last_Index loop
                        Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Space_Index, Pos_H);
                        Px.Char := ' ';
                        Px.Char_Color := Text_C.Text_Color;

                        Px.Is_Underline     := Text_C.Is_Underline;
                        Px.Is_Strikethrough := Text_C.Is_Strikethrough;

                        Set_Buffer_Pixel (Widget_C.Render_Buffer, Space_Index, Pos_H, Px);
                     end loop;

                     Pos_W := Loop_Last_Index;
                  end;
               else
                  Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
                  Px.Char := Char;
                  Px.Char_Color := Text_C.Text_Color;

                  Px.Is_Bold           := Text_C.Is_Bold;
                  Px.Is_Italic         := Text_C.Is_Italic;
                  Px.Is_Underline      := Text_C.Is_Underline;
                  Px.Is_Strikethrough  := Text_C.Is_Strikethrough;

                  Set_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H, Px);
               end if;

               if Pos_W = TUI_Width'Last or Pos_W >= Widget_C.Size_Width then
                  Pos_W := Text_C.Offset_X;
                  Pos_H := Pos_H + 1;
               else
                  Pos_W := Pos_W + 1;
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
      Framebuffer_Index : Framebuffer_Index_t;
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
            Framebuffer_Index := RenderInfo_C.Drawing_FB.all.Back;

            for R_Entity_ID of Matched_Roots loop
               Root_Components := Get_Entity_Components (Entity_List.all, R_Entity_ID);
               declare
                  Root : Widget_Component_T renames Widget_Component_T (
                    Get_Component_Ptr (Root_Components, Widget_Component_T'Tag).all);
               begin

                  RecursiveBufferCopy (RenderInfo_C.Buffers (Framebuffer_Index), Root, Root);
               end;
            end loop;
         end;
      end loop;

      Entity_List_PO.Release_Reading;
   end BufferCopySystem;

   --===========================================================================
   -- SYSTEM: BUFFER DRAW (TERMINAL OUTPUT)
   --===========================================================================

   --  TODO: Undo stateful optimization, replace with separated checking,
   --    string conversion, and printing (the hot loop issue)
   --  TODO: Compare the current framebuffer against the other one & remove
   --    mentions to backbuffer (backbuffer is unneeded and should eventually
   --    be removed)

   procedure BufferDrawSystem (Entity_List_PO : in out Entity_Components_PO) is

      package GFX renames Graphics;

      function Move (Row : TUI_Height; Col : TUI_Width) return String is
        (GFX.CSI & GFX.Trim (Row'Image) & ";" & GFX.Trim (Col'Image) & "H");

      Entity_List : Entity_Components_Ptr;
      Search_Components : constant Component_Tag_Vector.Vector :=
        Component_Tag_Vector.To_Vector (Render_Info_Component_T'Tag, 1);
      Matched_Entities : Entity_ID_Vector.Vector;
      RI_Component_List : Components_Ptr;

      Frontbuffer_Index : Framebuffer_Index_t;
      Backbuffer_Index : Framebuffer_Index_t;

      type PosPixel_t is record
         X : TUI_Width;
         Y : TUI_Height;
         P : Pixel_t;
      end record;
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
            type Flat_Buffer_t is range 1 .. Positive (TUI_Width'Last) * Positive(TUI_Height'Last);
            All_Pixels : array (Flat_Buffer_t) of PosPixel_t;
            Updated_Pixels : array (Flat_Buffer_t) of PosPixel_t;
            All_Pixels_Length : Natural := 0;
            Updated_Pixels_Length : Natural := 0;
         begin
            RI.Drawing_FB.all.Start_Draw;
            Frontbuffer_Index := RI.Drawing_FB.all.Front;
            Backbuffer_Index := RI.Drawing_FB.all.Back;

            --  Record pixels with their positions into array
            declare
               All_Pixels_Index : Flat_Buffer_t := 1;
            begin
               for Y in TUI_Height'First .. RI.Terminal_Height loop
                  for X in TUI_Width'First .. RI.Terminal_Width loop
                     All_Pixels (All_Pixels_Index) := (
                       X => X,
                       Y => Y,
                       P => Graphics.Get_Buffer_Pixel (RI.Buffers (Frontbuffer_Index), X, Y)
                     );
                     All_Pixels_Length := All_Pixels_Length + 1;
                     if All_Pixels_Index /= Flat_Buffer_t'Last then
                        All_Pixels_Index := All_Pixels_Index + 1;
                     end if;
                  end loop;
               end loop;
            end;

            --  Filter out pixels that aren't different (unless this is the first frame)
            declare
               Cur : PosPixel_t;
               Back : Pixel_t;
               Updated_Pixels_Index : Flat_Buffer_t := 1;
            begin
               for All_Pixels_Index in 1 .. All_Pixels_Length loop
                  Cur := All_Pixels (Flat_Buffer_t (All_Pixels_Index));
                  Back := Graphics.Get_Buffer_Pixel (RI.Buffers (Backbuffer_Index), Cur.X, Cur.Y);

                  if not (Cur.P = Back) or RI.First_Frame then
                     Updated_Pixels (Updated_Pixels_Index) := Cur;
                     Updated_Pixels_Length := Updated_Pixels_Length + 1;
                     if Updated_Pixels_Index /= Flat_Buffer_t'Last then
                        Updated_Pixels_Index := Updated_Pixels_Index + 1;
                     end if;
                  end if;
               end loop;
            end;

            --  Draw updated pixels
            declare
               Px : PosPixel_t;
            begin
               for Updated_Pixels_Index in 1 .. Updated_Pixels_Length loop
                  Px := Updated_Pixels (Flat_Buffer_t (Updated_Pixels_Index));
                  Ada.Text_IO.Put (Move (Px.Y, Px.X));
                  Ada.Wide_Wide_Text_IO.Put (+(Px.P));
               end loop;
            end;

            --  Update first-frame var (if needed)
            RI.First_Frame := False;

            --  Release RenderInfo
            RI.Drawing_FB.all.End_Draw;
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
   begin

      Entity_List_PO.Claim_Reading (Entity_List);
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_Tags);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);

         declare
            Render_Info : Render_Info_Component_T renames Render_Info_Component_T (
              Get_Component_Ptr (Component_List, Render_Info_Component_T'Tag).all);
         begin
            Render_Info.Drawing_FB.all.Swap;
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

   --  TODO: Remove dependency on text components, add custom rendering to
   --    enable highlighting substrings for highlighting of cursor/chosen day
   procedure CalendarDisplaySystem (Entity_List_PO : in out Entity_Components_PO) is
      function Trim (S : String) return String is (
         S (S'First + 1 .. S'Last)
      );
      function Pad (S : String; C : Character := '0') return String is (
         (if S'Length = 1 then "" & C else "") & S
      );
      Entity_List : Entity_Components_Ptr;
      --  Search for widgets with text and calendar components
      Search_Component_Tags : constant Component_Tag_Vector.Vector :=
        Widget_Component_T'Tag &
        Calendar_Component_T'Tag;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;

      package String_Vector_P is new
        Ada.Containers.Indefinite_Vectors
          (Index_Type => Natural,
           Element_Type => String);
      use String_Vector_P;

      --  Values to strings
      Weekdays : constant String_Vector_P.Vector := String_Vector_P.To_Vector ("Sunday", 1)
        & "Monday" & "Tuesday" & "Wednesday" & "Thursday" & "Friday" & "Saturday";
      Months : constant String_Vector_P.Vector := String_Vector_P.To_Vector ("Jan", 1)
        & "Feb" & "Mar" & "Apr" & "May" & "Jun" & "Jul" & "Aug" & "Sep" & "Oct" & "Nov" & "Dec";

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
            Calendar_C : Calendar_Component_T renames Calendar_Component_T (
              Get_Component_Ptr (Component_List, Calendar_Component_T'Tag).all);

            Text_Width : constant Integer := Integer (Widget_C.Size_Width);
            Text_Height : constant Integer := Integer (Widget_C.Size_Height);
            Week_Length : constant Positive := 7;
            --Date_Weekday : Natural := Natural (
            --  (Ada.Calendar.Time_Of (Calendar_C.Year,
            --                         Calendar_C.Month,
            --                         Calendar_C.Day
            --                        ) - Rata_Die_Date) / 86400) mod Week_Length + Rata_Die_Weekday;
            Date_Weekday : constant Natural := Natural (Ada.Calendar.Arithmetic."-"
              (Ada.Calendar.Time_Of (Calendar_C.Year,
               Calendar_C.Month,
               Calendar_C.Day), Rata_Die_Date)) mod Week_Length + Rata_Die_Weekday;
            Space_After_Entry : Positive;
            Trailing_Space : Natural;
         begin

            if Calendar_C.Display_Mode = Month_Page
              and Text_Width >= 3*Week_Length-1 and Text_Height >= 9 then
               --  Month_Page mode selected and enough size for it

               declare
                  First_Weekday : constant Natural := (Date_Weekday - Calendar_C.Day + 1) mod Week_Length;

                  --  Year, row 1, left aligned
                  Text_Year : constant String := Trim (Calendar_C.Year'Image);
                  --  Padding. 7 = length of year + length of month abbreviation
                  Text_Year_Pad : constant String := Ada.Strings.Fixed."*" ((Text_Width - 7), " ");
                  --  Month, row 1, right aligned
                  Text_Month : constant String := Months (Natural (Calendar_C.Month) - 1);
                  --  Row 2, spacer
                  Text_Spacer : constant String := Ada.Strings.Fixed."*" (Text_Width, "-");
                  --  Row 3, weekday abbreviations
                  Text_Weekdays : String (1 .. Text_Width) := Ada.Strings.Fixed."*" ((Text_Width), " ");
                  Text_Weekdays_I : Positive := 1;
                  --  Rows 4-9, month days
                  Text_Days : String (1 .. Text_Width * 6) := Ada.Strings.Fixed."*" ((Text_Width * 6), " ");
                  Text_Days_I : Positive := 1;
                  Selected_Day_I : Positive;
               begin
                  --  Row 3, weekday abbreviations
                  Space_After_Entry := (Text_Width-2*Week_Length) / (Week_Length-1);
                  Trailing_Space := Text_Width - 2*Week_Length - Space_After_Entry*(Week_Length-1);
                  for Weekday of Weekdays loop
                     Text_Weekdays (Text_Weekdays_I .. Text_Weekdays_I + 1) :=
                       Weekday (Weekday'First .. Weekday'First + 1);
                     Text_Weekdays_I := Text_Weekdays_I + 2 + Space_After_Entry;
                  end loop;

                  --  Month-day section
                  declare
                     Weekday_Pos : Natural := 0;
                     Month_End : constant Ada.Calendar.Time := Ada.Calendar.Arithmetic."-" (
                       (if Calendar_C.Month = 12 then
                           Ada.Calendar.Time_Of (Calendar_C.Year + 1, 1, 1)
                        else
                           Ada.Calendar.Time_Of (Calendar_C.Year, Calendar_C.Month + 1, 1)
                       ), Ada.Calendar.Arithmetic.Day_Count (1));
                     Day_Count : constant Ada.Calendar.Day_Number := Ada.Calendar.Day (Month_End);
                  begin
                     --  Row 4, padding to align month days to weekdays
                     Weekday_Pos := First_Weekday;
                     Text_Days_I := First_Weekday * (2 + Space_After_Entry) + 1;

                     --  Rows 4-9, month days
                     for Month_Day in 1 .. Positive (Day_Count) loop
                        Text_Days (Text_Days_I .. Text_Days_I + 1) := Pad (Trim (Month_Day'Image), ' ');

                        --  Record text index of selected day
                        if Month_Day = Calendar_C.Day then
                           Selected_Day_I := Text_Days_I;
                        end if;

                        Weekday_Pos := Weekday_Pos + 1;
                        Text_Days_I := Text_Days_I + 2;

                        if Weekday_Pos mod 7 = 0 then
                           Text_Days_I := Text_Days_I + Trailing_Space;
                        else
                           Text_Days_I := Text_Days_I + Space_After_Entry;
                        end if;
                     end loop;
                  end;

                  --  Render text to pixels
                  declare
                     Combined_Text : constant String :=
                       Text_Year & Text_Year_Pad & Text_Month &
                       Text_Spacer &
                       Text_Weekdays &
                       Text_Days;
                     Buffer_X : TUI_Width := TUI_Width'First;
                     Buffer_Y : TUI_Height := TUI_Height'First;
                     Current_Pixel : Pixel_t;
                  begin
                     for Combined_Text_Index in Combined_Text'First .. Combined_Text'Last loop
                        --  Update pixel
                        Current_Pixel := Get_Buffer_Pixel (Widget_C.Render_Buffer, Buffer_X, Buffer_Y);
                        Current_Pixel.Char := Combined_Text (Combined_Text_Index);
                        Current_Pixel.Char_Color := Graphics.White;
                        Current_Pixel.Background_Color := Graphics.Black;
                        Set_Buffer_Pixel (Widget_C.Render_Buffer, Buffer_X, Buffer_Y, Current_Pixel);

                        --  Update buffer position
                        if (Buffer_X = TUI_Width'Last
                            or Buffer_X = TUI_Width (Text_Width)) then
                           Buffer_X := TUI_Width'First;
                           Buffer_Y := Buffer_Y + 1;
                        else
                           Buffer_X := Buffer_X + 1;
                        end if;
                     end loop;
                  end;

                  --  Show highlight on selected day
                  declare
                     Selection_X : constant TUI_Width := TUI_Width ((Natural (Selected_Day_I) - 1) mod Text_Width + 1);
                     --  Magic 3 = # of rows preceeding the first row of days
                     Selection_Y : constant TUI_Height := TUI_Height ((Natural (Selected_Day_I) - 1) / Text_Width + 1 + 3);
                     Current_Pixel : Pixel_t;
                     Swap_Color : Color_t;
                  begin
                     --  Invert FG and BG of selected day's pixels
                     for X in Selection_X .. Selection_X + 1 loop
                        Current_Pixel := Get_Buffer_Pixel (Widget_C.Render_Buffer, X, Selection_Y);
                        Current_Pixel.Char_Color := Graphics.Black;
                        Current_Pixel.Background_Color := Graphics.White;
                        Set_Buffer_Pixel (Widget_C.Render_Buffer, X, Selection_Y, Current_Pixel);
                     end loop;
                  end;
               end;
            else
               --  Not enough size or Date_String mode selected

               declare
                  Combined_Text : constant String :=
                    Pad (Trim (Calendar_C.Year'Image) & "/") &
                    Pad (Trim (Calendar_C.Month'Image) & "/") &
                    Pad (Trim (Calendar_C.Day'Image) & ", ") &
                    Weekdays (Date_Weekday);
                  Buffer_X : TUI_Width := TUI_Width'First;
                  Buffer_Y : TUI_Height := TUI_Height'First;
                  Current_Pixel : Pixel_t;
               begin
                  for Combined_Text_Index in Combined_Text'First .. Combined_Text'Last loop
                     --  Update pixel
                     Current_Pixel := Get_Buffer_Pixel (Widget_C.Render_Buffer, Buffer_X, Buffer_Y);
                     Current_Pixel.Char := Combined_Text (Combined_Text_Index);
                     Set_Buffer_Pixel (Widget_C.Render_Buffer, Buffer_X, Buffer_Y, Current_Pixel);

                     --  Update buffer position
                     if (Buffer_X = TUI_Width'Last
                        or Buffer_X = TUI_Width (Text_Width)) then
                        exit when Buffer_Y = TUI_Height'Last
                          or Buffer_Y = TUI_Height (Text_Height);
                        Buffer_X := TUI_Width'First;
                        Buffer_Y := Buffer_Y + 1;
                     else
                        Buffer_X := Buffer_X + 1;
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;

      --  Release lock on entity list
      Entity_List_PO.Release_Reading;
   end CalendarDisplaySystem;


   --===========================================================================
   -- SYSTEM: Get Active Tab
   --===========================================================================
   function Get_Active_Tab (Entity_List_PO : in out Entity_Components_PO) return Natural is
      Entity_List : Entity_Components_Ptr;
      CP          : Components_Ptr;
      Result      : Natural;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      CP := Get_Entity_Components (Entity_List.all, To_EID ("root"));
      if CP /= null and then Has_Component (CP.all, Tab_Manager_Component_T'Tag) then
         Result := Tab_Manager_Component_T (
            Get_Component (CP.all, Tab_Manager_Component_T'Tag)).Active_Tab;
      else
         Result := 0;
      end if;
      Entity_List_PO.Release_Reading;
      return Result;
   end Get_Active_Tab;

   --===========================================================================
   -- SYSTEM: Reset Backbuffer
   --===========================================================================

   procedure Initialize_World
   (World      : in out Entity_Components_PO;
      Width      : in     TUI_Width;
      Height     : in     TUI_Height;
      Tab_Count  : in     Natural := 0)
   is
      CP          : Components_Ptr;
      RI          : Render_Info_Component_T;
      RW          : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
      Root_BG     : Background_Color_Component_T;
   begin
      --  Create render info entity
      CP := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width       := Width;
      RI.Terminal_Height      := Height;
      RI.Prev_Terminal_Width  := Natural (Width);
      RI.Prev_Terminal_Height := Natural (Height);
      RI.Backbuffer           := Create_Buffer (Width, Height);
      RI.Buffers (0)        := Create_Buffer (Width, Height);
      RI.Buffers (1)        := Create_Buffer (Width, Height);
      RI.Drawing_FB           := new Protected_DB;
      for RX in TUI_Width'First .. Width loop
         for RY in TUI_Height'First .. Height loop
            Set_Buffer_Pixel (RI.Backbuffer, RX, RY,
               (Char             => Character'Val (1),
               Char_Color       => White,
               Background_Color => White,
               Is_Bold          => True,
               Is_Italic        => False,
               Is_Underline     => False,
               Is_Strikethrough => False));
         end loop;
      end loop;
      Add_Component (CP.all, To_CID ("RenderInfo"), RI);

      --  Create root entity
      CP := Add_Entity (World, To_EID ("root"));
      RW.Position_X    := TUI_Width'First;
      RW.Position_Y    := TUI_Height'First;
      RW.Size_Width    := Width;
      RW.Size_Height   := Height;
      RW.Render_Buffer := Create_Buffer (Width, Height);
      Add_Component (CP.all, To_CID ("WidgetComponent"), RW);
      Add_Component (CP.all, To_CID ("RootWidget"),      Root_Marker);
      Root_BG.Background_Color := Black;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), Root_BG);

      --  Attach tab manager if tabs are needed
      if Tab_Count > 0 then
         declare
            TM : Tab_Manager_Component_T;
         begin
            TM.Active_Tab := 0;
            TM.Tab_Count  := Tab_Count;
            Add_Component (CP.all, To_CID ("TabManager"), TM);
         end;
      end if;
   end Initialize_World;

   --===========================================================================
   -- SYSTEM: Reset Backbuffer
   --===========================================================================
   procedure ResetBackbufferSystem
     (Entity_List_PO : in out Entity_Components_PO)
   is
      Entity_List : Entity_Components_Ptr;
      CP          : Components_Ptr;
   begin
      Entity_List_PO.Claim_Writing (Entity_List);
      CP := Get_Entity_Components (Entity_List.all, To_EID ("render_info"));
      if CP /= null then
         declare
            RI : Render_Info_Component_T renames
              Render_Info_Component_T
                (Get_Component_Ptr (CP, Render_Info_Component_T'Tag).all);
         begin
            for RX in TUI_Width'First .. RI.Terminal_Width loop
               for RY in TUI_Height'First .. RI.Terminal_Height loop
                  Set_Buffer_Pixel
                    (RI.Buffers (0),
                     RX,
                     RY,
                     (Char             => Character'Val (1),
                      Char_Color       => White,
                      Background_Color => White,
                      Is_Bold          => True,
                      Is_Italic        => False,
                      Is_Underline     => False,
                      Is_Strikethrough => False));
                  Set_Buffer_Pixel
                    (RI.Buffers (1),
                     RX,
                     RY,
                     (Char             => Character'Val (1),
                      Char_Color       => White,
                      Background_Color => White,
                      Is_Bold          => True,
                      Is_Italic        => False,
                      Is_Underline     => False,
                      Is_Strikethrough => False));
               end loop;
            end loop;
         end;
      end if;
      Entity_List_PO.Release_Writing;
   end ResetBackbufferSystem;

--===========================================================================
-- SYSTEM: TAB SWITCHING
-- When the user presses [ or ], this system updates which entities are visible
-- by rebuilding the root widget's children list for the new active tab.
--===========================================================================

procedure TabSwitchSystem
  (Entity_List_PO : in out Entity_Components_PO;
   Direction      : in     Tab_Direction)
is

   Entity_List   : Entity_Components_Ptr;       --  Points to the whole world of entities
   Manager_Comps : Components_Ptr;              --  Will point to the tab manager's components
   Root_Comps    : Components_Ptr;              --  Will point to the root widget's components
   Matched       : Entity_ID_Vector.Vector;     --  List of entities that have a Tab_Manager

   --  Build a search filter: "find entities that have Tab_Manager_Component_T"
   Search : constant Component_Tag_Vector.Vector :=
      Component_Tag_Vector.To_Vector (Tab_Manager_Component_T'Tag, 1);
begin
   --  Lock the world for writing because we are going to change the
   --  root widget's children list
   Entity_List_PO.Claim_Writing (Entity_List);

   --  Find the entity that has a Tab_Manager component (this will be root)
   Matched := Get_Entities_Matching (Entity_List.all, Search);

   --  Loop over matched entities (in practice just one - root)
   for EID of Matched loop

      --  Get the components of this entity
      Manager_Comps := Get_Entity_Components (Entity_List.all, EID);

      declare
         --  Get a direct live reference to the Tab_Manager component
         --  so any change we make to Mgr immediately changes the real data
         Mgr : Tab_Manager_Component_T renames Tab_Manager_Component_T (
            Get_Component_Ptr (Manager_Comps, Tab_Manager_Component_T'Tag).all);
      begin
         --  STEP 1: Move the active tab number forward or backward
         if Direction = Next then
            --  Going forward: increment, but wrap back to 0 if we were on the last tab
            Mgr.Active_Tab :=
               (if Mgr.Active_Tab = Mgr.Tab_Count - 1 then 0
                else Mgr.Active_Tab + 1);
         else
            --  Going backward: decrement, but wrap to last tab if we were on tab 0
            Mgr.Active_Tab :=
               (if Mgr.Active_Tab = 0 then Mgr.Tab_Count - 1
                else Mgr.Active_Tab - 1);
         end if;

         --  STEP 2: Rebuild the root widget's children list for the new tab
         --  The root's children list is what the renderer uses to decide
         --  what appears on screen - if an entity is not in this list it
         --  is invisible even though it still exists in the world
         Root_Comps := Get_Entity_Components (Entity_List.all, To_EID ("root"));
         if Root_Comps /= null then
            declare
               --  Direct live reference to root's widget component
               Root_W : Widget_Component_T renames Widget_Component_T (
                  Get_Component_Ptr (Root_Comps, Widget_Component_T'Tag).all);

               --  Search filter for finding all tab page entities
               Page_Search : constant Component_Tag_Vector.Vector :=
                  Component_Tag_Vector.To_Vector (Tab_Page_Component_T'Tag, 1);

               Page_Entities : Entity_ID_Vector.Vector;  --  Will hold all tab page entity IDs
               Page_Comps    : Components_Ptr;           --  Working pointer for each page entity
            begin
               --  Wipe the children list completely so we can refill it fresh
               Root_W.Children.Clear;

               --  PASS 1: Add chrome entities
               --  Chrome = help bar, tab bar, separator - things that should
               --  always be visible regardless of which tab is active.
               --  We identify chrome by what it does NOT have:
               --  no Tab_Page tag, not root itself, not the render_info entity
               --  PASS 1: Add chrome entities
               for Cursor in Entity_List.all.Iterate loop
                  declare
                     All_EID : constant Entity_Id    := Entity_Map.Key (Cursor);
                     EC      : constant Components_Ptr :=
                        Get_Entity_Components (Entity_List.all, All_EID);
                  begin
                     if EC /= null
                        and then Has_Component (EC.all, Widget_Component_T'Tag)
                        and then not Has_Component (EC.all, Tab_Page_Component_T'Tag)
                        and then not Has_Component (EC.all, Root_Widget_Component_T'Tag)
                        and then not Has_Component (EC.all, Render_Info_Component_T'Tag)
                     then
                        Root_W.Children.Append (All_EID);
                     end if;
                  end;
               end loop;

               --  PASS 2: Add tab page entities for the active tab only
               --  Get all entities that have a Tab_Page component
               Page_Entities := Get_Entities_Matching (Entity_List.all, Page_Search);

               for Page_EID of Page_Entities loop
                  Page_Comps := Get_Entity_Components (Entity_List.all, Page_EID);
                  declare
                     --  Read this entity's Tab_Index to see which tab it belongs to
                     Page : Tab_Page_Component_T renames Tab_Page_Component_T (
                        Get_Component_Ptr (Page_Comps, Tab_Page_Component_T'Tag).all);
                  begin
                     --  Only add it if its tab number matches the one we just switched to
                     if Page.Tab_Index = Mgr.Active_Tab then
                        Root_W.Children.Append (Page_EID);
                     end if;
                  end;
               end loop;

            end;
         end if;
      end;
   end loop;

   --  Release the write lock so other systems can access the world
   Entity_List_PO.Release_Writing;
end TabSwitchSystem;


procedure TabInitSystem (Entity_List_PO : in out Entity_Components_PO) is
   Entity_List   : Entity_Components_Ptr;
   Root_Comps    : Components_Ptr;
   Manager_Comps : Components_Ptr;
   Matched       : Entity_ID_Vector.Vector;
   Search : constant Component_Tag_Vector.Vector :=
      Component_Tag_Vector.To_Vector (Tab_Manager_Component_T'Tag, 1);
begin
   Entity_List_PO.Claim_Writing (Entity_List);
   Matched := Get_Entities_Matching (Entity_List.all, Search);
   for EID of Matched loop
      Manager_Comps := Get_Entity_Components (Entity_List.all, EID);
      declare
         Mgr : Tab_Manager_Component_T renames Tab_Manager_Component_T (
            Get_Component_Ptr (Manager_Comps, Tab_Manager_Component_T'Tag).all);
      begin
         Root_Comps := Get_Entity_Components (Entity_List.all, To_EID ("root"));
         if Root_Comps /= null then
            declare
               Root_W : Widget_Component_T renames Widget_Component_T (
                  Get_Component_Ptr (Root_Comps, Widget_Component_T'Tag).all);
               Page_Search : constant Component_Tag_Vector.Vector :=
                  Component_Tag_Vector.To_Vector (Tab_Page_Component_T'Tag, 1);
               Page_Entities : Entity_ID_Vector.Vector;
               Page_Comps    : Components_Ptr;
            begin
               Root_W.Children.Clear;
               for Cursor in Entity_List.all.Iterate loop
                  declare
                     All_EID : constant Entity_Id := Entity_Map.Key (Cursor);
                     EC      : constant Components_Ptr :=
                        Get_Entity_Components (Entity_List.all, All_EID);
                  begin
                     if EC /= null
                        and then Has_Component (EC.all, Widget_Component_T'Tag)
                        and then not Has_Component (EC.all, Tab_Page_Component_T'Tag)
                        and then not Has_Component (EC.all, Root_Widget_Component_T'Tag)
                        and then not Has_Component (EC.all, Render_Info_Component_T'Tag)
                     then
                        Root_W.Children.Append (All_EID);
                     end if;
                  end;
               end loop;
               Page_Entities := Get_Entities_Matching (Entity_List.all, Page_Search);
               for Page_EID of Page_Entities loop
                  Page_Comps := Get_Entity_Components (Entity_List.all, Page_EID);
                  declare
                     Page : Tab_Page_Component_T renames Tab_Page_Component_T (
                        Get_Component_Ptr (Page_Comps, Tab_Page_Component_T'Tag).all);
                  begin
                     if Page.Tab_Index = Mgr.Active_Tab then
                        Root_W.Children.Append (Page_EID);
                     end if;
                  end;
               end loop;
            end;
         end if;
      end;
   end loop;
   Entity_List_PO.Release_Writing;
end TabInitSystem;

--===========================================================================
-- HELPER: MAKE WIDGET WITH BACKGROUND COLOR
-- Convenience wrapper around Make_Widget that also attaches a background
-- color in one call instead of three separate lines every time.
--===========================================================================
function Make_Widget_With_BG
  (World : in out Entity_Components_PO;  --  The world to add the entity to
   Name  : String;                        --  Unique name for the entity
   X     : TUI_Width;  Y : in TUI_Height; --  Position on screen
   W     : TUI_Width;  H : in TUI_Height; --  Size of the widget
   BG    : Color_t)                       --  Background color to attach
   return Components_Ptr                  --  Returns pointer so caller can add more components
is
   --  Step 1: Create the base widget entity with position, size and render buffer
   --  The root parent container never changes — only the children list does
   CP   : constant Components_Ptr := Make_Widget (World, Name, X, Y, W, H);
   BG_C : Background_Color_Component_T;  --  Empty background component to fill and attach
begin
   --  Step 2: Fill the background component with the requested color
   --  and attach it to the same entity Make_Widget just created
   BG_C.Background_Color := BG;
   Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG_C);

   --  Return the pointer so the caller can keep attaching more components
   --  like text, progress bars, tab page tags etc
   return CP;
end Make_Widget_With_BG;


end ECS;
