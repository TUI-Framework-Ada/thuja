--ecs.adb
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Graphics; use Graphics;
with Flexbox; use Flexbox;
with IDs; use type IDs.Component_ID_Vector.Vector;

package body ECS is

   --  Easy access to unbounded strings
   package SU renames Ada.Strings.Unbounded;

   function Hash_Component (Key : Component_Id) return Ada.Containers.Hash_Type is
   begin
      return SU.Hash (SU.Unbounded_String (Key));
   end Hash_Component;

   procedure Add_Component (Self : in out Components;
                            Component : in Component_Id;
                            Component_Struct : in Component_T'Class) is
   begin
      Self.Components_Map.Include (Component, Component_Struct);
   end Add_Component;

   procedure Remove_Component (Self : in out Components;
                               Component : in Component_Id) is
   begin
      Self.Components_Map.Exclude (Component);
   end Remove_Component;

   function Get_Component (Self : in out Components;
                           Component : in Component_Id)
                           return Component_T'Class is
   begin
      return Self.Components_Map (Component);
   end Get_Component;

   function Get_Component_Ptr (Self : Components_Ptr;
                               Component_Str : String)
                               return Component_Class_Ptr is
      Map : Component_Map renames Self.all.Components_Map;
   begin
      return Map.Reference (To_CID (Component_Str)).Element;
   end Get_Component_Ptr;

   function Has_Component (Self : in Components;
                           Component : in Component_Id) return Boolean is
   begin
      return Self.Components_Map.Contains (Component);
   end Has_Component;

   ------------------------------------------------------------------
   -- HASH FUNCTION FOR ENTITY IDS
   ------------------------------------------------------------------
   -- Added was forgoetten from UML: Entity_Components : HashMap<Entity_ID, *Components>
   function Hash_Entity (Key : Entity_Id) return Ada.Containers.Hash_Type is
   begin
      return SU.Hash (SU.Unbounded_String (Key));
   end Hash_Entity;

   ------------------------------------------------------------------
   --  Protected object for the entity list
   ------------------------------------------------------------------
   protected body Entity_Components_PO is
      --  Wait for no writers to receive the entity list to read from
      entry Claim_Reading (Entity_List : in out Entity_Components_Ptr)
        when not Write_Using is
      begin
         Read_Using := Read_Using + 1;
         Entity_List := Entities'Access;
      end Claim_Reading;

      --  Wait for no readers to receive an exclusive reference to the entity list
      entry Claim_Writing (Entity_List : in out Entity_Components_Ptr)
        when (Read_Using = 0) and (not Write_Using) is
      begin
         Write_Using := True;
         Entity_List := Entities'Access;
      end Claim_Writing;

      --  Release a reading copy
      procedure Release_Reading is
      begin
         Read_Using := Read_Using - 1;
      end Release_Reading;

      --  Release the writing reference
      procedure Release_Writing is
      begin
         Write_Using := False;
      end Release_Writing;
   end Entity_Components_PO;

   ---------------------------------------
   -- Add_Entity
   ---------------------------------------
   function Add_Entity (Self : in out Entity_Components_PO; Id : Entity_Id) return Components_Ptr is
      Entity_List : Entity_Components_Ptr;
      New_Components : Components_Ptr;
   begin
      Self.Claim_Writing (Entity_List);

      if Entity_List.Contains (Id) then
         New_Components := Entity_List (Id); --  Return existing entity
      else
         New_Components := new Components;
         Entity_List.Insert (Id, New_Components); -- Add new entity with empty components
      end if;

      Self.Release_Writing;
      return New_Components;
   end Add_Entity;

   ---------------------------------------
   -- Remove_Entity
   ---------------------------------------
   procedure Remove_Entity (Self : in out Entity_Components_PO; Id : Entity_Id) is
      Entity_List : Entity_Components_Ptr;
   begin
      Self.Claim_Writing (Entity_List);
      if Entity_List.Contains (Id) then
         --  Delete from entity list
         Entity_List.Delete (Id);

         --  Remove EID from all widget components
         declare
            Search_Component_IDs : Component_ID_Vector.Vector;
            Matched_Entities : Entity_ID_Vector.Vector;
            Component_List : Components_Ptr;
            Widget_C : Widget_Component_T;
         begin
            Search_Component_IDs.Append (To_CID ("WidgetComponent"));
            Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);
            for EID of Matched_Entities loop
               Component_List := Get_Entity_Components (Entity_List.all, EID);
               Widget_C := Widget_Component_T (
                  Get_Component (Component_List.all, To_CID ("WidgetComponent"))
                                              );

               --  Remove from children
               if Widget_C.Children.Contains (Id) then
                  Widget_C.Children.Delete (Widget_C.Children.Find_Index (Id));
               end if;

               --  Update components
               Add_Component (
                  Get_Entity_Components (Entity_List.all, EID).all,
                  To_CID ("WidgetComponent"),
                  Widget_C
               );
            end loop;
         end;
      end if;
      Self.Release_Writing;
   end Remove_Entity;

   ---------------------------------------
   -- Get_Entity_Components
   ---------------------------------------
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

   ---------------------------------------
   -- Get_Entities_Matching
   ---------------------------------------
   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector
   is
      Result : Entity_ID_Vector.Vector;
      Checking_Entity : Entity_Id;
      Matching : Boolean;
   begin
      --  ECS logic
      --  This (theoretically) tests each entity's Components against Required

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
               exit; --  break
            end if;
         end loop;

         if Matching then
            Result.Append (Checking_Entity);
         end if;
      end loop;

      return Result;
   end Get_Entities_Matching;

--   procedure ExampleSystem (Entity_List_PO : in out Entity_Components_PO) is
--      Entity_List : Entity_Components_Ptr;
--      Search_Component_IDs : constant Component_ID_Vector.Vector :=
--        To_CID ("Component_1") &
--        To_CID ("Component_2");
--      Matched_Entities : Entity_ID_Vector.Vector;
--      Component_List : Components_Ptr;
--   begin
--
--      --  Wait for inclusive lock for entity list
--      Entity_List_PO.Claim_Reading (Entity_List);
--      --  Search for entities matching the list of components
--      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);
--
--      for EID of Matched_Entities loop
--         Component_List := Get_Entity_Components (Entity_List.all, EID);
--         declare
--            --  Obtain a view to the component allowing direct modification
--            Component_1_C : Component_1_T renames Component_1_T (
--              Get_Component_Ptr (Component_List, "Component_1").all);
--         begin
--
--            --  Read/update component as needed by interacting with it through the view
--            ...
--
--         end;
--      end loop;
--
--      --  Release lock on entity list
--      Entity_List_PO.Release_Reading;
--   end ExampleSystem;

   --  Built-in systems


-- ================================================================
-- UPDATED FlexLayoutSystem FOR YOUR ECS.ADB
-- Replace your existing FlexLayoutSystem with this version
-- ================================================================

-- FLEXBOX INTEGRATION: Corrected System with Position Mode support
-- This version respects Position_Mode_Component_T and skips widgets
-- that are set to Absolute, Relative, or Fixed positioning modes.
procedure FlexLayoutSystem (Entity_List : Entity_Components) is
   Search_Component_IDs : Component_ID_Vector.Vector;
   Matched_Entities     : Entity_ID_Vector.Vector;

   -- Containers for the Parent (The Flex Container)
   Parent_Comps         : Components_Ptr;
   Flex_C               : Flex_Layout_Component_T;
   Parent_Widget_C      : Widget_Component_T;

   -- Containers for the Children (The Items)
   Child_Comps          : Components_Ptr;
   Child_Widget_C       : Widget_Component_T;
   Child_Id             : Entity_Id;

   -- NEW: Position mode checking
   -- Thought Child_Pos_Mode - Holds the child's Position_Mode_Component so we can check what mode it's in
   -- Skip_Child - Boolean flag (T/F) that makes the logic clearer: "should I skip positioning this child?"
   Child_Pos_Mode       : Position_Mode_Component_T;
   Skip_Child           : Boolean;

   -- Temporary integers for safe calculation
   Calc_X, Calc_Y, Calc_W, Calc_H : Integer;
begin
   Search_Component_IDs.Append (To_CID ("FlexLayoutComponent"));
   Search_Component_IDs.Append (To_CID ("WidgetComponent"));

   Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_IDs);

   for Parent_EID of Matched_Entities loop
      Parent_Comps := Get_Entity_Components (Entity_List, Parent_EID);

      Flex_C := Flex_Layout_Component_T (
         Get_Component (Parent_Comps.all, To_CID ("FlexLayoutComponent"))
      );
      Parent_Widget_C := Widget_Component_T (
         Get_Component (Parent_Comps.all, To_CID ("WidgetComponent"))
      );

      -- 1. Sync Flex Container size with the Parent Widget size
      Flex_C.Flex_Container.Width := Integer (Parent_Widget_C.Size_Width);
      Flex_C.Flex_Container.Height := Integer (Parent_Widget_C.Size_Height);

      -- 2. Run Layout Algorithm
      if Flex_C.Is_Dirty then
         Flexbox.Layout (Flex_C.Flex_Container);
         Flex_C.Is_Dirty := False;
         Add_Component (Parent_Comps.all, To_CID ("FlexLayoutComponent"), Flex_C);
      end if;

      -- 3. Apply Calculated Positions to Child Entities
      if Flex_C.Flex_Container.Items /= null then
         for I in 1 .. Flex_C.Flex_Container.Item_Count loop

            Child_Id := Flex_C.Flex_Container.Items(I).Related_Entity;
            Child_Comps := Get_Entity_Components(Entity_List, Child_Id);

            if Child_Comps /= null and then
               Has_Component(Child_Comps.all, To_CID("WidgetComponent")) then

               -- ========================================================
               -- NEW: Check if this child should be positioned by flex
               -- ========================================================

               -- Step 1: Assume we do NOT skip it
               Skip_Child := False;

               -- Step 2: Check if it has a PositionMode component
               if Has_Component(Child_Comps.all, To_CID("PositionMode")) then
                  Child_Pos_Mode := Position_Mode_Component_T (
                     Get_Component (Child_Comps.all, To_CID ("PositionMode"))
                  );

                  -- Step 3: If mode is not Flex, set Skip_Child to True
                  if Child_Pos_Mode.Mode /= Flex then
                     Skip_Child := True;
                  end if;
               end if;

               -- Step 4: If Skip_Child is False, proceed to position/size the child
               if not Skip_Child then
                  Child_Widget_C := Widget_Component_T (
                     Get_Component (Child_Comps.all, To_CID ("WidgetComponent"))
                  );

                  -- FIX: Use Integer math first to allow 0 offsets, then cast to TUI type
                  -- Parent (1) + Offset (0) = 1 (Valid TUI_Width)
                  Calc_X := Integer(Parent_Widget_C.Position_X) +
                           Flex_C.Flex_Container.Items(I).Position_X;
                  Calc_Y := Integer(Parent_Widget_C.Position_Y) +
                           Flex_C.Flex_Container.Items(I).Position_Y;

                  Child_Widget_C.Position_X := TUI_Width(Calc_X);
                  Child_Widget_C.Position_Y := TUI_Height(Calc_Y);

                  -- UPDATE SIZE:
                  -- We also use Integer'Max(1, ...) to ensure size never hits 0 and crashes
                  if Flex_C.Flex_Container.Direction = Row then
                     Calc_W := Integer'Max(1, Flex_C.Flex_Container.Items(I).Computed_Size);
                     Calc_H := Integer'Max(1, Flex_C.Flex_Container.Items(I).Cross_Size);
                  else
                     Calc_H := Integer'Max(1, Flex_C.Flex_Container.Items(I).Computed_Size);
                     Calc_W := Integer'Max(1, Flex_C.Flex_Container.Items(I).Cross_Size);
                  end if;

                  Child_Widget_C.Size_Width := TUI_Width(Calc_W);
                  Child_Widget_C.Size_Height := TUI_Height(Calc_H);

                  -- Save Child Widget back to ECS
                  Add_Component (Child_Comps.all, To_CID ("WidgetComponent"), Child_Widget_C);
               end if;
               -- If Skip_Child = True, we leave the widget's position/size unchanged

            end if;
         end loop;
      end if;

   end loop;
end FlexLayoutSystem;

   procedure WidgetBackgroundSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_IDs : constant Component_ID_Vector.Vector :=
        To_CID ("WidgetComponent") &
        To_CID ("BackgroundColorComponent");
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      BGColor : Color_t;
      Px : Pixel_t;
   begin

      --  Wait for inclusive lock for entity list
      Entity_List_PO.Claim_Reading (Entity_List);
      --  Search for entities matching the list of components
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);
         declare
            --  Obtain a view to the component allowing direct modification
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, "WidgetComponent").all);
            BGColor_C : Background_Color_Component_T renames Background_Color_Component_T (
              Get_Component_Ptr (Component_List, "BackgroundColorComponent").all);
         begin
            BGColor := BGColor_C.Background_Color;

            for Pos_W in TUI_Width'First .. Widget_C.Size_Width loop
               for Pos_H in TUI_Height'First .. Widget_C.Size_Height loop
                  --  returns a copy of the buffer's pixel
                  Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
                  --  edit values of the copy
                  Px.Char := ' ';
                  Px.Background_Color := BGColor;
                  --  pass back to update in the buffer
                  Set_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H, Px);
               end loop;
            end loop;
         end;
      end loop;

      --  Release lock on entity list
      Entity_List_PO.Release_Reading;
   end WidgetBackgroundSystem;

   procedure TextRenderSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_IDs : constant Component_ID_Vector.Vector :=
        To_CID ("WidgetComponent") &
        To_CID ("TextComponent");
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Pos_W : TUI_Width;
      Pos_H : TUI_Height;
      Text : SU.Unbounded_String;
      Char : Character;
      Px : Pixel_t;
   begin

      --  Wait for inclusive lock for entity list
      Entity_List_PO.Claim_Reading (Entity_List);
--      --  Search for entities matching the list of components
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);

      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);

         declare
            --  Obtain a view to the component allowing direct modification
            Widget_C : Widget_Component_T renames Widget_Component_T (
              Get_Component_Ptr (Component_List, "WidgetComponent").all);

            Text_C : Text_Component_T renames Text_Component_T (
              Get_Component_Ptr (Component_List, "TextComponent").all);
         begin
            Text := Text_C.Text;

            -- Initiatize drawing position using text offsets
            -- Assume Offset_X/Y are relative to the widget's (1, 1) coordinate
            Pos_W := Text_C.Offset_X;
            Pos_H := Text_C.Offset_Y;

            for Text_Index in Positive'First .. SU.Length(Text) loop
               --  Get character and update pixel fields inside widget's buffer
               Char := SU.Element (Text, Text_Index);
               Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
               Px.Char := Char;
               Px.Char_Color := Text_C.Text_Color;

               -- For text stylization
               Px.Is_Bold           := Text_C.Is_Bold;
               Px.Is_Italic         := Text_C.Is_Italic;
               Px.Is_Underline      := Text_C.Is_Underline;
               Px.Is_Strikethrough  := Text_C.Is_Strikethrough;

               Set_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H, Px);

               --  Increment position in 2D array
               Pos_W := Pos_W + 1;
               if Pos_W > Widget_C.Size_Width then
                  Pos_W := 1;
                  Pos_H := Pos_H + 1;
               end if;
               --  If out of bounds, break
               exit when Pos_H > Widget_C.Size_Height;
            end loop;
         end;
      end loop;

      --  Release lock on entity list
      Entity_List_PO.Release_Reading;
   end TextRenderSystem;

   procedure BufferCopySystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;

      procedure RecursiveBufferCopy (Framebuffer : in out Buffer_T;
                                     Root : Widget_Component_T;
                                     Parent : Widget_Component_T) is
         Child_Component_List : Components_Ptr;
         Child_Widget : Widget_Component_T;
         Root_Left, Root_Right, Parent_X : TUI_Width;
         Root_Top, Root_Bottom, Parent_Y : TUI_Height;
      begin
         --  Calc root edges
         Root_Left := Root.Position_X;
         Root_Right := Root.Position_X + Root.Size_Width - TUI_Width (1);
         Root_Top := Root.Position_Y;
         Root_Bottom := Root.Position_Y + Root.Size_Height - TUI_Height (1);

         --  For each pixel of Render_Buffer,
         --    only within the bounds of the widget
         --  Assuming 1-indexed Buffer_T and Position_X/Y
         for Pos_W in TUI_Width'First .. Parent.Size_Width loop
            for Pos_H in TUI_Height'First .. Parent.Size_Height loop
               Parent_Pixel := Get_Buffer_Pixel (Parent.Render_Buffer, Pos_W, Pos_H);
               --  Calc X
               Parent_X := Parent.Position_X + Pos_W - TUI_Width (1);
               --  Calc Y
               Parent_Y := Parent.Position_Y + Pos_H - TUI_Height (1);
               --  In-bounds check
               if (Parent_X < Root_Left) or
                 (Parent_X > Root_Right) or
                 (Parent_Y < Root_Top) or
                 (Parent_Y > Root_Bottom) then
                  exit;
               end if;
               --  Copy values from parent to framebuffer
               Set_Buffer_Pixel (
                  Framebuffer,
                  Parent_X, Parent_Y,
                  Get_Buffer_Pixel (Parent.Render_Buffer, Pos_W, Pos_H)
                         );
            end loop;
         end loop;

         --  For the parent's children
         for Child_Entity_ID of Parent.Children loop
            --  Fetch the child's WidgetComponent
            Child_Component_List := Get_Entity_Components (
               Entity_List.all, Child_Entity_ID
                                                          );
            Child_Widget := Widget_Component_T (
               Get_Component (Child_Component_List.all, To_CID ("WidgetComponent"))
                                               );
            --  Loop again over the children
            RecursiveBufferCopy (Framebuffer, Parent, Child_Widget);
         end loop;
      end RecursiveBufferCopy;

      RI_Component_IDs : Component_ID_Vector.Vector;
      Root_Component_IDs : Component_ID_Vector.Vector;
      Matched_RIs : Entity_ID_Vector.Vector;
      Matched_Roots : Entity_ID_Vector.Vector;
      RI_Components : Components_Ptr;
      Root_Components : Components_Ptr;
      RenderInfo_C : Render_Info_Component_T;
      Root : Widget_Component_T;
      Rendering_To_FB_2 : Boolean;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      RI_Component_IDs.Append (To_CID ("RenderInfo"));
      Root_Component_IDs.Append (To_CID ("RootWidget"));
      Matched_RIs := Get_Entities_Matching (Entity_List.all, RI_Component_IDs);
      Matched_Roots := Get_Entities_Matching (Entity_List.all, Root_Component_IDs);
      --  For each entity with RenderInfo
      for RI_Entity_ID of Matched_RIs loop
         RI_Components := Get_Entity_Components (Entity_List.all, RI_Entity_ID);
         RenderInfo_C := Render_Info_Component_T (
            Get_Component (RI_Components.all, To_CID ("RenderInfo"))
                                                 );
         RenderInfo_C.Drawing_FB.all.Wait (Rendering_To_FB_2);
         --  For each root
         for R_Entity_ID of Matched_Roots loop
            Root_Components := Get_Entity_Components (Entity_List.all, R_Entity_ID);
            Root := Widget_Component_T (
               Get_Component (Root_Components.all, To_CID ("WidgetComponent"))
                                       );

            --  For it and its children
            if Rendering_To_FB_2 then
               RecursiveBufferCopy (RenderInfo_C.Framebuffer_2, Root, Root);
            else
               RecursiveBufferCopy (RenderInfo_C.Framebuffer_1, Root, Root);
            end if;
         end loop;

         --  Update components
         Add_Component (
                        Get_Entity_Components (Entity_List.all, RI_Entity_ID).all,
                        To_CID ("RenderInfo"),
                        RenderInfo_C
                       );
         --  Release RenderInfo
         RenderInfo_C.Drawing_FB.all.Post;
      end loop;
      Entity_List_PO.Release_Reading;
   end BufferCopySystem;

   -- NOTE: Currently for resetting cursor position the cursor retains its position but is still shown.
   -- Additionally, when ctrl + c the position of the cursor may be getting saved but isn't saved when forced out on ctrl + c.
   procedure BufferDrawSystem (Entity_List_PO : in out Entity_Components_PO) is
      --  Both pixel rendering and ANSI codes
      CSI : constant String := Character'Val (16#1B#) & '[';
      Hide_Cursor  : constant String := CSI & "?25l"; -- not hiding cursor?
      Show_Cursor  : constant String := CSI & "?25h"; -- unsure if show is occuring
      Save_Pos     : constant String := CSI & "s";
      Restore_Pos  : constant String := CSI & "u";
      --Reset_SGR    : constant String := CSI & "0m"; -- Resets colors and styles

      -- Helper to bundle cleanup commands
      --Cleanup_Str  : constant Wide_Wide_String :=
      --   Ada.Characters.Conversions.To_Wide_Wide_String(Reset_SGR & Restore_Pos & Show_Cursor);

      function Trim (S : String) return String is (S (S'First + 1 .. S'Last));
      function FG (P : Pixel_t) return String is
        (CSI & "38;2;" & Trim (P.Char_Color.Red'Image) & ";"
             & Trim (P.Char_Color.Green'Image) & ";"
             & Trim (P.Char_Color.Blue'Image) & "m");
      function BG (P : Pixel_t) return String is
        (CSI & "48;2;" & Trim (P.Background_Color.Red'Image) & ";"
             & Trim (P.Background_Color.Green'Image) & ";"
             & Trim (P.Background_Color.Blue'Image) & "m");
      -- 1m sets Bold, 22m sets Bold off
      function Bold (P : Pixel_t) return String is
        (CSI & (if P.Is_Bold then "1m" else "22m"));
      -- 3m sets Italic, 23m sets Italic off
      function Italic (P : Pixel_t) return String is
        (CSI & (if P.Is_Italic then "3m" else "23m"));
      -- 4m sets Underline, 24m sets Underline off
      function Underline (P : Pixel_t) return String is
        (CSI & (if P.Is_Underline then "4m" else "24m"));
      -- 9m sets Strikethrough, 29 sets Strikethrough off
      function Strikethrough (P : Pixel_t) return String is
        (CSI & (if P.Is_Strikethrough then "9m" else "29m"));
      -- Format function to include format styles
      function Format (P : Pixel_t) return String is
         (FG (P) & BG (P) & Bold (P) & Italic (P) & Underline (P) & Strikethrough (P));
      function Move (Row : TUI_Height; Col : TUI_Width) return String is
        (CSI & Trim (Row'Image) & ";" & Trim (Col'Image) & "H");
      Reset : constant String := CSI & "0m";
      function Convert (P : Pixel_t; Row : TUI_Height;
                        Col : TUI_Width) return String is
        (Move (Row, Col) & Format (P) & P.Char & Reset);

      --  Real stuff begins
      Entity_List : Entity_Components_Ptr;
      Search_Components : Component_ID_Vector.Vector;
      Matched_Entities : Entity_ID_Vector.Vector;
      --  Pointer to Components instance
      RI_Component_List : Components_Ptr;
      --  RenderInfo component
      RI : Render_Info_Component_T;
      --  Framebuffer pixel
      FB_Pixel : Pixel_t;
      Drawing_From_FB_1 : Boolean;

      --  Local type needed due to accessibility rules for safe 'Access usage
      type Drawing_Ptr is access all Buffer_T;
      Drawing : Drawing_Ptr;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      Search_Components.Append (To_CID ("RenderInfo"));
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Components);

      -- PRE-RENDER: Hide the cursor and save its current position
      Ada.Wide_Wide_Text_IO.Put (Ada.Characters.Conversions.To_Wide_Wide_String(Hide_Cursor & Save_Pos));

      -- PROTECTED RENDER LOOP
      begin
         for EID of Matched_Entities loop
            RI_Component_List := Get_Entity_Components (Entity_List.all, EID);
            RI := Render_Info_Component_T (Get_Component (RI_Component_List.all, To_CID ("RenderInfo")));
            RI.Drawing_FB.all.Wait (Drawing_To_FB_1);
            Drawing := (if Drawing_To_FB_1 then RI.Framebuffer_1'Access else RI.Framebuffer_2'Access);

            --  Begin comparing FB to BB and drawing
            for Y in TUI_Height'First .. RI.Terminal_Height loop
               for X in TUI_Width'First .. RI.Terminal_Width loop
                  if Get_Buffer_Pixel (Drawing.all, X, Y) /=
                    Get_Buffer_Pixel (RI.Backbuffer, X, Y)
                  then
                     --  Fetch buffer pixels
                     FB_Pixel := Get_Buffer_Pixel (Drawing.all, X, Y);

                     -- Draw to terminal
                     Ada.Wide_Wide_Text_IO.Put (ConvertWW (FB_Pixel, Y, X));

                     -- Update backbuffer
                     Set_Buffer_Pixel (RI.Backbuffer, X, Y, FB_Pixel);
                  end if;
               end loop;
            end loop;

            -- Pass updated component back
            Add_Component (RI_Component_List.all, To_CID ("RenderInfo"), RI);

            --  Release RenderInfo
            RI.Drawing_FB.all.Post;
         end loop;

      exception
         when others =>
            -- CRASH-ClEANUP: restore position, show cursor
            Ada.Wide_Wide_Text_IO.Put (Ada.Characters.Conversions.To_Wide_Wide_String(Restore_Pos & Show_Cursor));
            Ada.Wide_Wide_Text_IO.Flush;
            raise; -- Rethrow the error for debug just incase
      end;

      -- POST-RENDER: Normal cleanup, reset text, restore position, show cursor
      Ada.Wide_Wide_Text_IO.Put (Ada.Characters.Conversions.To_Wide_Wide_String(Restore_Pos & Show_Cursor));

      -- Ensure commands are sent to the hardware immediately
      Ada.Wide_Wide_Text_IO.Flush;
      Entity_List_PO.Release_Reading;
   end BufferDrawSystem;

   ---------------------------------------------------------------------------
   --  Progress Bar Render System
   ---------------------------------------------------------------------------

   procedure ProgressBarRenderSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List          : Entity_Components_Ptr;
      Search_Component_IDs : Component_ID_Vector.Vector;
      Matched_Entities     : Entity_ID_Vector.Vector;
      Comp_Ptr             : Components_Ptr;
      Widget_C             : Widget_Component_T;
      PB_C                 : Progress_Bar_Component_T;
      BG_C                 : Background_Color_Component_T;
      Px                   : Pixel_t;
      Bar_Width            : Natural;
      Filled_Cells         : Natural;
      Percent              : Natural;
      Percent_Str          : String (1 .. 4);  --  "XXX%" or " XX%" etc.
      Pos_Index            : Natural;
      Current_Char         : Character;
      Has_BG               : Boolean;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      --  Query for entities with WidgetComponent and ProgressBarComponent
      Search_Component_IDs.Append (To_CID ("WidgetComponent"));
      Search_Component_IDs.Append (To_CID ("ProgressBarComponent"));
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);

      for EID of Matched_Entities loop
         Comp_Ptr := Get_Entity_Components (Entity_List.all, EID);

         --  Get components
         Widget_C := Widget_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("WidgetComponent")));
         PB_C := Progress_Bar_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")));

         --  Check for optional background color component
         Has_BG := Has_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"));
         if Has_BG then
            BG_C := Background_Color_Component_T (
               Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent")));
         end if;

         --  Calculate bar dimensions
         --  Format: [====    ] XXX%
         --  Border chars take 2 positions, percentage takes ~5 positions (" 100%")
         --  So bar content width = Widget width - 2 (borders) - 5 (percentage if shown)

         if PB_C.Show_Percentage then
            if Natural (Widget_C.Size_Width) > 7 then
               Bar_Width := Natural (Widget_C.Size_Width) - 7;  -- 2 borders + 5 for " XXX%"
            else
               Bar_Width := 1;
            end if;
         else
            if Natural (Widget_C.Size_Width) > 2 then
               Bar_Width := Natural (Widget_C.Size_Width) - 2;  -- Just borders
            else
               Bar_Width := 1;
            end if;
         end if;

         --  Calculate filled cells
         Filled_Cells := Natural (PB_C.Value * Float (Bar_Width));
         if Filled_Cells > Bar_Width then
            Filled_Cells := Bar_Width;
         end if;

         --  Calculate percentage for display
         Percent := Natural (PB_C.Value * 100.0);
         if Percent > 100 then
            Percent := 100;
         end if;

         --  Format percentage string (right-aligned, 3 digits + %)
         declare
            Pct_Img : constant String := Natural'Image (Percent);
         begin
            --  Natural'Image has leading space, so "  0" to " 100"
            if Percent < 10 then
               Percent_Str := "  " & Pct_Img (Pct_Img'Last) & "%";
            elsif Percent < 100 then
               Percent_Str := " " & Pct_Img (Pct_Img'First + 1 .. Pct_Img'Last) & "%";
            else
               Percent_Str := Pct_Img (Pct_Img'First + 1 .. Pct_Img'Last) & "%";
            end if;
         end;

         --  Render to buffer (first row only for single-line progress bar)
         Pos_Index := 0;
         for X in TUI_Width'First .. Widget_C.Size_Width loop
            Pos_Index := Pos_Index + 1;
            Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, X, TUI_Height'First);

            --  Set background color if available
            if Has_BG then
               Px.Background_Color := BG_C.Background_Color;
            end if;

            --  Determine character and color at this position
            if Pos_Index = 1 then
               --  Left border
               Current_Char := PB_C.Border_Left;
               Px.Char_Color := White;
            elsif Pos_Index = Natural (Widget_C.Size_Width) - 4 and PB_C.Show_Percentage then
               --  Space before percentage
               Current_Char := ' ';
               Px.Char_Color := White;
            elsif Pos_Index > Natural (Widget_C.Size_Width) - 4 and PB_C.Show_Percentage then
               --  Percentage text area
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
               --  Right border (no percentage)
               Current_Char := PB_C.Border_Right;
               Px.Char_Color := White;
            elsif Pos_Index = Bar_Width + 2 then
               --  Right border (with percentage calculation)
               Current_Char := PB_C.Border_Right;
               Px.Char_Color := White;
            elsif Pos_Index > 1 and Pos_Index <= Bar_Width + 1 then
               --  Bar content area
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

         --  Fill remaining rows with background (for multi-row widgets)
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

         --  Update components back to entity
         Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
         Add_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent"), PB_C);
      end loop;
      Entity_List_PO.Release_Reading;
   end ProgressBarRenderSystem;

   --  Swaps the double-buffering flag of Render_Info_Component_T
   --  Should be called after all other systems
   procedure DoubleBufferFlagSystem (Entity_List_PO : in out Entity_Components_PO) is
      Entity_List : Entity_Components_Ptr;
      Search_Component_IDs : Component_ID_Vector.Vector;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Render_Info : Render_Info_Component_T;
      Drawing_From_FB_1 : Boolean;
   begin
      Entity_List_PO.Claim_Reading (Entity_List);
      Search_Component_IDs.Append (To_CID ("RenderInfo"));
      Matched_Entities := Get_Entities_Matching (Entity_List.all, Search_Component_IDs);
      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List.all, EID);

         Render_Info := Render_Info_Component_T (
            Get_Component (Component_List.all, To_CID ("RenderInfo"))
         );
         --
         Render_Info.Drawing_FB.all.Wait (Drawing_From_FB_1);
         Render_Info.Drawing_FB.all.Swap;
         Render_Info.Drawing_FB.all.Post;

         --  Pass updated vals back to the Components instance
         --  Required to run Get_Entity_Components again to avoid issues with

         --    Update components
         Add_Component (
            Get_Entity_Components (Entity_List.all, EID).all,
            To_CID ("RenderInfo"),
            Render_Info
                       );
      end loop;
      Entity_List_PO.Release_Reading;
   end DoubleBufferFlagSystem;

end ECS;
