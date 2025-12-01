with Ada.Characters.Conversions;
with Ada.Strings.Unbounded;
with Ada.Wide_Wide_Text_IO;
with Graphics; use Graphics;

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

   function Get_Component (Self : in Components;
                           Component : in Component_Id)
                           return Component_T'Class is
   begin
      return Self.Components_Map (Component);
   end Get_Component;

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

   ---------------------------------------
   -- Add_Entity
   ---------------------------------------
   function Add_Entity (Self : in out Entity_Components; Id : Entity_Id) return Components_Ptr is
      New_Components : Components_Ptr;
   begin
      if Self.Contains (Id) then
         return Self (Id);
      end if;

      New_Components := new Components;
      Self.Insert (Id, New_Components); -- Add new entity with empty components
      return New_Components;
   end Add_Entity;

   ---------------------------------------
   -- Remove_Entity
   ---------------------------------------
   procedure Remove_Entity (Self : in out Entity_Components; Id : Entity_Id) is
   begin
      if Self.Contains (Id) then
         Self.Delete (Id);
      end if;
   end Remove_Entity;

   ---------------------------------------
   -- Get_Entity_Components
   ---------------------------------------
   function Get_Entity_Components (Self : in Entity_Components; Id : Entity_Id)
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

--   procedure ExampleSystem (Entity_List : Entity_Components) is
--      Search_Component_IDs : Component_ID_Vector.Vector;
--      Matched_Entities : Entity_ID_Vector.Vector;
--      Component_List : Components_Ptr;
--      Component : Component1;
--   begin
--   Search_Component_IDs.Append (To_CID ("Component1"));
--   Search_Component_IDs.Append (To_CID ("Component2"));
--   Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_IDs);
--      for EID of Matched_Entities loop
--         Component_List := Get_Entity_Components (Entity_List, EID);
--         Component := Component1 (
--            Get_Component (Component_List.all, "Component1")
--                                 );
--            ...
--         --  Pass updated vals back to the Components instance
--         --  Required to run Get_Entity_Components again to avoid issues with
--
--         --    Update components
--         Add_Component (
--            Get_Entity_Components (Entity_List, EID).all,
--            To_CID ("Component1"),
--            Component
--                       );
--      end loop;
--   end ExampleSystem;

   --  Built-in systems

   procedure WidgetBackgroundSystem (Entity_List : Entity_Components) is
      Search_Component_IDs : Component_ID_Vector.Vector;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Widget_C : Widget_Component_T;
      BGColor_C : Background_Color_Component_T;
      BGColor : Color_t;
      Px : Pixel_t;
   begin
      Search_Component_IDs.Append (To_CID ("WidgetComponent"));
      Search_Component_IDs.Append (To_CID ("BackgroundColorComponent"));
      Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_IDs);
      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List, EID);
         Widget_C := Widget_Component_T (
            Get_Component (Component_List.all, To_CID ("WidgetComponent"))
                                     );
         BGColor_C := Background_Color_Component_T (
            Get_Component (Component_List.all, To_CID ("BackgroundColorComponent"))
                                               );
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

         --  Update components
         Add_Component (
                        Get_Entity_Components (Entity_List, EID).all,
                        To_CID ("WidgetComponent"),
                        Widget_C
                       );
         Add_Component (
                        Get_Entity_Components (Entity_List, EID).all,
                        To_CID ("BackgroundColorComponent"),
                        BGColor_C
                       );
      end loop;
   end WidgetBackgroundSystem;

   procedure TextRenderSystem (Entity_List : Entity_Components) is
      Search_Component_IDs : Component_ID_Vector.Vector;
      Matched_Entities : Entity_ID_Vector.Vector;
      Component_List : Components_Ptr;
      Widget_C : Widget_Component_T;
      Text_C : Text_Component_T;
      Pos_W : TUI_Width;
      Pos_H : TUI_Height;
      Text : SU.Unbounded_String;
      Char : Character;
      Px : Pixel_t;
   begin
      Search_Component_IDs.Append (To_CID ("WidgetComponent"));
      Search_Component_IDs.Append (To_CID ("TextComponent"));
      Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_IDs);
      for EID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List, EID);
         Widget_C := Widget_Component_T (
            Get_Component (Component_List.all, To_CID ("WidgetComponent"))
                                        );
         Text_C := Text_Component_T (
            Get_Component (Component_List.all, To_CID ("TextComponent"))
                                    );
         Text := Text_C.Text;

         Pos_W := TUI_Width'First;
         Pos_H := TUI_Height'First;
         for Text_Index in Positive'First .. SU.Length(Text) loop
            --  Get character and update pixel fields inside widget's buffer
            Char := SU.Element (Text, Text_Index);
            Px := Get_Buffer_Pixel (Widget_C.Render_Buffer, Pos_W, Pos_H);
            Px.Char := Char;
            Px.Char_Color := Text_C.Text_Color;
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

         --  Update components
         Add_Component (
                        Get_Entity_Components (Entity_List, EID).all,
                        To_CID ("WidgetComponent"),
                        Widget_C
                       );
         Add_Component (
                        Get_Entity_Components (Entity_List, EID).all,
                        To_CID ("TextComponent"),
                        Text_C
                       );
      end loop;
   end TextRenderSystem;

   procedure BufferCopySystem (Entity_List : Entity_Components) is
      procedure RecursiveBufferCopy (Framebuffer : in out Buffer_T;
                                     Root : Widget_Component_T;
                                     Parent : Widget_Component_T) is
         Child_Component_List : Components_Ptr;
         Child_Widget : Widget_Component_T;
         Parent_Pixel : Pixel_t;
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
                  Parent_Pixel
                         );
            end loop;
         end loop;

         --  For the parent's children
         for Child_Entity_ID of Parent.Children loop
            --  Fetch the child's WidgetComponent
            Child_Component_List := Get_Entity_Components (
               Entity_List, Child_Entity_ID
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
   begin
      RI_Component_IDs.Append (To_CID ("RenderInfo"));
      Root_Component_IDs.Append (To_CID ("RootWidget"));
      Matched_RIs := Get_Entities_Matching (Entity_List, RI_Component_IDs);
      Matched_Roots := Get_Entities_Matching (Entity_List, Root_Component_IDs);
      --  For each entity with RenderInfo
      for RI_Entity_ID of Matched_RIs loop
         RI_Components := Get_Entity_Components (Entity_List, RI_Entity_ID);
         RenderInfo_C := Render_Info_Component_T (
            Get_Component (RI_Components.all, To_CID ("RenderInfo"))
                                                 );
         --  For each root
         for R_Entity_ID of Matched_Roots loop
            Root_Components := Get_Entity_Components (Entity_List, R_Entity_ID);
            Root := Widget_Component_T (
               Get_Component (Root_Components.all, To_CID ("WidgetComponent"))
                                       );

            --  For it and its children
            RecursiveBufferCopy (RenderInfo_C.Framebuffer, Root, Root);
         end loop;

         --  Update components
         Add_Component (
                        Get_Entity_Components (Entity_List, RI_Entity_ID).all,
                        To_CID ("RenderInfo"),
                        RenderInfo_C
                       );
      end loop;
   end BufferCopySystem;

   procedure BufferDrawSystem (Entity_List : Entity_Components) is
      --  Both pixel rendering and ANSI codes
      CSI : constant String := Character'Val (16#1B#) & '[';
      function Trim (S : String) return String is (S (S'First + 1 .. S'Last));
      function FG (P : Pixel_t) return String is
        (CSI & "38;2;" & Trim (P.Char_Color.Red'Image) & ";"
             & Trim (P.Char_Color.Green'Image) & ";"
             & Trim (P.Char_Color.Blue'Image) & "m");
      function BG (P : Pixel_t) return String is
        (CSI & "48;2;" & Trim (P.Background_Color.Red'Image) & ";"
             & Trim (P.Background_Color.Green'Image) & ";"
             & Trim (P.Background_Color.Blue'Image) & "m");
      function Bold (P : Pixel_t) return String is
        (CSI & (if P.Is_Bold then "1m" else "22m"));
      function Format (P : Pixel_t) return String is
         (FG (P) & BG (P) & Bold (P));
      function Move (Row : TUI_Height; Col : TUI_Width) return String is
        (CSI & Trim (Row'Image) & ";" & Trim (Col'Image) & "H");
      function ConvertWW (P : Pixel_t; Row : TUI_Height;
                          Col : TUI_Width) return Wide_Wide_String is
        (Ada.Characters.Conversions.To_Wide_Wide_String (
         Move (Row, Col) &
           Format (P)) &
           Ada.Characters.Conversions.To_Wide_Wide_Character (P.Char));

      --  Real stuff begins
      Search_Components : Component_ID_Vector.Vector;
      Matched_Entities : Entity_ID_Vector.Vector;
      --  Pointer to Components instance
      RI_Component_List : Components_Ptr;
      --  RenderInfo component
      RI : Render_Info_Component_T;
      --  Framebuffer pixel
      FB_Pixel : Pixel_t;
   begin
      Search_Components.Append (To_CID ("RenderInfo"));
      Matched_Entities := Get_Entities_Matching (Entity_List, Search_Components);
      for EID of Matched_Entities loop
         RI_Component_List := Get_Entity_Components (Entity_List, EID);
         RI := Render_Info_Component_T (Get_Component (RI_Component_List.all, To_CID ("RenderInfo")));
         --  Begin comparing FB to BB and drawing
         for Y in TUI_Height'First .. RI.Terminal_Height loop
            for X in TUI_Width'First .. RI.Terminal_Width loop
               if Get_Buffer_Pixel (RI.Framebuffer, X, Y) /=
                 Get_Buffer_Pixel (RI.Backbuffer, X, Y)
               then
                  --  Fetch buffer pixels
                  FB_Pixel := Get_Buffer_Pixel (RI.Framebuffer, X, Y);
                  --  Draw to terminal
                  Ada.Wide_Wide_Text_IO.Put (ConvertWW (FB_Pixel, Y, X));
                  --  Copy values into backbuffer's pixel
                  Set_Buffer_Pixel (RI.Backbuffer, X, Y, FB_Pixel);
               end if;
            end loop;
         end loop;

         --  Update components
         Add_Component (
                        Get_Entity_Components (Entity_List, EID).all,
                        To_CID ("RenderInfo"),
                        RI
                       );
      end loop;
   end BufferDrawSystem;

end ECS;
