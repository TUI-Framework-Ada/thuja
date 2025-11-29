with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;
--  Implemented components (e.g. RenderInfo, WidgetComponent, etc.)
with Components; use Components;
--  Helper types (e.g. Buffer_T, Pixel, Color, etc.)
with Graphics; use Graphics;
--  ID types
with IDs; use IDs;

package body Systems is

--  procedure ExampleSystem (Entity_List : Entity_Components) is
--     Search_Component_IDs : Component_ID_Vector.Vector
--       := "Component1" & "Component2";
--     Matched_Entities : Entity_ID_Vector.Vector
--       := Get_Entities_Matching (Entity_List, Search_Component_IDs);
--     Component_List : Components_Ptr;
--     Component : Component1;
--  begin
--     for EID of Matched_Entities loop
--        Component_List := Get_Entity_Components (Entity_List, EID);
--        Component := Component1 (
--           Get_Component (Component_List, "Component1")
--                                );
--     end loop;
--  end ExampleSystem;

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
      for Entity_ID of Matched_Entities loop
         Component_List := Get_Entity_Components (Entity_List, Entity_ID);
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
      end loop;
   end WidgetBackgroundSystem;

   procedure TextRenderSystem (Entity_List : Entity_Components) is
      Search_Component_IDs : Component_ID_Vector.Vector
        := "WidgetComponent" & "TextComponent";
      Matched_Entities : Entity_ID_Vector.Vector
        := Entity_List.Get_Entities_Matching (Search_Component_IDs);
      Component_List : Components_Access;
      Widget_C : WidgetComponent;
      Text_C : TextComponent;
      Pos_W : Positive;
      Pos_H : Positive;
      Text : String;
      Char : Character;
      Px : Pixel;
   begin
      for Entity_ID of Matched_Entities loop
         Component_List := Entity_List.Get_Entity_Components (Entity_ID);
         Widget_C := WidgetComponent (
            Component_List.Get_Component ("WidgetComponent")
                                     );
         Text_C := TextComponent (
            Component_List.Get_Component ("TextComponent")
                                 );
         Text := Text_C.Text;

         Pos_W := Positive'First;
         Pos_H := Positive'First;
         for Text_Index in Positive'First .. Text.Length loop
            --  Get character and update pixel fields inside widget's buffer
            Char := Text (Text_Index);
            Px := Widget_C.Render_Buffer.Get_Pixel (Pos_W, Pos_H);
            Px.Character := Char;
            Px.Text_Color := Text_C.Text_Color;
            Widget_C.Render_Buffer.Set_Pixel (Pos_W, Pos_H, Px);

            --  Increment position in 2D array
            Pos_W := Pos_W + 1;
            if Pos_W > Widget_C.Size_Width then
               Pos_W := 1;
               Pos_H := Pos_H + 1;
            end if;
            --  If out of bounds, break
            exit when Pos_H > Widget_C.Size_Height;
         end loop;
      end loop;
   end TextRenderSystem;

   procedure BufferCopySystem (Entity_List : Entity_Components) is
      procedure RecursiveBufferCopy (Framebuffer : Buffer_T;
                                     Parent : WidgetComponent) is
         Child_Component_List : Components_Access;
         Child_Widget : WidgetComponent;
         Parent_Pixel : Pixel;
         FB_Pixel : Pixel;
      begin
         --  For each pixel of Render_Buffer,
         --    only within the bounds of the widget
         --  Assuming 1-indexed Buffer_T and Position_X/Y
         for Pos_W in Positive'First .. Parent.Size_Width loop
            for Pos_H in Positive'First .. Parent.Size_Height loop
               --  Might need to be changed to Get_Pixel(Render_Buffer, ...)
               Parent_Pixel := Parent.Render_Buffer.Get_Pixel (Pos_W, Pos_H);
               --  Copy values from parent to framebuffer
               Framebuffer.Set_Pixel (
                  Parent.Position_X + Pos_W - 1, Parent.Position_Y + Pos_H - 1,
                  Parent_Pixel
                                     );
            end loop;
         end loop;

         --  For the parent's children
         for Child_Entity_ID of Parent.Children loop
            --  Fetch the child's WidgetComponent
            Child_Component_List := Entity_List.Get_Entity_Components (
               Child_Entity_ID
                                                                      );
            Child_Widget := WidgetComponent (
               Child_Component_List.Get_Component ("WidgetComponent")
                                            );
            --  Loop again over the children
            RecursiveBufferCopy (Framebuffer, Child_Widget);
         end loop;
      end RecursiveBufferCopy;

      RI_Component_IDs : Component_ID_Vector.Vector := "RenderInfo";
      Root_Component_IDs : Component_ID_Vector.Vector := "RootWidget";
      Matched_RIs : Entity_ID_Vector.Vector
        := Entity_List.Get_Entities_Matching (RI_Component_IDs);
      Matched_Roots : Entity_ID_Vector.Vector
        := Entity_List.Get_Entities_Matching (Root_Component_IDs);
      RI_Components : Components_Access;
      Root_Components : Components_Access;
      RenderInfo_C : RenderInfo;
      Root : WidgetComponent;
   begin
      --  For each entity with RenderInfo
      for RI_Entity_ID of Matched_RIs loop
         RI_Components := Entity_List.Get_Entity_Components (RI_Entity_ID);
         RenderInfo_C := RenderInfo (
            RI_Components.Get_Component ("RenderInfo")
                                    );
         --  For each root
         for R_Entity_ID of Matched_Roots loop
            Root_Components := Entity_List.Get_Entity_Components (R_Entity_ID);
            Root := WidgetComponent (
               Root_Components.Get_Component ("WidgetComponent")
                                    );

            --  For it and its children
            RecursiveBufferCopy (RenderInfo_C.Framebuffer, Root);
         end loop;
      end loop;
   end BufferCopySystem;

   procedure BufferDrawSystem (Entity_List : Entity_Components) is
      --  Both pixel rendering and ANSI codes
      CSI : constant String := Character'Val (16#1B#) & '[';
      function Trim (S : String) return String is (S (S'First + 1 .. S'Last));
      function FG (P : Pixel) return String is
        (CSI & "38;2;" & Trim (P.Text_Color.Red'Image) & ";"
             & Trim (P.Text_Color.Green'Image) & ";"
             & Trim (P.Text_Color.Blue'Image) & "m");
      function BG (P : Pixel) return String is
        (CSI & "48;2;" & Trim (P.Background_Color.Red'Image) & ";"
             & Trim (P.Background_Color.Green'Image) & ";"
             & Trim (P.Background_Color.Blue'Image) & "m");
      function Bold (P : Pixel) return String is
        (CSI & (if P.Is_Bold then "1m" else "0m"));
      function Format (P : Pixel) return String is
        (FG (P) & BG (P) & Bold (P));
      function Move (Row : Positive; Col : Positive) return String is
        (CSI & Trim (Row'Image) & ";" & Trim (Col'Image) & "H");
      function ConvertWW (P : Pixel; Row : Positive;
                          Col : Positive) return Wide_Wide_String is
        (Move (Row, Col) &
           Ada.Characters.Conversions.To_Wide_Wide_String (Format (P)) &
           Wide_Wide_Character (P.Character));
      RESET : constant String := CSI & "0m";

      --  Real stuff begins
      Search_Components : Component_ID_Vector.Vector := "RenderInfo";
      Matched_Entities : Entity_ID_Vector.Vector
        := Entity_List.Get_Entities_Matching (Search_Components);
      --  Pointer to Components instance
      RI_Component_List : Components_Access;
      --  RenderInfo component
      RI : RenderInfo;
      --  Framebuffer pixel
      FB_Pixel : Pixel;
      --  Backbuffer pixel
      BB_Pixel : Pixel;
   begin
      for Entity_ID of Matched_Entities loop
         RI_Component_List := Entity_List.Get_Entity_Components (Entity_ID);
         RI := RenderInfo (RI_Component_List.Get_Component ("RenderInfo"));
         --  Begin comparing FB to BB and drawing
         for Y in 1 .. RI.Terminal_Height loop
            for X in 1 .. RI.Terminal_Width loop
               if RI.Framebuffer.Get_Pixel (X, Y) /=
                 RI.Backbuffer.Get_Pixel (X, Y)
               then
                  --  Fetch buffer pixels
                  FB_Pixel := RI.Framebuffer.Get_Pixel (X, Y);
                  --  Draw to terminal
                  Ada.Wide_Wide_Text_IO.Put (ConvertWW (FB_Pixel, Y, X));
                  --  Copy values into backbuffer's pixel
                  RI.Backbuffer.Set_Pixel (X, Y, FB_Pixel);
               end if;
            end loop;
         end loop;
      end loop;
   end BufferDrawSystem;
end Systems;
