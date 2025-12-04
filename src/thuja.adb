--  Thuja - Terminal User Interface Widget Library
--  Package Body

with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;
with Ada.Text_IO;

package body Thuja is

   ---------------------------------------------------------------------------
   --  Progress Bar Widget Creation
   ---------------------------------------------------------------------------

   procedure Create_Progress_Bar
     (Entity_List  : in Out Entity_Components;
      E_ID         : in Entity_Id;
      Pos_X        : in TUI_Width;
      Pos_Y        : in TUI_Height;
      Width        : in TUI_Width;
      Height       : in TUI_Height := 1;
      Filled_Color : in Color_t := Green;
      Empty_Color  : in Color_t := Gray;
      BG_Color     : in Color_t := Black)
   is
      Comp_Ptr : Components_Ptr;
      Widget_C : Widget_Component_T;
      BG_C     : Background_Color_Component_T;
      PB_C     : Progress_Bar_Component_T;
   begin
      --  Add entity and get components pointer
      Comp_Ptr := Add_Entity (Entity_List, E_ID);

      --  Configure Widget Component
      Widget_C.Position_X := Pos_X;
      Widget_C.Position_Y := Pos_Y;
      Widget_C.Size_Width := Width;
      Widget_C.Size_Height := Height;
      Widget_C.Is_Visible := True;
      Widget_C.Is_Enabled := True;
      Widget_C.Has_Focus := False;
      Widget_C.Render_Buffer := Create_Buffer (Width, Height);

      --  Configure Background Color Component
      BG_C.Background_Color := BG_Color;

      --  Configure Progress Bar Component
      PB_C.Value := 0.0;
      PB_C.Filled_Char := '=';
      PB_C.Empty_Char := ' ';
      PB_C.Filled_Color := Filled_Color;
      PB_C.Empty_Color := Empty_Color;
      PB_C.Show_Percentage := True;
      PB_C.Border_Left := '[';
      PB_C.Border_Right := ']';

      --  Add components to entity
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget_C);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG_C);
      Add_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent"), PB_C);
   end Create_Progress_Bar;

   ---------------------------------------------------------------------------
   --  Progress Bar Operations
   ---------------------------------------------------------------------------

   procedure Set_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Value       : in Float)
   is
      Comp_Ptr : Components_Ptr;
      PB_C     : Progress_Bar_Component_T;
      Clamped  : Float;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List, E_ID);
      if Comp_Ptr = null then
         return;
      end if;

      if not Has_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")) then
         return;
      end if;

      --  Clamp value to 0.0 .. 1.0
      if Value <= 0.0 then
         Clamped := 0.0;
      elsif Value >= 1.0 then
         Clamped := 1.0;
      else
         Clamped := Value;
      end if;

      --  Get current component, update, and store back
      PB_C := Progress_Bar_Component_T (
         Get_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")));
      PB_C.Value := Clamped;
      Add_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent"), PB_C);
   end Set_Progress;

   function Get_Progress
     (Entity_List : in Entity_Components;
      E_ID        : in Entity_Id) return Float
   is
      Comp_Ptr : Components_Ptr;
      PB_C     : Progress_Bar_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List, E_ID);
      if Comp_Ptr = null then
         return 0.0;
      end if;

      if not Has_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")) then
         return 0.0;
      end if;

      PB_C := Progress_Bar_Component_T (
         Get_Component (Comp_Ptr.all, To_CID ("ProgressBarComponent")));
      return PB_C.Value;
   end Get_Progress;

   procedure Increment_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Amount      : in Float)
   is
      Current : Float;
   begin
      Current := Get_Progress (Entity_List, E_ID);
      Set_Progress (Entity_List, E_ID, Current + Amount);
   end Increment_Progress;

   ---------------------------------------------------------------------------
   --  Progress Bar Render System
   ---------------------------------------------------------------------------

   procedure ProgressBarRenderSystem (Entity_List : in Out Entity_Components) is
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
      --  Query for entities with WidgetComponent and ProgressBarComponent
      Search_Component_IDs.Append (To_CID ("WidgetComponent"));
      Search_Component_IDs.Append (To_CID ("ProgressBarComponent"));
      Matched_Entities := Get_Entities_Matching (Entity_List, Search_Component_IDs);

      for EID of Matched_Entities loop
         Comp_Ptr := Get_Entity_Components (Entity_List, EID);

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
   end ProgressBarRenderSystem;

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   procedure Log is
      Pix : Pixel_t := ('A', Red, Black, True);
   begin
      Ada.Text_IO.Put_Line (Pix'Image);
   end Log;

end Thuja;