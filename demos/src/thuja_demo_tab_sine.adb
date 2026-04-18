with Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Components;                        use Components;
with ECS;                               use ECS;
with IDs;                               use IDs;

package body Thuja_demo_tab_sine is

   ----------------------------------------------------------------------------
   --  Internal helpers
   ----------------------------------------------------------------------------

   function Float_To_String (F : Float) return String is
      Rounded  : constant Integer := Integer (F * 10.0);
      Int_Part : constant Integer := Rounded / 10;
      Dec_Part : constant Integer := (Rounded mod 10);
   begin
      return
        Integer'Image (Int_Part) (2 .. Integer'Image (Int_Part)'Last)
        & "."
        & Integer'Image (Dec_Part) (2 .. Integer'Image (Dec_Part)'Last);
   end Float_To_String;

   ----------------------------------------------------------------------------

   function Build_Graph_Text (Tab : Tab_T) return Unbounded_String is
      Center_Row : constant Float := Float (Tab.Graph_Height) / 4.0;
      Max_Swing  : constant Float := Center_Row - 2.0;

      type Grid_T is
        array (0 .. Tab.Graph_Height - 1, 0 .. Tab.Graph_Width - 1) of Boolean;

      Grid     : Grid_T := (others => (others => False));
      Wave_Row : array (0 .. Tab.Graph_Width - 1) of Integer;
      Result   : Unbounded_String := Null_Unbounded_String;
   begin
      --  1. Calculate the row index for each column.
      for Col in 0 .. Tab.Graph_Width - 1 loop
         declare
            X     : constant Float := Float (Col) / Float (Tab.Graph_Width);
            Angle : constant Float :=
              2.0 * Ada.Numerics.Pi * Tab.Frequency * X + Tab.Phase;
            Value : constant Float := Tab.Amplitude * Sin (Angle) * Max_Swing;
            R_Val : Integer := Integer (Center_Row - Value);
         begin
            if R_Val < 0 then
               R_Val := 0;
            end if;
            if R_Val > Tab.Graph_Height - 1 then
               R_Val := Tab.Graph_Height - 1;
            end if;
            Wave_Row (Col) := R_Val;
         end;
      end loop;

      --  2. Vertical interpolation — connect adjacent columns so the wave
      --     looks continuous even at high frequencies.
      for Col in 0 .. Tab.Graph_Width - 1 loop
         declare
            This_Row : constant Integer := Wave_Row (Col);
            Next_Row : constant Integer :=
              (if Col < Tab.Graph_Width - 1
               then Wave_Row (Col + 1)
               else This_Row);
            Row_Min  : constant Integer := Integer'Min (This_Row, Next_Row);
            Row_Max  : constant Integer := Integer'Max (This_Row, Next_Row);
         begin
            for R in Row_Min .. Row_Max loop
               Grid (R, Col) := True;
            end loop;
         end;
      end loop;

      --  3. Serialise the grid to a newline-delimited string.
      for Row in 0 .. Tab.Graph_Height - 1 loop
         for Col in 0 .. Tab.Graph_Width - 1 loop
            if Grid (Row, Col) then
               Append (Result, '*');
            elsif Row = Integer (Center_Row) then
               Append (Result, '-');
            else
               Append (Result, ' ');
            end if;
         end loop;
         Append (Result, Character'Val (10));
      end loop;

      return Result;
   end Build_Graph_Text;

   ----------------------------------------------------------------------------

   function Build_Title_Text (Tab : Tab_T) return Unbounded_String is
   begin
      return
        To_Unbounded_String
          ("  ~ Sine Wave ~"
           & "   AMP: "
           & Float_To_String (Tab.Amplitude)
           & "   FREQ: "
           & Float_To_String (Tab.Frequency)
           & "   SPEED: "
           & Float_To_String (Tab.Speed)
           & "   |  w/s: amp   a/d: freq   q/e: speed");
   end Build_Title_Text;

   ----------------------------------------------------------------------------
   --  Create_Entities
   ----------------------------------------------------------------------------

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      Tab_Page_C : Tab_Page_Component_T;

      Graph_Top : constant TUI_Height := Content_Top + 1;
      Graph_H   : constant TUI_Height := Term_Height - Graph_Top - 1;

      Title_CP     : Components_Ptr;
      Graph_CP     : Components_Ptr;
      Title_Text_C : Text_Component_T;
      Graph_Text_C : Text_Component_T;
      EL           : Entity_Components_Ptr;
   begin
      Tab.Graph_Width := Positive (Term_Width);
      Tab.Graph_Height := Positive (Graph_H);

      Tab_Page_C.Tab_Index := Tab.Tab_Index;

      ---------------------------------------------------------------------------
      --  Title widget
      ---------------------------------------------------------------------------
      Title_CP :=
        Make_Widget_With_BG
          (World,
           "sine_title",
           TUI_Width'First,
           Content_Top,
           Term_Width,
           1,
           (Red => 30, Green => 30, Blue => 80));

      World.Claim_Writing (EL);
      Add_Component (Title_CP.all, To_CID ("TabPage"), Tab_Page_C);
      World.Release_Writing;

      Title_Text_C :=
        (Text             => Build_Title_Text (Tab),
         Text_Color       => White,
         Offset_X         => 1,
         Offset_Y         => 1,
         Is_Bold          => True,
         Is_Italic        => False,
         Is_Underline     => False,
         Is_Strikethrough => False);

      World.Claim_Writing (EL);
      Add_Component (Title_CP.all, To_CID ("TextComponent"), Title_Text_C);
      World.Release_Writing;

      ---------------------------------------------------------------------------
      --  Graph widget
      ---------------------------------------------------------------------------
      Graph_CP :=
        Make_Widget_With_BG
          (World,
           "sine_graph",
           TUI_Width'First,
           Graph_Top,
           Term_Width,
           Graph_H,
           (Red => 20, Green => 20, Blue => 30));

      World.Claim_Writing (EL);
      Add_Component (Graph_CP.all, To_CID ("TabPage"), Tab_Page_C);
      World.Release_Writing;

      Graph_Text_C :=
        (Text             => Build_Graph_Text (Tab),
         Text_Color       => (Red => 0, Green => 220, Blue => 220),
         Offset_X         => 1,
         Offset_Y         => 1,
         Is_Bold          => False,
         Is_Italic        => False,
         Is_Underline     => False,
         Is_Strikethrough => False);

      World.Claim_Writing (EL);
      Add_Component (Graph_CP.all, To_CID ("TextComponent"), Graph_Text_C);
      World.Release_Writing;
   end Create_Entities;

   ----------------------------------------------------------------------------
   --  Update
   ----------------------------------------------------------------------------

   overriding
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO)
   is
      EL           : Entity_Components_Ptr;
      Title_CP     : Components_Ptr;
      Graph_CP     : Components_Ptr;
      Title_Text_C : Text_Component_T;
      Graph_Text_C : Text_Component_T;
   begin
      Tab.Phase := Tab.Phase - (Tab.Speed * 0.16);

      World.Claim_Reading (EL);
      Title_CP := Get_Entity_Components (EL.all, To_EID ("sine_title"));
      Graph_CP := Get_Entity_Components (EL.all, To_EID ("sine_graph"));
      World.Release_Reading;

      if Title_CP = null or else Graph_CP = null then
         return;
      end if;

      Title_Text_C :=
        Text_Component_T
          (Get_Component (Title_CP.all, To_CID ("TextComponent")));
      Title_Text_C.Text := Build_Title_Text (Tab);

      Graph_Text_C :=
        Text_Component_T
          (Get_Component (Graph_CP.all, To_CID ("TextComponent")));
      Graph_Text_C.Text := Build_Graph_Text (Tab);

      World.Claim_Writing (EL);
      Add_Component (Title_CP.all, To_CID ("TextComponent"), Title_Text_C);
      Add_Component (Graph_CP.all, To_CID ("TextComponent"), Graph_Text_C);
      World.Release_Writing;
   end Update;

   ----------------------------------------------------------------------------
   --  Handle_Input
   ----------------------------------------------------------------------------

   procedure Handle_Input (Tab : in out Tab_T; Char : in Character) is
   begin
      case Char is
         when 'w'    =>
            if Tab.Amplitude + Amp_Step <= Amp_Max then
               Tab.Amplitude := Tab.Amplitude + Amp_Step;
            end if;

         when 's'    =>
            if Tab.Amplitude - Amp_Step >= Amp_Min then
               Tab.Amplitude := Tab.Amplitude - Amp_Step;
            end if;

         when 'd'    =>
            if Tab.Frequency + Freq_Step <= Freq_Max then
               Tab.Frequency := Tab.Frequency + Freq_Step;
            end if;

         when 'a'    =>
            if Tab.Frequency - Freq_Step >= Freq_Min then
               Tab.Frequency := Tab.Frequency - Freq_Step;
            end if;

         when 'e'    =>
            if Tab.Speed + Speed_Step <= Speed_Max then
               Tab.Speed := Tab.Speed + Speed_Step;
            end if;

         when 'q'    =>
            if Tab.Speed - Speed_Step >= Speed_Min then
               Tab.Speed := Tab.Speed - Speed_Step;
            end if;

         when others =>
            null;
      end case;
   end Handle_Input;

end Thuja_demo_tab_sine;
