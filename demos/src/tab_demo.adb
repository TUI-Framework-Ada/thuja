with Input_Handling; use Input_Handling;
with Graphics;       use Graphics;
with ECS;            use ECS;
with IDs;            use IDs;
with Components;     use Components;
with Flexbox;
with Flex_Demo;
with Text_Editor;
use type Text_Editor.Editor_Mode_T;
with htop;           use htop;
with Sort_Demo;
with Ada.Strings.Unbounded;

procedure Tab_Demo is

   package SU renames Ada.Strings.Unbounded;

   subtype String_t is String;
   subtype Boolean_t is Boolean;
   subtype Character_t is Character;

   Term_Width  : constant TUI_Width := 80;
   Term_Height : constant TUI_Height := 50;
   Content_Top : constant TUI_Height := 4;

   World : Entity_Components_PO;

   Flex_Con_X : constant TUI_Width := 4;
   Flex_Con_Y : constant TUI_Height := Content_Top + 3;

   Max_Con_H : constant Natural :=
     Natural (Term_Height) - Natural (Flex_Con_Y) - 2;

   function Img (N : Natural) return String_t is
      S : constant String_t := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Pad (S : String_t; Len : Natural) return String_t is
      Result   : String_t (1 .. Len) := [others => ' '];
      Copy_Len : constant Natural := Natural'Min (S'Length, Len);
   begin
      Result (1 .. Copy_Len) := S (S'First .. S'First + Copy_Len - 1);
      return Result;
   end Pad;

   ---------------------------------------------------------------------------
   --  Chrome
   ---------------------------------------------------------------------------
   procedure Create_Chrome is
      Discard : Components_Ptr;
   begin
      Discard :=
        Make_Widget (World, "chrome_help", TUI_Width'First, 1, Term_Width, 1);
      Discard :=
        Make_Widget
          (World, "chrome_tabbar", TUI_Width'First, 2, Term_Width, 1);
      Discard :=
        Make_Widget (World, "chrome_sep", TUI_Width'First, 3, Term_Width, 1);
   end Create_Chrome;

   procedure Update_Chrome is
      EL          : Entity_Components_Ptr;
      Current_Tab : constant Natural := Get_Active_Tab (World);

      Help_BG         : constant Color_t :=
        (Red => 30, Green => 30, Blue => 50);
      Help_FG         : constant Color_t :=
        (Red => 180, Green => 200, Blue => 220);
      Tab_BG_Active   : constant Color_t :=
        (Red => 70, Green => 130, Blue => 180);
      Tab_BG_Inactive : constant Color_t :=
        (Red => 40, Green => 40, Blue => 60);
      Sep_FG          : constant Color_t :=
        (Red => 70, Green => 130, Blue => 180);

      Labels : constant array (0 .. 3) of String_t (1 .. 14) :=
        ["    HTop      ", "  Text Editor ", "   Flexbox    ",
         " Sort Visual  "];

      function Help_Text return String_t is
      begin
         if Current_Tab = 1 and then Text_Editor.Mode = Text_Editor.Insert then
            return
              "INSERT: type to edit  |  ESC: back to Navigation"
              & "                         ";
         elsif Current_Tab = 2 then
            return
              " j: justify  a: align  +/-: width  H/h: height  |  [/]: tab  |  Esc: Quit";
         elsif Current_Tab = 3 then
            return Sort_Demo.Help_Text;
         else
            return
              " [ Prev  ] Next  |  Esc: Quit"
              & "                                    ";
         end if;
      end Help_Text;

   begin
      World.Claim_Writing (EL);

      declare
         CP : constant Components_Ptr :=
           Get_Entity_Components (EL.all, To_EID ("chrome_help"));
         W  : Widget_Component_T :=
           Widget_Component_T
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         Fill_Row (W.Render_Buffer, 1, Help_BG);
         Write_To_Buffer (W.Render_Buffer, 1, 1, Help_Text, Help_FG, Help_BG, False);
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      declare
         CP  : constant Components_Ptr :=
           Get_Entity_Components (EL.all, To_EID ("chrome_tabbar"));
         W   : Widget_Component_T :=
           Widget_Component_T
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
         Col : TUI_Width := 2;
      begin
         Fill_Row (W.Render_Buffer, 1, Tab_BG_Inactive);
         for D in 0 .. 3 loop
            declare
               Lbl    : constant String_t := Labels (D);
               Is_Act : constant Boolean_t := (D = Current_Tab);
               BG     : constant Color_t :=
                 (if Is_Act then Tab_BG_Active else Tab_BG_Inactive);
               FG     : constant Color_t :=
                 (if Is_Act
                  then White
                  else (Red => 140, Green => 150, Blue => 170));
            begin
               Write_To_Buffer
                 (W.Render_Buffer, Col, 1, Lbl, FG, BG, False, Bold => Is_Act);
               Col := Col + TUI_Width (Lbl'Length) + 1;
            end;
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      declare
         CP : constant Components_Ptr :=
           Get_Entity_Components (EL.all, To_EID ("chrome_sep"));
         W  : Widget_Component_T :=
           Widget_Component_T
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         for X in TUI_Width'First .. Term_Width loop
            Set_Buffer_Pixel
              (W.Render_Buffer,
               X,
               1,
               (Char                   => '-',
                Char_Color             => Sep_FG,
                Background_Color       => Black,
                Background_Transparent => True,
                Is_Bold                => False,
                Is_Italic              => False,
                Is_Underline           => False,
                Is_Strikethrough       => False));
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      World.Release_Writing;
   end Update_Chrome;

   ---------------------------------------------------------------------------
   --  Tab 0 — HTop
   ---------------------------------------------------------------------------
   BG_cpu      : constant Color_t := (Red => 10, Green => 20, Blue => 10);
   BG_mem      : constant Color_t := (Red => 20, Green => 15, Blue => 5);
   BG_disk     : constant Color_t := (Red => 5, Green => 15, Blue => 20);
   BG_prochead : constant Color_t := (Red => 15, Green => 15, Blue => 30);
   BG_procbody : constant Color_t := Blue;

   procedure HTop_Add_Text (CP : Components_Ptr; Text : String_t; Color : Color_t; Bold : Boolean_t)
   is
      T : Text_Component_T;
   begin
      T.Text := SU.To_Unbounded_String (Text);
      T.Text_Color := Color;
      T.Offset_X := 1;
      T.Offset_Y := 1;
      T.Is_Bold := Bold;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
   end HTop_Add_Text;

   procedure Create_HTop_Entities is
      CP  : Components_Ptr;
      PB  : Progress_Bar_Component_T;
      Tab : Tab_Page_Component_T;
      Row : Natural := Natural (Content_Top);

      BG_instr    : constant Color_t := (Red => 30, Green => 30, Blue => 50);
   begin
      Tab.Tab_Index := 0;

      CP :=
        Make_Widget_With_BG
          (World,
           "instrlabel",
           2,
           TUI_Height (Row),
           76,
           1,
           BG_instr);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      HTop_Add_Text (CP, "b: Backgrounds p: Processes", White, True);
      Row := Row + 1;

      for C in 0 .. Max_Cores - 1 loop
         CP :=
           Make_Widget_With_BG
             (World,
              "cpulabel" & Img (C),
              2,
              TUI_Height (Row),
              10,
              1,
              BG_cpu);
         Add_Component (CP.all, To_CID ("TabPage"), Tab);
         HTop_Add_Text (CP, "CPU " & Img (C), White, True);

         CP :=
           Make_Widget_With_BG
             (World,
              "cpubar" & Img (C),
              13,
              TUI_Height (Row),
              60,
              1,
              BG_cpu);
         Add_Component (CP.all, To_CID ("TabPage"), Tab);
         PB.Value := 0.0;
         PB.Filled_Char := '=';
         PB.Empty_Char := ' ';
         PB.Filled_Color := Green;
         PB.Empty_Color := Gray;
         PB.Show_Percentage := True;
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         Row := Row + 1;
      end loop;

      Row := Row + 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "memlabel",
           2,
           TUI_Height (Row),
           76,
           1,
           BG_mem);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      HTop_Add_Text (CP, "Memory:", White, True);
      Row := Row + 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "rambar",
           2,
           TUI_Height (Row),
           60,
           1,
           BG_mem);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      PB.Value := 0.0;
      PB.Filled_Color := Yellow;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "swpbar",
           2,
           TUI_Height (Row),
           60,
           1,
           BG_mem);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      PB.Value := 0.0;
      PB.Filled_Color := Red;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 2;

      CP :=
        Make_Widget_With_BG
          (World,
           "disklabel",
           2,
           TUI_Height (Row),
           76,
           1,
           BG_disk);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      HTop_Add_Text (CP, "Disk:", White, True);
      Row := Row + 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "diskbar",
           2,
           TUI_Height (Row),
           60,
           1,
           BG_disk);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      PB.Value := 0.0;
      PB.Filled_Color := Green;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 2;

      CP :=
        Make_Widget_With_BG
          (World,
           "proc_header",
           2,
           TUI_Height (Row),
           76,
           1,
           BG_prochead);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      HTop_Add_Text
        (CP,
         Pad ("PID", 7)
         & Pad ("USER", 10)
         & Pad ("CPU%", 6)
         & Pad ("Mem%", 6)
         & "S Command",
         White,
         True);
      Row := Row + 1;

      for R in 0 .. Num_Proc_Rows - 1 loop
         CP :=
           Make_Widget_With_BG
             (World, "procList" & Img (R), 2, TUI_Height (Row), 76, 1, BG_procbody);
         Add_Component (CP.all, To_CID ("TabPage"), Tab);
         HTop_Add_Text (CP, "", White, False);
         Row := Row + 1;
      end loop;
   end Create_HTop_Entities;

   procedure Toggle_HTop_Backgrounds is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;

      procedure Add_BG (Entity : Components_Ptr; Color : Color_t) is
         BG : constant Background_Color_Component_T := (Background_Color => Color);
      begin
         Add_Component (Entity.all, "Background_Color", BG);
      end Add_BG;
   begin
      World.Claim_Reading (EL);

      --  CPU labels & progress bars
      for C in 0 .. Max_Cores - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("cpulabel" & Img (C)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, BG_cpu);
            end if;
            CP.PO.Release_List;
         end if;
         CP := Get_Entity_Components (EL.all, To_EID ("cpubar" & Img (C)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, BG_cpu);
            end if;
            CP.PO.Release_List;
         end if;
      end loop;

      --  Memory label
      CP := Get_Entity_Components (EL.all, To_EID ("memlabel"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_mem);
         end if;
         CP.PO.Release_List;
      end if;

      --  RAM progress bar
      CP := Get_Entity_Components (EL.all, To_EID ("rambar"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_mem);
         end if;
         CP.PO.Release_List;
      end if;

      --  Swap progress bar
      CP := Get_Entity_Components (EL.all, To_EID ("swpbar"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_mem);
         end if;
         CP.PO.Release_List;
      end if;

      --  Disk label
      CP := Get_Entity_Components (EL.all, To_EID ("disklabel"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_disk);
         end if;
         CP.PO.Release_List;
      end if;

      --  Disk progress bar
      CP := Get_Entity_Components (EL.all, To_EID ("diskbar"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_disk);
         end if;
         CP.PO.Release_List;
      end if;

      --  Processes header label
      CP := Get_Entity_Components (EL.all, To_EID ("proc_header"));
      if CP /= null then
         CP.PO.Claim_List;
         if Has_Component (CP.all, Background_Color_Component_T'Tag) then
            Remove_Component (CP.all, Background_Color_Component_T'Tag);
         else
            Add_BG (CP, BG_prochead);
         end if;
         CP.PO.Release_List;
      end if;

      --  Process lines
      for R in 0 .. Num_Proc_Rows - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("procList" & Img (R)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, BG_procbody);
            end if;
            CP.PO.Release_List;
         end if;
      end loop;

      World.Release_Reading;
   end Toggle_HTop_Backgrounds;

   procedure Toggle_HTop_Processes is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
      Tab : Tab_Page_Component_T;
      --  The value of Row in Create_HTop_Entities just before proc_header
      Row : Natural := 21;
   begin
      Tab.Tab_Index := 0;
      World.Claim_Writing (EL);

      CP := Get_Entity_Components (EL.all, To_EID ("proc_header"));
      if CP /= null then
         World.Release_Writing;
         Remove_Entity (World, To_EID ("proc_header"));
         World.Claim_Writing (EL);
      else
         World.Release_Writing;
         CP :=
           Make_Widget_With_BG
             (World,
              "proc_header",
              2,
              TUI_Height (Row),
              76,
              1,
              BG_prochead);
         World.Claim_Writing (EL);
         Add_Component (CP.all, To_CID ("TabPage"), Tab);
         HTop_Add_Text
           (CP,
            Pad ("PID", 7)
            & Pad ("USER", 10)
            & Pad ("CPU%", 6)
            & Pad ("Mem%", 6)
            & "S Command",
            White,
            True);
      end if;
      Row := Row + 1;

      for R in 0 .. Num_Proc_Rows - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("procList" & Img (R)));
         if CP /= null then
            World.Release_Writing;
            Remove_Entity (World, To_EID ("procList" & Img (R)));
            World.Claim_Writing (EL);
         else
            World.Release_Writing;
            CP :=
              Make_Widget_With_BG
                (World, "procList" & Img (R), 2, TUI_Height (Row), 76, 1, BG_procbody);
            World.Claim_Writing (EL);
            Add_Component (CP.all, To_CID ("TabPage"), Tab);
            HTop_Add_Text (CP, "", White, False);
            Row := Row + 1;
            end if;
      end loop;

      World.Release_Writing;
   end Toggle_HTop_Processes;

   procedure Update_HTop_Stats is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
      PB : Progress_Bar_Component_T;
      T  : Text_Component_T;
   begin
      Refresh;
      World.Claim_Writing (EL);

      for C in 0 .. Num_Cores - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("cpubar" & Img (C)));
         if CP /= null then
            PB :=
              Progress_Bar_Component_T
                (Get_Component (CP.all, To_CID ("ProgressBarComponent")));
            declare
               Usage : constant Float := CPU_Values (C);
            begin
               PB.Value := Usage;
               PB.Filled_Color :=
                 (if Usage < 0.33
                  then Green
                  elsif Usage < 0.66
                  then Yellow
                  else Red);
            end;
            Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         end if;
      end loop;

      CP := Get_Entity_Components (EL.all, To_EID ("memlabel"));
      if CP /= null then
         T :=
           Text_Component_T (Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := Mem_Label;
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("rambar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (Get_Component (CP.all, To_CID ("ProgressBarComponent")));
         PB.Value := Mem_Pct;
         PB.Filled_Color :=
           (if Mem_Pct < 0.5
            then Green
            elsif Mem_Pct < 0.75
            then Yellow
            else Red);
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("swpbar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (Get_Component (CP.all, To_CID ("ProgressBarComponent")));
         PB.Value := Swap_Pct;
         PB.Filled_Color :=
           (if Swap_Pct < 0.5
            then Green
            elsif Swap_Pct < 0.75
            then Yellow
            else Red);
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("disklabel"));
      if CP /= null then
         T :=
           Text_Component_T (Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := Disk_Label;
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("diskbar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (Get_Component (CP.all, To_CID ("ProgressBarComponent")));
         PB.Value := Disk_Pct;
         PB.Filled_Color :=
           (if Disk_Pct < 0.5
            then Green
            elsif Disk_Pct < 0.75
            then Yellow
            else Red);
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      end if;

      for R in 0 .. Num_Proc_Rows - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("procList" & Img (R)));
         if CP /= null then
            T :=
              Text_Component_T
                (Get_Component (CP.all, To_CID ("TextComponent")));
            T.Text := Proc_Rows (R).Text;
            T.Text_Color :=
              (if Proc_Rows (R).High_CPU
               then Red
               elsif Proc_Rows (R).Mid_CPU
               then Gold
               else White);
            Add_Component (CP.all, To_CID ("TextComponent"), T);
         end if;
      end loop;

      World.Release_Writing;
   end Update_HTop_Stats;

   ---------------------------------------------------------------------------
   --  Tab 1 — Text Editor
   ---------------------------------------------------------------------------
   procedure Create_Editor_Entities is
      CP      : Components_Ptr;
      T       : Text_Component_T;
      Tab     : Tab_Page_Component_T;
      Ed_H    : constant TUI_Height := Term_Height - Content_Top - 1;
      Ed_BG   : constant Color_t := (Red => 25, Green => 25, Blue => 25);
      Stat_BG : constant Color_t := Blue;
   begin
      Tab.Tab_Index := 1;

      CP :=
        Make_Widget_With_BG
          (World,
           "ed_area",
           TUI_Width'First,
           Content_Top,
           Term_Width,
           Ed_H,
           Ed_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
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
           Term_Height,
           Term_Width,
           1,
           Stat_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      T.Text :=
        SU.To_Unbounded_String (SU.To_String (Text_Editor.Status_Text));
      T.Text_Color := White;
      T.Offset_X := 1;
      T.Offset_Y := 1;
      T.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
   end Create_Editor_Entities;

   procedure Update_Editor_Display is
      EL   : Entity_Components_Ptr;
      CP   : Components_Ptr;
      T    : Text_Component_T;
      Ed_H : constant Natural := Natural (Term_Height - Content_Top - 1);
   begin
      World.Claim_Writing (EL);

      CP := Get_Entity_Components (EL.all, To_EID ("ed_area"));
      if CP /= null then
         T :=
           Text_Component_T (Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text :=
           SU.To_Unbounded_String
             (SU.To_String (Text_Editor.Build_Editor_Text (0, Ed_H)));
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("ed_status"));
      if CP /= null then
         T :=
           Text_Component_T (Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text :=
           SU.To_Unbounded_String (SU.To_String (Text_Editor.Status_Text));
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      World.Release_Writing;
   end Update_Editor_Display;

   ---------------------------------------------------------------------------
   --  Tab 2 — Flexbox Demo
   ---------------------------------------------------------------------------
   procedure Create_Flex_Demo_Entities is
      CP  : Components_Ptr;
      Tab : Tab_Page_Component_T;
      Txt : Text_Component_T;
      Fl  : Flex_Layout_Component_T;

      Dark_BG : constant Color_t := (Red => 20, Green => 20, Blue => 35);
      Con_BG  : constant Color_t := (Red => 30, Green => 30, Blue => 50);
      Child_H : constant Natural := Flex_Demo.Con_H / 2;

      Child_Colors : constant array (1 .. Flex_Demo.Num_Items) of Color_t :=
        [(Red => 70, Green => 130, Blue => 180),
         (Red => 80, Green => 160, Blue => 80),
         (Red => 180, Green => 120, Blue => 50),
         (Red => 140, Green => 70, Blue => 160)];
   begin
      Tab.Tab_Index := 2;

      CP :=
        Make_Widget_With_BG
          (World, "flex_hint", Flex_Con_X, Content_Top, 76, 1, Dark_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
      Txt.Text :=
        SU.To_Unbounded_String
          ("j: justify  a: align  +/-: width  H/h: height");
      Txt.Text_Color := (Red => 180, Green => 200, Blue => 220);
      Txt.Offset_X := 1;
      Txt.Offset_Y := 1;
      Txt.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), Txt);

      CP :=
        Make_Widget_With_BG
          (World, "flex_status", Flex_Con_X, Content_Top + 1, 76, 1, Dark_BG);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
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
      Add_Component (CP.all, To_CID ("TabPage"), Tab);

      for I in 1 .. Flex_Demo.Num_Items loop
         declare
            Child_CP : constant Components_Ptr :=
              Make_Widget_With_BG
                (World,
                 Flex_Demo.Child_Names (I),
                 Flex_Con_X,
                 Flex_Con_Y,
                 TUI_Width (Flex_Demo.Item_Basis),
                 TUI_Height (Child_H),
                 Child_Colors (I));
         begin
            Add_Component (Child_CP.all, To_CID ("TabPage"), Tab);
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
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         for I in 1 .. Flex_Demo.Num_Items loop
            Con_W.Children.Append (To_EID (Flex_Demo.Child_Names (I)));
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), Con_W);
      end;
   end Create_Flex_Demo_Entities;

   procedure Update_Flex_Demo is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
   begin
      World.Claim_Writing (EL);

      CP := Get_Entity_Components (EL.all, To_EID ("flex_con"));
      if CP /= null then
         declare
            W  : Widget_Component_T :=
              Widget_Component_T
                (Get_Component (CP.all, To_CID ("WidgetComponent")));
            Fl : Flex_Layout_Component_T :=
              Flex_Layout_Component_T
                (Get_Component (CP.all, To_CID ("FlexLayoutComponent")));
         begin
            W.Size_Width := TUI_Width (Flex_Demo.Con_W);
            W.Size_Height := TUI_Height (Flex_Demo.Con_H);
            Fl.Flex_Container.Width := Flex_Demo.Con_W;
            Fl.Flex_Container.Height := Flex_Demo.Con_H;
            Fl.Flex_Container.Justify := Flex_Demo.Current_Justify;
            Fl.Flex_Container.Align := Flex_Demo.Current_Align;
            Fl.Is_Dirty := True;
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
            Add_Component (CP.all, To_CID ("FlexLayoutComponent"), Fl);
         end;
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("flex_status"));
      if CP /= null then
         declare
            T : Text_Component_T :=
              Text_Component_T
                (Get_Component (CP.all, To_CID ("TextComponent")));
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
      FlexLayoutSystem (World);
      FlexAlignTextSystem (World);
   end Update_Flex_Demo;

   ---------------------------------------------------------------------------
   --  Tab 3 — Sound of Sorting
   ---------------------------------------------------------------------------
   Sort_Bar_H  : constant TUI_Height := Term_Height - Content_Top - 2;
   Sort_Code_H : constant TUI_Height := Sort_Bar_H;
   Sort_Stat_H : constant TUI_Height := 2;

   procedure Create_Sort_Entities is
      CP  : Components_Ptr;
      Tab : Tab_Page_Component_T;
   begin
      Tab.Tab_Index := 3;

      CP :=
        Make_Widget
          (World,
           "sort_bars",
           TUI_Width'First,
           Content_Top,
           TUI_Width (Sort_Demo.Bar_W),
           Sort_Bar_H);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);

      CP :=
        Make_Widget
          (World,
           "sort_code",
           TUI_Width (Sort_Demo.Bar_W + 1),
           Content_Top,
           TUI_Width (Sort_Demo.Code_W),
           Sort_Code_H);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);

      CP :=
        Make_Widget
          (World,
           "sort_stat",
           TUI_Width'First,
           Term_Height - 2,
           Term_Width,
           Sort_Stat_H);
      Add_Component (CP.all, To_CID ("TabPage"), Tab);
   end Create_Sort_Entities;

   procedure Update_Sort_Display is
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
            Sort_Demo.Render_Status (W.Render_Buffer, Term_Width, Sort_Stat_H);
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         end;
      end if;

      World.Release_Writing;
   end Update_Sort_Display;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------
   procedure Render is
   begin
      Update_Chrome;
      WidgetBackgroundSystem (World);
      FlexAlignTextSystem (World);
      TextRenderSystem (World);
      ProgressBarRenderSystem (World);
      BufferCopySystem (World);
      DoubleBufferFlagSystem (World);
      BufferDrawSystem (World);
   end Render;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------
   Event   : Input_Event_t;
   Running : Boolean_t := True;

begin
   Text_Editor.Initialise;
   Sort_Demo.Initialise;
   Initialise;

   Graphics.Clear_Screen;

   Initialize_World (World, Term_Width, Term_Height, Tab_Count => 4);
   Create_Chrome;
   Create_HTop_Entities;
   Create_Editor_Entities;
   Create_Flex_Demo_Entities;
   Create_Sort_Entities;

   Update_Chrome;
   TabInitSystem (World);
   ResetBackbufferSystem (World);
   Render;

   Input_Reader.Start;

   while Running loop
      ClearWidgetBufferSystem (World);
      loop
         Input_Buffer.Consume (Event);
         exit when
           Event.Cmd = None and then Event.Char_Value = Character_t'Val (0);

         if (Event.Char_Value = '[' or else Event.Char_Value = ']')
           and then Text_Editor.Mode /= Text_Editor.Insert
         then
            if Event.Char_Value = '[' then
               TabSwitchSystem (World, Prev);
            else
               TabSwitchSystem (World, Next);
            end if;
            Render;

         elsif Get_Active_Tab (World) = 0 then
            case Event.Char_Value is
               when 'b'    =>
                  Toggle_HTop_Backgrounds;

               when 'p'    =>
                  Toggle_HTop_Processes;
                  --  We don't need to switch the tab, just to refresh the new
                  --    entities into the tab
                  TabSwitchSystem (World, Prev);
                  TabSwitchSystem (World, Next);

               when others =>
                  if Event.Cmd = Quit
                    or else Event.Char_Value = Character_t'Val (27)
                  then
                     Running := False;
                     exit;
                  end if;
            end case;

         elsif Get_Active_Tab (World) = 1 then
            if Text_Editor.Mode = Text_Editor.Insert then
               case Event.Cmd is
                  when Quit   =>
                     Text_Editor.Mode := Text_Editor.Navigation;
                     Text_Editor.Clamp_Col;

                  when Enter  =>
                     Text_Editor.Handle_Enter;

                  when Tab    =>
                     Text_Editor.Handle_Tab;

                  when others =>
                     if Event.Char_Value = Character_t'Val (127)
                       or else Event.Char_Value = Character_t'Val (8)
                     then
                        Text_Editor.Handle_Backspace;
                     elsif Event.Char_Value >= ' ' then
                        Text_Editor.Insert_Char (Event.Char_Value);
                     end if;
               end case;
            else
               case Event.Cmd is
                  when Quit   =>
                     Running := False;
                     exit;

                  when others =>
                     Text_Editor.Handle_Navigation (Event.Char_Value);
               end case;
            end if;

         elsif Get_Active_Tab (World) = 2 then
            case Event.Char_Value is
               when 'j'    =>
                  Flex_Demo.Next_Justify;
                  Update_Flex_Demo;

               when 'a'    =>
                  Flex_Demo.Next_Align;
                  Update_Flex_Demo;

               when '+'    =>
                  Flex_Demo.Grow (Natural (Term_Width) - 6);
                  Update_Flex_Demo;

               when '-'    =>
                  Flex_Demo.Shrink;
                  Update_Flex_Demo;

               when 'H'    =>
                  Flex_Demo.Grow_Height (Max_Con_H);
                  Update_Flex_Demo;

               when 'h'    =>
                  Flex_Demo.Shrink_Height;
                  Update_Flex_Demo;

               when others =>
                  if Event.Cmd = Quit
                    or else Event.Char_Value = Character_t'Val (27)
                  then
                     Running := False;
                     exit;
                  end if;
            end case;

         elsif Get_Active_Tab (World) = 3 then
            case Event.Char_Value is
               when '1' .. '6' =>
                  Sort_Demo.Switch_Algo
                    (Character_t'Pos (Event.Char_Value)
                     - Character_t'Pos ('0'));

               when ' '        =>
                  Sort_Demo.Toggle_Play;

               when 'n' | 'N'  =>
                  Sort_Demo.Step_Forward;

               when '+' | '='  =>
                  Sort_Demo.Speed_Down;

               when '-'        =>
                  Sort_Demo.Speed_Up;

               when 'r' | 'R'  =>
                  Sort_Demo.Reset;

               when others     =>
                  if Event.Cmd = Quit
                    or else Event.Char_Value = Character_t'Val (27)
                  then
                     Running := False;
                     exit;
                  end if;
            end case;

         else
            if Event.Cmd = Quit or else Event.Char_Value = Character_t'Val (27)
            then
               Running := False;
               exit;
            end if;
         end if;
      end loop;

      if Get_Active_Tab (World) = 0 then
         Update_HTop_Stats;
      end if;

      if Get_Active_Tab (World) = 1 then
         Update_Editor_Display;
      end if;

      if Get_Active_Tab (World) = 2 then
         Update_Flex_Demo;
      end if;

      if Get_Active_Tab (World) = 3 then
         Update_Sort_Display;
      end if;

      Render;

      delay 0.05;
   end loop;

   Input_Reader.Stop;
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;

end Tab_Demo;
