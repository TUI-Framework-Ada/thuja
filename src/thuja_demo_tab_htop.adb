with Components; use Components;
with IDs;        use IDs;
with htop;       use htop;
with ECS;        use ECS;
with Ada.Strings.Unbounded;

package body Thuja_demo_tab_htop is

   package SU renames Ada.Strings.Unbounded;

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Pad (S : String; Len : Natural) return String is
      Result   : String (1 .. Len) := (others => ' ');
      Copy_Len : constant Natural := Natural'Min (S'Length, Len);
   begin
      Result (1 .. Copy_Len) := S (S'First .. S'First + Copy_Len - 1);
      return Result;
   end Pad;

   procedure Add_Text (CP : ECS.Components_Ptr; Text : String; Color : Color_t; Bold : Boolean) is
      T : Text_Component_T;
   begin
      T.Text := SU.To_Unbounded_String (Text);
      T.Text_Color := Color;
      T.Offset_X := 1;
      T.Offset_Y := 1;
      T.Is_Bold := Bold;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
   end Add_Text;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      CP       : Components_Ptr;
      PB       : Progress_Bar_Component_T;
      Page_Tab : Tab_Page_Component_T;
      Row      : Natural := Natural (Content_Top);

      -- Max_Con_H : constant Natural :=
      --   Natural (Term_Height) - Natural (Content_Top) - 2;

   begin
      Page_Tab.Tab_Index := 0;

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
         Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
         Add_Text (CP, "CPU " & Img (C), White, True);

         CP :=
           Make_Widget_With_BG
             (World,
              "cpubar" & Img (C),
              13,
              TUI_Height (Row),
              60,
              1,
              BG_cpu);
         Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
      Add_Text (CP, "Memory:", White, True);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
      Add_Text (CP, "Disk:", White, True);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
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
      Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
      Add_Text
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
         Add_Component (CP.all, To_CID ("TabPage"), Page_Tab);
         Add_Text (CP, "", White, False);
         Row := Row + 1;
      end loop;
   end Create_Entities;

   overriding
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO)
   is
      EL : ECS.Entity_Components_Ptr;
      CP : Components_Ptr;
      PB : Progress_Bar_Component_T;
      T  : Text_Component_T;
   begin
      Refresh;
      World.Claim_Writing (EL);

      for C in 0 .. Num_Cores - 1 loop
         CP := ECS.Get_Entity_Components (EL.all, To_EID ("cpubar" & Img (C)));
         if CP /= null then
            PB :=
              Progress_Bar_Component_T
                (ECS.Get_Component (CP.all, To_CID ("ProgressBarComponent")));
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

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("memlabel"));
      if CP /= null then
         T :=
           Text_Component_T
             (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := Mem_Label;
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("rambar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (ECS.Get_Component (CP.all, To_CID ("ProgressBarComponent")));
         PB.Value := Mem_Pct;
         PB.Filled_Color :=
           (if Mem_Pct < 0.5
            then Green
            elsif Mem_Pct < 0.75
            then Yellow
            else Red);
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      end if;

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("swpbar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (ECS.Get_Component (CP.all, To_CID ("ProgressBarComponent")));
         PB.Value := Swap_Pct;
         PB.Filled_Color :=
           (if Swap_Pct < 0.5
            then Green
            elsif Swap_Pct < 0.75
            then Yellow
            else Red);
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      end if;

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("disklabel"));
      if CP /= null then
         T :=
           Text_Component_T
             (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := Disk_Label;
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := ECS.Get_Entity_Components (EL.all, To_EID ("diskbar"));
      if CP /= null then
         PB :=
           Progress_Bar_Component_T
             (ECS.Get_Component (CP.all, To_CID ("ProgressBarComponent")));
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
         CP :=
           ECS.Get_Entity_Components (EL.all, To_EID ("procList" & Img (R)));
         if CP /= null then
            T :=
              Text_Component_T
                (ECS.Get_Component (CP.all, To_CID ("TextComponent")));
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
   end Update;

end Thuja_demo_tab_htop;
