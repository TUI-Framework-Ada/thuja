with Input_Handling; use Input_Handling;
with Graphics;       use Graphics;
with ECS;            use ECS;
with IDs;            use IDs;
with Components;     use Components;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with System_Stats;

procedure Tab_Demo is

   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;
   package SS renames System_Stats;
   use type SS.Platform_Type;
   use type SS.Process_Array_Ptr;

   subtype String_t    is String;
   subtype Boolean_t   is Boolean;
   subtype Character_t is Character;

   Term_Width  : constant TUI_Width  := 80;
   Term_Height : constant TUI_Height := 50;
   Content_Top : constant TUI_Height := 4;

   type Demo_ID is (Demo_Boxes, Demo_HTop, Demo_Editor);
   Active_Demo : Demo_ID := Demo_Boxes;

   World : Entity_Components_PO;

   Chrome_EIDs : Entity_ID_Vector.Vector;
   Boxes_EIDs  : Entity_ID_Vector.Vector;
   HTop_EIDs   : Entity_ID_Vector.Vector;
   Editor_EIDs : Entity_ID_Vector.Vector;

   ---------------------------------------------------------------------------
   --  Box demo
   ---------------------------------------------------------------------------
   Box_W : constant TUI_Width  := 12;
   Box_H : constant TUI_Height := 4;

   type Box_Info is record
      X     : TUI_Width;
      Y     : TUI_Height;
      Order : Natural;
      Label : String_t (1 .. 2);
   end record;

   Boxes : constant array (1 .. 10) of Box_Info := [
      (X => 3,  Y => Content_Top + 0, Order => 3,  Label => " 3"),
      (X => 17, Y => Content_Top + 0, Order => 8,  Label => " 8"),
      (X => 31, Y => Content_Top + 0, Order => 1,  Label => " 1"),
      (X => 45, Y => Content_Top + 0, Order => 6,  Label => " 6"),
      (X => 59, Y => Content_Top + 0, Order => 9,  Label => " 9"),
      (X => 3,  Y => Content_Top + 6, Order => 5,  Label => " 5"),
      (X => 17, Y => Content_Top + 6, Order => 2,  Label => " 2"),
      (X => 31, Y => Content_Top + 6, Order => 10, Label => "10"),
      (X => 45, Y => Content_Top + 6, Order => 4,  Label => " 4"),
      (X => 59, Y => Content_Top + 6, Order => 7,  Label => " 7")
   ];

   Box_Colors : constant array (1 .. 10) of Color_t := [
      (Red => 70,  Green => 130, Blue => 180),
      (Red => 180, Green => 80,  Blue => 80),
      (Red => 80,  Green => 160, Blue => 80),
      (Red => 180, Green => 140, Blue => 60),
      (Red => 140, Green => 80,  Blue => 160),
      (Red => 60,  Green => 160, Blue => 160),
      (Red => 180, Green => 120, Blue => 60),
      (Red => 100, Green => 140, Blue => 60),
      (Red => 160, Green => 80,  Blue => 120),
      (Red => 80,  Green => 100, Blue => 180)
   ];

   ---------------------------------------------------------------------------
   --  Editor state
   ---------------------------------------------------------------------------
   package Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => SU.Unbounded_String);

   Lines        : Line_Vectors.Vector;
   Current_Line : Natural := 0;
   Current_Col  : Natural := 0;
   Sticky_Col   : Natural := 0;

   Gutter_Width : constant Positive := 4;
   Ed_Text_Width : constant Positive := 80 - Gutter_Width - 2;
   Editor_Rows  : constant Positive := 46;   --  50 rows - 4 chrome rows

   type Editor_Mode_T is (Navigation, Insert);
   Ed_Mode : Editor_Mode_T := Navigation;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------
   function Img (N : Natural) return String_t is
      S : constant String_t := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Img_F1 (F : Float) return String_t is
      W : constant Natural := Natural (Float'Floor (F));
      D : constant Natural :=
         Natural (Float'Floor ((F - Float'Floor (F)) * 10.0));
   begin
      return Img (W) & "." & Img (D);
   end Img_F1;

   function Pad (S : String_t; Len : Natural) return String_t is
      Result   : String_t (1 .. Len) := [others => ' '];
      Copy_Len : constant Natural    := Natural'Min (S'Length, Len);
   begin
      Result (1 .. Copy_Len) := S (S'First .. S'First + Copy_Len - 1);
      return Result;
   end Pad;

   procedure Write_To_Buffer
     (Buf  : in out Buffer_T;
      Col  : in     TUI_Width;
      Row  : in     TUI_Height;
      Text : in     String_t;
      FG   : in     Color_t;
      BG   : in     Color_t;
      Bold : in     Boolean_t := False)
   is
      X : TUI_Width := Col;
   begin
      for I in Text'Range loop
         exit when X > Buf.Width;
         Set_Buffer_Pixel (Buf, X, Row,
            (Char             => Text (I),
             Char_Color       => FG,
             Background_Color => BG,
             Is_Bold          => Bold,
             Is_Italic        => False,
             Is_Underline     => False,
             Is_Strikethrough => False));
         X := X + 1;
      end loop;
   end Write_To_Buffer;

   procedure Fill_Row
     (Buf : in out Buffer_T;
      Row : in     TUI_Height;
      BG  : in     Color_t)
   is
   begin
      for X in TUI_Width'First .. Buf.Width loop
         Set_Buffer_Pixel (Buf, X, Row,
            (Char             => ' ',
             Char_Color       => BG,
             Background_Color => BG,
             Is_Bold          => False,
             Is_Italic        => False,
             Is_Underline     => False,
             Is_Strikethrough => False));
      end loop;
   end Fill_Row;

   ---------------------------------------------------------------------------
   --  Poison the backbuffer so BufferDrawSystem does a full repaint
   ---------------------------------------------------------------------------
   procedure Reset_Backbuffer is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
      RI : Render_Info_Component_T;
   begin
      World.Claim_Writing (EL);
      CP := Get_Entity_Components (EL.all, To_EID ("render_info"));
      if CP /= null then
         RI := Render_Info_Component_T (
            Get_Component (CP.all, To_CID ("RenderInfo")));
         for RX in TUI_Width'First .. Term_Width loop
            for RY in TUI_Height'First .. Term_Height loop
               Set_Buffer_Pixel (RI.Backbuffer, RX, RY,
                  (Char             => Character_t'Val (1),
                   Char_Color       => White,
                   Background_Color => White,
                   Is_Bold          => True,
                   Is_Italic        => False,
                   Is_Underline     => False,
                   Is_Strikethrough => False));
            end loop;
         end loop;
         Add_Component (CP.all, To_CID ("RenderInfo"), RI);
      end if;
      World.Release_Writing;
   end Reset_Backbuffer;

   ---------------------------------------------------------------------------
   --  Swap root's children: chrome + whichever demo is active
   ---------------------------------------------------------------------------
   procedure Set_Active_Tab is
      EL : Entity_Components_Ptr;
      RC : Components_Ptr;
      RW : Widget_Component_T;
   begin
      World.Claim_Writing (EL);
      RC := Get_Entity_Components (EL.all, To_EID ("root"));
      RW := Widget_Component_T (
         Get_Component (RC.all, To_CID ("WidgetComponent")));

      RW.Children.Clear;

      for EID of Chrome_EIDs loop
         RW.Children.Append (EID);
      end loop;

      case Active_Demo is
         when Demo_Boxes  =>
            for EID of Boxes_EIDs  loop RW.Children.Append (EID); end loop;
         when Demo_HTop   =>
            for EID of HTop_EIDs   loop RW.Children.Append (EID); end loop;
         when Demo_Editor =>
            for EID of Editor_EIDs loop RW.Children.Append (EID); end loop;
      end case;

      Add_Component (RC.all, To_CID ("WidgetComponent"), RW);
      World.Release_Writing;
   end Set_Active_Tab;

   ---------------------------------------------------------------------------
   --  Create world root + render info
   ---------------------------------------------------------------------------
   procedure Create_World is
      CP          : Components_Ptr;
      RI          : Render_Info_Component_T;
      RW          : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
      Root_BG     : Background_Color_Component_T;
   begin
      CP := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width       := Term_Width;
      RI.Terminal_Height      := Term_Height;
      RI.Prev_Terminal_Width  := Natural (Term_Width);
      RI.Prev_Terminal_Height := Natural (Term_Height);
      RI.Backbuffer           := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_1        := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_2        := Create_Buffer (Term_Width, Term_Height);
      RI.Drawing_FB           := new Protected_DB;

      for RX in TUI_Width'First .. Term_Width loop
         for RY in TUI_Height'First .. Term_Height loop
            Set_Buffer_Pixel (RI.Backbuffer, RX, RY,
               (Char             => Character_t'Val (1),
                Char_Color       => White,
                Background_Color => White,
                Is_Bold          => True,
                Is_Italic        => False,
                Is_Underline     => False,
                Is_Strikethrough => False));
         end loop;
      end loop;

      Add_Component (CP.all, To_CID ("RenderInfo"), RI);

      CP := Add_Entity (World, To_EID ("root"));
      RW.Position_X    := TUI_Width'First;
      RW.Position_Y    := TUI_Height'First;
      RW.Size_Width    := Term_Width;
      RW.Size_Height   := Term_Height;
      RW.Render_Buffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (CP.all, To_CID ("WidgetComponent"),  RW);
      Add_Component (CP.all, To_CID ("RootWidget"),       Root_Marker);
      Root_BG.Background_Color := Black;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), Root_BG);
   end Create_World;

   ---------------------------------------------------------------------------
   --  Create chrome entities and populate Chrome_EIDs
   ---------------------------------------------------------------------------
   procedure Create_Chrome is
      CP : Components_Ptr;
      W  : Widget_Component_T;

      procedure Make_Strip (Name : String_t; Row : TUI_Height) is
      begin
         CP := Add_Entity (World, To_EID (Name));
         W.Position_X    := TUI_Width'First;
         W.Position_Y    := Row;
         W.Size_Width    := Term_Width;
         W.Size_Height   := 1;
         W.Render_Buffer := Create_Buffer (Term_Width, 1);
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         Chrome_EIDs.Append (To_EID (Name));
      end Make_Strip;
   begin
      Make_Strip ("chrome_help",   1);
      Make_Strip ("chrome_tabbar", 2);
      Make_Strip ("chrome_sep",    3);
   end Create_Chrome;

   ---------------------------------------------------------------------------
   --  Update chrome buffers each frame
   ---------------------------------------------------------------------------
   procedure Update_Chrome is
      EL : Entity_Components_Ptr;

      Help_BG         : constant Color_t :=
         (Red => 30,  Green => 30,  Blue => 50);
      Help_FG         : constant Color_t :=
         (Red => 180, Green => 200, Blue => 220);
      Tab_BG_Active   : constant Color_t :=
         (Red => 70,  Green => 130, Blue => 180);
      Tab_BG_Inactive : constant Color_t :=
         (Red => 40,  Green => 40,  Blue => 60);
      Sep_FG          : constant Color_t :=
         (Red => 70,  Green => 130, Blue => 180);

      Demo_Labels : constant array (Demo_ID) of String_t (1 .. 14) := [
         Demo_Boxes  => "  Tab Order   ",
         Demo_HTop   => "    HTop      ",
         Demo_Editor => "  Text Editor "
      ];

      --  Help text is context-sensitive for the editor tab
      function Help_Text return String_t is
      begin
         if Active_Demo = Demo_Editor and then Ed_Mode = Insert then
            return "INSERT: type to edit  |  ESC: back to Navigation                    " &
                   "        ";
         else
            return " [ Prev  ] Next  |  Tab: Focus  |  Esc: Quit                    " &
                   "         ";
         end if;
      end Help_Text;

   begin
      World.Claim_Writing (EL);

      --  Help bar
      declare
         CP : constant Components_Ptr :=
            Get_Entity_Components (EL.all, To_EID ("chrome_help"));
         W  : Widget_Component_T :=
            Widget_Component_T (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         Fill_Row (W.Render_Buffer, 1, Help_BG);
         Write_To_Buffer (W.Render_Buffer, 1, 1,
            Help_Text, Help_FG, Help_BG);
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      --  Tab bar
      declare
         CP  : constant Components_Ptr :=
            Get_Entity_Components (EL.all, To_EID ("chrome_tabbar"));
         W   : Widget_Component_T :=
            Widget_Component_T (Get_Component (CP.all, To_CID ("WidgetComponent")));
         Col : TUI_Width := 2;
      begin
         Fill_Row (W.Render_Buffer, 1, Tab_BG_Inactive);
         for D in Demo_ID loop
            declare
               Lbl    : constant String_t  := Demo_Labels (D);
               Is_Act : constant Boolean_t := (D = Active_Demo);
               BG     : constant Color_t   :=
                  (if Is_Act then Tab_BG_Active else Tab_BG_Inactive);
               FG     : constant Color_t   :=
                  (if Is_Act then White
                   else (Red => 140, Green => 150, Blue => 170));
            begin
               Write_To_Buffer (W.Render_Buffer, Col, 1,
                  Lbl, FG, BG, Bold => Is_Act);
               Col := Col + TUI_Width (Lbl'Length) + 1;
            end;
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      --  Separator
      declare
         CP : constant Components_Ptr :=
            Get_Entity_Components (EL.all, To_EID ("chrome_sep"));
         W  : Widget_Component_T :=
            Widget_Component_T (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         for X in TUI_Width'First .. Term_Width loop
            Set_Buffer_Pixel (W.Render_Buffer, X, 1,
               (Char             => '-',
                Char_Color       => Sep_FG,
                Background_Color => Black,
                Is_Bold          => False,
                Is_Italic        => False,
                Is_Underline     => False,
                Is_Strikethrough => False));
         end loop;
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      World.Release_Writing;
   end Update_Chrome;

   ---------------------------------------------------------------------------
   --  Create box entities, populate Boxes_EIDs
   ---------------------------------------------------------------------------
   procedure Create_Boxes is
      CP  : Components_Ptr;
      W   : Widget_Component_T;
      BG  : Background_Color_Component_T;
      Txt : Text_Component_T;
      Sel : Selectable_Component_T;
   begin
      for I in Boxes'Range loop
         declare
            Name : constant String_t :=
               "box_" & (if I < 10
                         then String_t'(1 => Character_t'Val (48 + I))
                         else "10");
         begin
            CP := Add_Entity (World, To_EID (Name));

            W.Position_X    := Boxes (I).X;
            W.Position_Y    := Boxes (I).Y;
            W.Size_Width    := Box_W;
            W.Size_Height   := Box_H;
            W.Has_Focus     := (Boxes (I).Order = 1);
            W.Render_Buffer := Create_Buffer (Box_W, Box_H);
            Add_Component (CP.all, To_CID ("WidgetComponent"), W);

            BG.Background_Color := Box_Colors (I);
            Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);

            Txt.Text       := SU.To_Unbounded_String (Boxes (I).Label);
            Txt.Text_Color := White;
            Txt.Offset_X   := TUI_Width  (Box_W / 2 - 1);
            Txt.Offset_Y   := TUI_Height (Box_H / 2);
            Txt.Is_Bold    := True;
            Add_Component (CP.all, To_CID ("TextComponent"), Txt);

            Sel.Tab_Order := Boxes (I).Order;
            Add_Component (CP.all, To_CID ("SelectableComponent"), Sel);

            Boxes_EIDs.Append (To_EID (Name));
         end;
      end loop;
   end Create_Boxes;

   ---------------------------------------------------------------------------
   --  Create HTop entities, populate HTop_EIDs
   ---------------------------------------------------------------------------
   procedure Create_HTop_Entities is
      CP          : Components_Ptr;
      W           : Widget_Component_T;
      T           : Text_Component_T;
      BG          : Background_Color_Component_T;
      PB          : Progress_Bar_Component_T;
      Num_Cores   : constant Natural := Natural'Min (SS.Get_CPU_Count, 8);
      Row         : Natural := Natural (Content_Top);

      procedure Make
        (Name : String_t; X : TUI_Width; Y : TUI_Height;
         Wd   : TUI_Width; H : TUI_Height; Fill : Color_t)
      is
      begin
         CP := Add_Entity (World, To_EID (Name));
         W.Position_X    := X;
         W.Position_Y    := Y;
         W.Size_Width    := Wd;
         W.Size_Height   := H;
         W.Render_Buffer := Create_Buffer (Wd, H);
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
         BG.Background_Color := Fill;
         Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);
         HTop_EIDs.Append (To_EID (Name));
      end Make;

   begin
      for C in 0 .. Num_Cores - 1 loop
         Make ("cpulbl" & Img (C), 2, TUI_Height (Row), 10, 1,
               (Red => 10, Green => 20, Blue => 10));
         T.Text := SU.To_Unbounded_String ("CPU" & Img (C) & ":");
         T.Text_Color := White; T.Offset_X := 1; T.Offset_Y := 1;
         T.Is_Bold := False;
         Add_Component (CP.all, To_CID ("TextComponent"), T);

         Make ("cpubar" & Img (C), 13, TUI_Height (Row), 60, 1,
               (Red => 10, Green => 20, Blue => 10));
         PB.Value := 0.0; PB.Filled_Char := '='; PB.Empty_Char := ' ';
         PB.Filled_Color := Green; PB.Empty_Color := Gray;
         PB.Show_Percentage := True;
         Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);

         Row := Row + 1;
      end loop;

      Row := Row + 1;

      Make ("memlbl", 2, TUI_Height (Row), 76, 1,
            (Red => 20, Green => 15, Blue => 5));
      T.Text := SU.To_Unbounded_String ("Memory:");
      T.Text_Color := White; T.Offset_X := 1; T.Offset_Y := 1;
      T.Is_Bold := True;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
      Row := Row + 1;

      Make ("rambar", 2, TUI_Height (Row), 60, 1,
            (Red => 20, Green => 15, Blue => 5));
      PB.Value := 0.0; PB.Filled_Color := Yellow;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 1;

      Make ("swpbar", 2, TUI_Height (Row), 60, 1,
            (Red => 20, Green => 15, Blue => 5));
      PB.Value := 0.0; PB.Filled_Color := Hot_Pink;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 2;

      Make ("disklbl", 2, TUI_Height (Row), 76, 1,
            (Red => 5, Green => 15, Blue => 20));
      T.Text := SU.To_Unbounded_String ("Disk (/):"); T.Text_Color := White;
      T.Offset_X := 1; T.Offset_Y := 1; T.Is_Bold := True;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
      Row := Row + 1;

      Make ("diskbar", 2, TUI_Height (Row), 60, 1,
            (Red => 5, Green => 15, Blue => 20));
      PB.Value := 0.0; PB.Filled_Color := Cyan;
      Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
      Row := Row + 2;

      Make ("procheader", 2, TUI_Height (Row), 76, 1,
            (Red => 15, Green => 15, Blue => 30));
      T.Text := SU.To_Unbounded_String (
         Pad ("PID", 7) & Pad ("USER", 10) & Pad ("CPU%", 6) &
         Pad ("MEM%", 6) & "S  COMMAND");
      T.Text_Color := Violet; T.Offset_X := 1; T.Offset_Y := 1;
      T.Is_Bold := True;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
      Row := Row + 1;

      for R in 0 .. 9 loop
         Make ("procrow" & Img (R), 2, TUI_Height (Row), 76, 1, Black);
         T.Text := SU.To_Unbounded_String ("");
         T.Text_Color := White; T.Offset_X := 1; T.Offset_Y := 1;
         T.Is_Bold := False;
         Add_Component (CP.all, To_CID ("TextComponent"), T);
         Row := Row + 1;
      end loop;

      declare
         Dummy : Float := SS.Get_CPU_Usage_Average;
         pragma Unreferenced (Dummy);
      begin null; end;
   end Create_HTop_Entities;

   ---------------------------------------------------------------------------
   --  Update HTop stats
   ---------------------------------------------------------------------------
   procedure Update_HTop_Stats is
      EL    : Entity_Components_Ptr;
      CP    : Components_Ptr;
      PB    : Progress_Bar_Component_T;
      T     : Text_Component_T;

      Num_Cores : constant Natural := Natural'Min (SS.Get_CPU_Count, 8);
      Procs     : SS.Process_Array_Ptr;

      Tot_MB, Used_MB, Free_MB, Avail_MB,
      Buff_MB, Cache_MB, Swap_Tot_MB, Swap_Used_MB : Natural;
      Disk_Total_GB, Disk_Used_GB : Float;
      Disk_Path : constant String_t :=
         (if SS.Get_Platform = SS.Windows then "C:\" else "/");
   begin
      World.Claim_Writing (EL);

      for C in 0 .. Num_Cores - 1 loop
         CP := Get_Entity_Components (EL.all, To_EID ("cpubar" & Img (C)));
         if CP /= null then
            PB := Progress_Bar_Component_T (
               Get_Component (CP.all, To_CID ("ProgressBarComponent")));
            declare
               Usage : constant Float := SS.Get_CPU_Usage (C) / 100.0;
            begin
               PB.Value        := Usage;
               PB.Filled_Color :=
                  (if Usage < 0.33 then Green
                   elsif Usage < 0.66 then Yellow else Red);
            end;
            Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         end if;
      end loop;

      SS.Get_Memory_Detailed (Tot_MB, Used_MB, Free_MB, Avail_MB,
                              Buff_MB, Cache_MB, Swap_Tot_MB, Swap_Used_MB);
      declare
         Real_Used : constant Natural :=
            Tot_MB - Free_MB - Buff_MB - Cache_MB;
         Mem_Pct   : constant Float :=
            (if Tot_MB > 0 then Float (Real_Used) / Float (Tot_MB) else 0.0);
         Swap_Pct  : constant Float :=
            (if Swap_Tot_MB > 0
             then Float (Swap_Used_MB) / Float (Swap_Tot_MB) else 0.0);
      begin
         CP := Get_Entity_Components (EL.all, To_EID ("memlbl"));
         if CP /= null then
            T := Text_Component_T (
               Get_Component (CP.all, To_CID ("TextComponent")));
            T.Text := SU.To_Unbounded_String (
               "Memory: " & Img_F1 (Float (Real_Used) / 1024.0) &
               "G / " & Img_F1 (Float (Tot_MB) / 1024.0) & "G");
            Add_Component (CP.all, To_CID ("TextComponent"), T);
         end if;

         CP := Get_Entity_Components (EL.all, To_EID ("rambar"));
         if CP /= null then
            PB := Progress_Bar_Component_T (
               Get_Component (CP.all, To_CID ("ProgressBarComponent")));
            PB.Value        := Mem_Pct;
            PB.Filled_Color :=
               (if Mem_Pct < 0.5 then Green
                elsif Mem_Pct < 0.75 then Yellow else Red);
            Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         end if;

         CP := Get_Entity_Components (EL.all, To_EID ("swpbar"));
         if CP /= null then
            PB := Progress_Bar_Component_T (
               Get_Component (CP.all, To_CID ("ProgressBarComponent")));
            PB.Value        := Swap_Pct;
            PB.Filled_Color :=
               (if Swap_Pct < 0.5 then Green
                elsif Swap_Pct < 0.75 then Yellow else Red);
            Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         end if;
      end;

      SS.Get_Disk_Space_GB (Disk_Path, Disk_Total_GB, Disk_Used_GB);
      declare
         Disk_Pct : constant Float := SS.Get_Disk_Usage (Disk_Path);
      begin
         CP := Get_Entity_Components (EL.all, To_EID ("disklbl"));
         if CP /= null then
            T := Text_Component_T (
               Get_Component (CP.all, To_CID ("TextComponent")));
            T.Text := SU.To_Unbounded_String (
               "Disk: " & Img_F1 (Disk_Used_GB) &
               "G / " & Img_F1 (Disk_Total_GB) & "G");
            Add_Component (CP.all, To_CID ("TextComponent"), T);
         end if;

         CP := Get_Entity_Components (EL.all, To_EID ("diskbar"));
         if CP /= null then
            PB := Progress_Bar_Component_T (
               Get_Component (CP.all, To_CID ("ProgressBarComponent")));
            PB.Value        := Disk_Pct;
            PB.Filled_Color :=
               (if Disk_Pct < 0.5 then Green
                elsif Disk_Pct < 0.75 then Yellow else Red);
            Add_Component (CP.all, To_CID ("ProgressBarComponent"), PB);
         end if;
      end;

      Procs := SS.Get_Process_List;
      for R in 0 .. 9 loop
         CP := Get_Entity_Components (EL.all, To_EID ("procrow" & Img (R)));
         if CP /= null then
            T := Text_Component_T (
               Get_Component (CP.all, To_CID ("TextComponent")));
            if Procs /= null and then R < Procs'Length then
               declare
                  P       : SS.Process_Info renames Procs (R + 1);
                  CPU_Pct : constant Natural :=
                     Natural'Min (100, Natural (P.CPU));
                  Mem_Pct : constant Natural :=
                     Natural'Min (100, Natural (P.Memory));
                  St : constant Character_t :=
                     (case P.State is
                      when SS.Running       => 'R',
                      when SS.Sleeping      => 'S',
                      when SS.Stopped       => 'T',
                      when SS.Zombie        => 'Z',
                      when SS.Unknown_State => '?');
               begin
                  T.Text := SU.To_Unbounded_String (
                     Pad (Img (P.PID), 7) &
                     Pad (SU.To_String (P.User), 10) &
                     Pad (Img (CPU_Pct) & "%", 6) &
                     Pad (Img (Mem_Pct) & "%", 6) &
                     St & "  " &
                     Pad (SU.To_String (P.Name), 30));
                  T.Text_Color :=
                     (if CPU_Pct > 50 then Red
                      elsif CPU_Pct > 20 then Gold else White);
               end;
            else
               T.Text := SU.To_Unbounded_String ("");
            end if;
            Add_Component (CP.all, To_CID ("TextComponent"), T);
         end if;
      end loop;

      if Procs /= null then SS.Free_Process_List (Procs); end if;

      World.Release_Writing;
   end Update_HTop_Stats;

   ---------------------------------------------------------------------------
   --  Editor reflow helpers
   ---------------------------------------------------------------------------
   procedure Reflow_From (Start_Line : Natural) is
      L : Natural := Start_Line;
   begin
      loop
         exit when L >= Natural (Lines.Length);
         declare
            Cur : constant String_t := SU.To_String (Lines (L));
         begin
            exit when Cur'Length <= Ed_Text_Width;
            declare
               Keep     : constant String_t :=
                  Cur (Cur'First .. Cur'First + Ed_Text_Width - 1);
               Overflow : constant String_t :=
                  Cur (Cur'First + Ed_Text_Width .. Cur'Last);
            begin
               Lines.Replace_Element (L, SU.To_Unbounded_String (Keep));
               if L = Natural (Lines.Length) - 1 then
                  Lines.Append (SU.To_Unbounded_String (Overflow));
                  exit;
               else
                  Lines.Replace_Element
                    (L + 1,
                     SU.To_Unbounded_String
                       (Overflow & SU.To_String (Lines (L + 1))));
               end if;
            end;
         end;
         L := L + 1;
      end loop;
   end Reflow_From;

   procedure Reflow_Up_From (Start_Line : Natural) is
      L : Natural := Start_Line;
   begin
      loop
         exit when L >= Natural (Lines.Length) - 1;
         declare
            Cur_Len  : constant Natural := SU.Length (Lines (L));
            Space    : constant Natural := Ed_Text_Width - Cur_Len;
            Next_Str : constant String_t := SU.To_String (Lines (L + 1));
         begin
            exit when Space = 0 or else Next_Str'Length = 0;
            declare
               Pull_Count : constant Natural :=
                  Natural'Min (Space, Next_Str'Length);
               Pull       : constant String_t :=
                  Next_Str (Next_Str'First
                            .. Next_Str'First + Pull_Count - 1);
               Remaining  : constant String_t :=
                  Next_Str (Next_Str'First + Pull_Count .. Next_Str'Last);
            begin
               Lines.Replace_Element
                 (L, SU.To_Unbounded_String
                       (SU.To_String (Lines (L)) & Pull));
               if Remaining'Length = 0 then
                  Lines.Delete (L + 1);
                  exit;
               else
                  Lines.Replace_Element
                    (L + 1, SU.To_Unbounded_String (Remaining));
               end if;
            end;
         end;
         L := L + 1;
      end loop;
   end Reflow_Up_From;

   procedure Clamp_Col is
      Line_Len : constant Natural := SU.Length (Lines (Current_Line));
   begin
      Current_Col := (if Line_Len = 0 then 0
                      elsif Current_Col > Line_Len then Line_Len
                      else Current_Col);
   end Clamp_Col;

   ---------------------------------------------------------------------------
   --  Build editor display text
   ---------------------------------------------------------------------------
   function Build_Editor_Text return String_t is

      function Num_Gutter (N : Positive) return String_t is
         Img_S : constant String_t := Positive'Image (N);
         Raw   : constant String_t := Img_S (Img_S'First + 1 .. Img_S'Last);
         Gpad  : String_t (1 .. Gutter_Width) := [others => ' '];
      begin
         if Raw'Length >= Gutter_Width then
            Gpad := Raw (Raw'First .. Raw'First + Gutter_Width - 2) & " ";
         else
            Gpad (Gutter_Width - Raw'Length .. Gutter_Width - 1) := Raw;
            Gpad (Gutter_Width) := ' ';
         end if;
         return Gpad;
      end Num_Gutter;

      function Tilde_Gutter return String_t is
         Gpad : String_t (1 .. Gutter_Width) := [others => ' '];
      begin
         Gpad (1) := '~';
         return Gpad;
      end Tilde_Gutter;

      Result : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      for L in 0 .. Natural (Lines.Length) - 1 loop
         declare
            Raw_Line  : constant String_t := SU.To_String (Lines (L));
            Cursor_At : constant Integer  :=
               (if L = Current_Line
                then Integer (Natural'Min (Current_Col, Raw_Line'Length))
                else -1);
            Row : SU.Unbounded_String :=
               SU.To_Unbounded_String (Num_Gutter (L + 1));
         begin
            for I in Raw_Line'Range loop
               if Cursor_At >= 0
                  and then I = Raw_Line'First + Cursor_At
               then
                  SU.Append (Row, '|');
               end if;
               SU.Append (Row, Raw_Line (I));
            end loop;
            if Cursor_At >= 0 and then Cursor_At = Raw_Line'Length then
               SU.Append (Row, '|');
            end if;
            SU.Append (Result, Row);
            SU.Append (Result, Character_t'Val (10));
         end;
      end loop;
      for Row in Natural (Lines.Length) .. Editor_Rows - 1 loop
         SU.Append (Result, Tilde_Gutter);
         SU.Append (Result, Character_t'Val (10));
      end loop;
      return SU.To_String (Result);
   end Build_Editor_Text;

   function Ed_Status_Text return String_t is
   begin
      case Ed_Mode is
         when Navigation =>
            return "NAVIGATION  w/a/s/d: move  |  i: insert  |  ESC: quit";
         when Insert =>
            return "INSERT  type to edit  |  ESC: back to Navigation        ";
      end case;
   end Ed_Status_Text;

   ---------------------------------------------------------------------------
   --  Create editor entities, populate Editor_EIDs
   ---------------------------------------------------------------------------
   procedure Create_Editor_Entities is
      CP      : Components_Ptr;
      W       : Widget_Component_T;
      BG      : Background_Color_Component_T;
      T       : Text_Component_T;
      Ed_H    : constant TUI_Height := TUI_Height (Editor_Rows);
      Ed_BG   : constant Color_t   := (Red => 25, Green => 25, Blue => 25);
      Stat_BG : constant Color_t   := Blue;
   begin
      --  Main editing area (rows 4 .. 49)
      CP := Add_Entity (World, To_EID ("ed_area"));
      W.Position_X    := TUI_Width'First;
      W.Position_Y    := Content_Top;
      W.Size_Width    := Term_Width;
      W.Size_Height   := Ed_H;
      W.Has_Focus     := True;
      W.Render_Buffer := Create_Buffer (Term_Width, Ed_H);
      Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      BG.Background_Color := Ed_BG;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);
      T.Text       := SU.To_Unbounded_String (Build_Editor_Text);
      T.Text_Color := White;
      T.Offset_X   := 1; T.Offset_Y := 1; T.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
      Editor_EIDs.Append (To_EID ("ed_area"));

      --  Status bar (row 50)
      CP := Add_Entity (World, To_EID ("ed_status"));
      W.Position_X    := TUI_Width'First;
      W.Position_Y    := Term_Height;
      W.Size_Width    := Term_Width;
      W.Size_Height   := 1;
      W.Has_Focus     := False;
      W.Render_Buffer := Create_Buffer (Term_Width, 1);
      Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      BG.Background_Color := Stat_BG;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);
      T.Text       := SU.To_Unbounded_String (Ed_Status_Text);
      T.Text_Color := White;
      T.Offset_X   := 1; T.Offset_Y := 1; T.Is_Bold := False;
      Add_Component (CP.all, To_CID ("TextComponent"), T);
      Editor_EIDs.Append (To_EID ("ed_status"));
   end Create_Editor_Entities;

   ---------------------------------------------------------------------------
   --  Update editor display each frame
   ---------------------------------------------------------------------------
   procedure Update_Editor_Display is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;
      T  : Text_Component_T;
   begin
      World.Claim_Writing (EL);

      CP := Get_Entity_Components (EL.all, To_EID ("ed_area"));
      if CP /= null then
         T := Text_Component_T (
            Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := SU.To_Unbounded_String (Build_Editor_Text);
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      CP := Get_Entity_Components (EL.all, To_EID ("ed_status"));
      if CP /= null then
         T := Text_Component_T (
            Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := SU.To_Unbounded_String (Ed_Status_Text);
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;

      World.Release_Writing;
   end Update_Editor_Display;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------
   procedure Render is
   begin
      WidgetBackgroundSystem  (World);
      TextRenderSystem        (World);
      ProgressBarRenderSystem (World);
      BufferCopySystem        (World);
      DoubleBufferFlagSystem  (World);
      BufferDrawSystem        (World);
   end Render;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------
   Event       : Input_Event_t;
   Running     : Boolean_t := True;
   Tab_Pressed : Boolean_t := False;

begin
   Lines.Append (SU.Null_Unbounded_String);

   Graphics.Clear_Screen;

   Create_World;
   Create_Chrome;
   Create_Boxes;
   Create_HTop_Entities;
   Create_Editor_Entities;

   Set_Active_Tab;
   Update_Chrome;
   Reset_Backbuffer;
   Render;

   Input_Reader.Start;

   while Running loop
      Tab_Pressed := False;

      loop
         Input_Buffer.Consume (Event);
         exit when Event.Cmd = None
            and then Event.Char_Value = Character_t'Val (0);

         --  [ and ] switch tabs — blocked while editor is in Insert mode
         if (Event.Char_Value = '[' or else Event.Char_Value = ']')
            and then (Active_Demo /= Demo_Editor or else Ed_Mode = Navigation)
         then
            if Event.Char_Value = '[' then
               Active_Demo := (if Active_Demo = Demo_ID'First
                               then Demo_ID'Last
                               else Demo_ID'Pred (Active_Demo));
            else
               Active_Demo := (if Active_Demo = Demo_ID'Last
                               then Demo_ID'First
                               else Demo_ID'Succ (Active_Demo));
            end if;
            Ed_Mode := Navigation;
            Set_Active_Tab;
            Graphics.Clear_Screen;
            Reset_Backbuffer;
            Update_Chrome;
            Render;

         elsif Active_Demo = Demo_Editor then
            --  Editor input routing
            if Ed_Mode = Insert then
               case Event.Cmd is

                  when Quit =>
                     Ed_Mode := Navigation;
                     Clamp_Col;

                  when Enter =>
                     declare
                        Cur : constant String_t :=
                           SU.To_String (Lines (Current_Line));
                        Bef : constant String_t :=
                           Cur (Cur'First .. Cur'First + Current_Col - 1);
                        Aft : constant String_t :=
                           Cur (Cur'First + Current_Col .. Cur'Last);
                     begin
                        Lines.Replace_Element
                          (Current_Line, SU.To_Unbounded_String (Bef));
                        Lines.Insert
                          (Current_Line + 1, SU.To_Unbounded_String (Aft));
                        Current_Line := Current_Line + 1;
                        Current_Col  := 0;
                        Sticky_Col   := 0;
                     end;

                  when Tab =>
                     for Sp in 1 .. 4 loop
                        declare
                           S : constant String_t :=
                              SU.To_String (Lines (Current_Line));
                        begin
                           Lines.Replace_Element
                             (Current_Line,
                              SU.To_Unbounded_String
                                (S (S'First .. S'First + Current_Col - 1)
                                 & ' '
                                 & S (S'First + Current_Col .. S'Last)));
                           Current_Col := Current_Col + 1;
                           Sticky_Col  := Current_Col;
                           Reflow_From (Current_Line);
                           if Current_Col >= Ed_Text_Width then
                              declare
                                 NL : constant Natural := Current_Line + 1;
                              begin
                                 if NL < Natural (Lines.Length) then
                                    Current_Line := NL;
                                    Current_Col  := Current_Col - Ed_Text_Width;
                                    Sticky_Col   := Current_Col;
                                 else
                                    Current_Col :=
                                       SU.Length (Lines (Current_Line));
                                    Sticky_Col := Current_Col;
                                 end if;
                              end;
                           end if;
                        end;
                     end loop;

                  when others =>
                     if Event.Char_Value = Character_t'Val (127)
                        or else Event.Char_Value = Character_t'Val (8)
                     then
                        if Current_Col > 0 then
                           declare
                              S : constant String_t :=
                                 SU.To_String (Lines (Current_Line));
                           begin
                              Lines.Replace_Element
                                (Current_Line,
                                 SU.To_Unbounded_String
                                   (S (S'First .. S'First + Current_Col - 2)
                                    & S (S'First + Current_Col .. S'Last)));
                              Current_Col := Current_Col - 1;
                              Sticky_Col  := Current_Col;
                           end;
                        elsif Current_Line > 0 then
                           declare
                              Above_Len : constant Natural :=
                                 SU.Length (Lines (Current_Line - 1));
                              Merged : constant SU.Unbounded_String :=
                                 Lines (Current_Line - 1)
                                 & Lines (Current_Line);
                           begin
                              Lines.Replace_Element (Current_Line - 1, Merged);
                              Lines.Delete (Current_Line);
                              Current_Line := Current_Line - 1;
                              Current_Col  := Above_Len;
                              Sticky_Col   := Current_Col;
                           end;
                           Reflow_From (Current_Line);
                           Reflow_Up_From (Current_Line);
                        end if;

                     elsif Event.Char_Value >= ' ' then
                        declare
                           S : constant String_t :=
                              SU.To_String (Lines (Current_Line));
                        begin
                           Lines.Replace_Element
                             (Current_Line,
                              SU.To_Unbounded_String
                                (S (S'First .. S'First + Current_Col - 1)
                                 & Event.Char_Value
                                 & S (S'First + Current_Col .. S'Last)));
                           Current_Col := Current_Col + 1;
                           Sticky_Col  := Current_Col;
                           Reflow_From (Current_Line);
                           if Current_Col >= Ed_Text_Width then
                              declare
                                 NL : constant Natural := Current_Line + 1;
                              begin
                                 if NL < Natural (Lines.Length) then
                                    Current_Line := NL;
                                    Current_Col  := Current_Col - Ed_Text_Width;
                                    Sticky_Col   := Current_Col;
                                 else
                                    Current_Col :=
                                       SU.Length (Lines (Current_Line));
                                    Sticky_Col := Current_Col;
                                 end if;
                              end;
                           end if;
                        end;
                     end if;
               end case;

            else
               --  Editor Navigation mode
               case Event.Cmd is
                  when Quit =>
                     Running := False;
                     exit;
                  when others =>
                     case Event.Char_Value is
                        when 'i' => Ed_Mode := Insert;
                        when 'w' =>
                           if Current_Line > 0 then
                              Current_Line := Current_Line - 1;
                              Current_Col  := Natural'Min
                                (Sticky_Col,
                                 SU.Length (Lines (Current_Line)));
                           end if;
                        when 's' =>
                           if Current_Line < Natural (Lines.Length) - 1 then
                              Current_Line := Current_Line + 1;
                              Current_Col  := Natural'Min
                                (Sticky_Col,
                                 SU.Length (Lines (Current_Line)));
                           end if;
                        when 'a' =>
                           if Current_Col > 0 then
                              Current_Col := Current_Col - 1;
                              Sticky_Col  := Current_Col;
                           elsif Current_Line > 0 then
                              Current_Line := Current_Line - 1;
                              Current_Col  := SU.Length (Lines (Current_Line));
                              Sticky_Col   := Current_Col;
                           end if;
                        when 'd' =>
                           declare
                              LL : constant Natural :=
                                 SU.Length (Lines (Current_Line));
                           begin
                              if Current_Col < LL then
                                 Current_Col := Current_Col + 1;
                                 Sticky_Col  := Current_Col;
                              elsif Current_Line < Natural (Lines.Length) - 1
                              then
                                 Current_Line := Current_Line + 1;
                                 Current_Col  := 0;
                                 Sticky_Col   := 0;
                              end if;
                           end;
                        when others => null;
                     end case;
               end case;
            end if;

         else
            --  Boxes / HTop input (unchanged from original)
            if Event.Cmd = Quit
               or else Event.Char_Value = Character_t'Val (27)
            then
               Running := False;
               exit;
            elsif Event.Cmd = Tab then
               Tab_Pressed := True;
            end if;
         end if;
      end loop;

      if Tab_Pressed and then Active_Demo = Demo_Boxes then
         SelectionSystem (World, Tab_Pressed => True);
         Render;
      end if;

      if Active_Demo = Demo_HTop then
         Update_HTop_Stats;
         Render;
      end if;

      if Active_Demo = Demo_Editor then
         Update_Editor_Display;
         Update_Chrome;
         Render;
      end if;

      delay 0.05;
   end loop;

   Input_Reader.Stop;
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;

end Tab_Demo;