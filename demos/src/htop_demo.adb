-------------------------------------------------------------------------------
--  ETop_Demo.adb - etop-style system monitor using Thuja ECS
-------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded;
with Graphics; use Graphics;
with Console; use Console;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;
with System_Stats;
with Input_Handling;

procedure HTop_Demo is

   package SU renames Ada.Strings.Unbounded;
   package SS renames System_Stats;
   use type SS.Platform_Type;
   use type SS.Process_Array_Ptr;

   --  Main entity system
   Entity_System : Entity_Components_PO;
   
   --  Entity IDs
   Render_Info_Entity : Entity_Id;
   Root_Widget_Entity : Entity_Id;
   Title_Entity       : Entity_Id;
   
   --  CPU widgets (8 cores max for simplicity)
   CPU_Label_Entities : array (0 .. 7) of Entity_Id;
   CPU_Bar_Entities   : array (0 .. 7) of Entity_Id;
   
   --  Memory widgets
   Mem_Label_Entity   : Entity_Id;
   RAM_Bar_Entity     : Entity_Id;
   SWP_Bar_Entity     : Entity_Id;
   
   --  Disk widgets
   Disk_Label_Entity  : Entity_Id;
   Disk_Bar_Entity    : Entity_Id;
   
   --  Process widgets (10 rows)
   Proc_Header_Entity : Entity_Id;
   Proc_Row_Entities  : array (0 .. 9) of Entity_Id;

   Update_Interval : constant Time_Span := Milliseconds(1000);
   Running : Boolean := True;
   Scroll_Offset : Natural := 0;

   function Img(N : Natural) return String is
      S : constant String := Natural'Image(N);
   begin
      return S(S'First + 1 .. S'Last);
   end Img;
   
   function Img_F1(F : Float) return String is
      W : constant Natural := Natural(Float'Floor(F));
      D : constant Natural := Natural(Float'Floor((F - Float'Floor(F)) * 10.0));
   begin
      return Img(W) & "." & Img(D);
   end Img_F1;
   
   function Pad(S : String; Len : Natural) return String is
      Result : String(1 .. Len) := (others => ' ');
      Copy_Len : constant Natural := Natural'Min(S'Length, Len);
   begin
      Result(1 .. Copy_Len) := S(S'First .. S'First + Copy_Len - 1);
      return Result;
   end Pad;

   --------------------------------------------------------
   -- Initialize ECS and Create Widgets
   --------------------------------------------------------
   procedure Initialize_Demo is
      Comps : Components_Ptr;
      
      --  RenderInfo setup
      RI : Render_Info_Component_T;
      
      --  Widget components
      Root_Widget : Widget_Component_T;
      Widget_C : Widget_Component_T;
      Text_C : Text_Component_T;
      BG_C : Background_Color_Component_T;
      PB_C : Progress_Bar_Component_T;
      
      Num_Cores : constant Natural := Natural'Min(SS.Get_CPU_Count, 8);
      Current_Row : Natural := 1;
      
   begin
      --  Initialize terminal
      Clear_Screen;
      Enable_VT_Processing;
      Set_Cursor_Visible(False);
      
      --  Create RenderInfo entity
      Render_Info_Entity := To_EID("RenderInfo");
      Comps := Add_Entity(Entity_System, Render_Info_Entity);
      
      RI.Buffers (Framebuffer_Index_t'First) := Create_Buffer (80, 50);
      RI.Buffers (Framebuffer_Index_t'Last) := Create_Buffer (80, 50);
      RI.Backbuffer := Create_Buffer(80, 50);
      RI.Drawing_FB := new Protected_DB;
      RI.Terminal_Width := 80;
      RI.Terminal_Height := 50;
      RI.Prev_Terminal_Width := 80;
      RI.Prev_Terminal_Height := 50;
      
      Add_Component(Comps.all, To_CID("RenderInfo"), RI);
      
      --  Create root widget
      Root_Widget_Entity := To_EID("RootWidget");
      Comps := Add_Entity(Entity_System, Root_Widget_Entity);
      
      Root_Widget.Position_X := 1;
      Root_Widget.Position_Y := 1;
      Root_Widget.Size_Width := 80;
      Root_Widget.Size_Height := 50;
      Root_Widget.Is_Visible := True;
      Root_Widget.Render_Buffer := Create_Buffer(80, 50);
      
      Add_Component(Comps.all, To_CID("RootWidget"), Root_Widget_Component_T'(null record));
      Add_Component(Comps.all, To_CID("Widget"), Root_Widget);
      
      BG_C.Background_Color := Black;
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      --  Title Widget
      Title_Entity := To_EID("Title");
      Comps := Add_Entity(Entity_System, Title_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Is_Visible := True;
      Widget_C.Render_Buffer := Create_Buffer(76, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String("THUJA ETOP - [j]down [k]up [q]quit");
      Text_C.Text_Color := Cyan;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component(Comps.all, To_CID("Text"), Text_C);
      BG_C.Background_Color := (0, 40, 60);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      --  Add to root
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(Title_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 2;
      
      --  CPU bars
      for C in 0 .. Num_Cores - 1 loop
         --  Label
         CPU_Label_Entities(C) := To_EID("CPULabel" & Img(C));
         Comps := Add_Entity(Entity_System, CPU_Label_Entities(C));
         
         Widget_C.Position_X := 2;
         Widget_C.Position_Y := TUI_Height(Current_Row);
         Widget_C.Size_Width := 10;
         Widget_C.Size_Height := 1;
         Widget_C.Render_Buffer := Create_Buffer(10, 1);
         
         Add_Component(Comps.all, To_CID("Widget"), Widget_C);
         
         Text_C.Text := SU.To_Unbounded_String("CPU" & Img(C) & ":");
         Text_C.Text_Color := White;
         Text_C.Offset_X := 1;
         Text_C.Offset_Y := 1;
         Text_C.Is_Bold := False;
         
         Add_Component(Comps.all, To_CID("Text"), Text_C);
         BG_C.Background_Color := (10, 20, 10);
         Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
         
         declare
            Entity_List : Entity_Components_Ptr;
            Root_Comps : Components_Ptr;
            RW : Widget_Component_T;
         begin
            Entity_System.Claim_Writing(Entity_List);
            Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
            RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
            RW.Children.Append(CPU_Label_Entities(C));
            Add_Component(Root_Comps.all, To_CID("Widget"), RW);
            Entity_System.Release_Writing;
         end;
         
         --  Bar
         CPU_Bar_Entities(C) := To_EID("CPUBar" & Img(C));
         Comps := Add_Entity(Entity_System, CPU_Bar_Entities(C));
         
         Widget_C.Position_X := 13;
         Widget_C.Position_Y := TUI_Height(Current_Row);
         Widget_C.Size_Width := 60;
         Widget_C.Size_Height := 1;
         Widget_C.Render_Buffer := Create_Buffer(60, 1);
         
         Add_Component(Comps.all, To_CID("Widget"), Widget_C);
         
         PB_C.Value := 0.0;
         PB_C.Filled_Char := '=';
         PB_C.Empty_Char := ' ';
         PB_C.Filled_Color := Green;
         PB_C.Empty_Color := Gray;
         PB_C.Show_Percentage := True;
         
         Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
         BG_C.Background_Color := (10, 20, 10);
         Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
         
         declare
            Entity_List : Entity_Components_Ptr;
            Root_Comps : Components_Ptr;
            RW : Widget_Component_T;
         begin
            Entity_System.Claim_Writing(Entity_List);
            Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
            RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
            RW.Children.Append(CPU_Bar_Entities(C));
            Add_Component(Root_Comps.all, To_CID("Widget"), RW);
            Entity_System.Release_Writing;
         end;
         
         Current_Row := Current_Row + 1;
      end loop;
      
      Current_Row := Current_Row + 1;
      
      --  Memory Label
      Mem_Label_Entity := To_EID("MemLabel");
      Comps := Add_Entity(Entity_System, Mem_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(76, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String("Memory:");
      Text_C.Text_Color := White;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component(Comps.all, To_CID("Text"), Text_C);
      BG_C.Background_Color := (20, 15, 5);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(Mem_Label_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 1;
      
      --  RAM Bar
      RAM_Bar_Entity := To_EID("RAMBar");
      Comps := Add_Entity(Entity_System, RAM_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(60, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Color := Yellow;
      
      Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
      BG_C.Background_Color := (20, 15, 5);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(RAM_Bar_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 1;
      
      --  Swap Bar
      SWP_Bar_Entity := To_EID("SWPBar");
      Comps := Add_Entity(Entity_System, SWP_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(60, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Color := Hot_Pink;
      
      Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
      BG_C.Background_Color := (20, 15, 5);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(SWP_Bar_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 2;
      
      --  Disk Label
      Disk_Label_Entity := To_EID("DiskLabel");
      Comps := Add_Entity(Entity_System, Disk_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(76, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String("Disk (/):");
      Text_C.Text_Color := White;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component(Comps.all, To_CID("Text"), Text_C);
      BG_C.Background_Color := (5, 15, 20);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(Disk_Label_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 1;
      
      --  Disk Bar
      Disk_Bar_Entity := To_EID("DiskBar");
      Comps := Add_Entity(Entity_System, Disk_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(60, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Color := Cyan;
      
      Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
      BG_C.Background_Color := (5, 15, 20);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(Disk_Bar_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 2;
      
      --  Process Header
      Proc_Header_Entity := To_EID("ProcHeader");
      Comps := Add_Entity(Entity_System, Proc_Header_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := TUI_Height(Current_Row);
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer(76, 1);
      
      Add_Component(Comps.all, To_CID("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String(
         Pad("PID", 7) & Pad("USER", 10) & Pad("CPU%", 6) & 
         Pad("MEM%", 6) & "S  COMMAND"
      );
      Text_C.Text_Color := Violet;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component(Comps.all, To_CID("Text"), Text_C);
      BG_C.Background_Color := (15, 15, 30);
      Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing(Entity_List);
         Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
         RW.Children.Append(Proc_Header_Entity);
         Add_Component(Root_Comps.all, To_CID("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      Current_Row := Current_Row + 1;
      
      --  Process Rows
      for R in 0 .. 9 loop
         Proc_Row_Entities(R) := To_EID("ProcRow" & Img(R));
         Comps := Add_Entity(Entity_System, Proc_Row_Entities(R));
         
         Widget_C.Position_X := 2;
         Widget_C.Position_Y := TUI_Height(Current_Row);
         Widget_C.Size_Width := 76;
         Widget_C.Size_Height := 1;
         Widget_C.Render_Buffer := Create_Buffer(76, 1);
         
         Add_Component(Comps.all, To_CID("Widget"), Widget_C);
         
         Text_C.Text := SU.To_Unbounded_String("");
         Text_C.Text_Color := White;
         Text_C.Offset_X := 1;
         Text_C.Offset_Y := 1;
         Text_C.Is_Bold := False;
         
         Add_Component(Comps.all, To_CID("Text"), Text_C);
         BG_C.Background_Color := Black;
         Add_Component(Comps.all, To_CID("BackgroundColor"), BG_C);
         
         declare
            Entity_List : Entity_Components_Ptr;
            Root_Comps : Components_Ptr;
            RW : Widget_Component_T;
         begin
            Entity_System.Claim_Writing(Entity_List);
            Root_Comps := Get_Entity_Components(Entity_List.all, Root_Widget_Entity);
            RW := Widget_Component_T(Get_Component(Root_Comps.all, Widget_Component_T'Tag));
            RW.Children.Append(Proc_Row_Entities(R));
            Add_Component(Root_Comps.all, To_CID("Widget"), RW);
            Entity_System.Release_Writing;
         end;
         
         Current_Row := Current_Row + 1;
      end loop;
      
      --  Warm up CPU stats
      declare
         Dummy : Float := SS.Get_CPU_Usage_Average;
      begin
         null;
      end;
      
   end Initialize_Demo;

   --------------------------------------------------------
   -- Update System Stats
   --------------------------------------------------------
   procedure Update_Stats is
      Entity_List : Entity_Components_Ptr;
      Comps : Components_Ptr;
      PB_C : Progress_Bar_Component_T;
      Text_C : Text_Component_T;
      
      Num_Cores : constant Natural := Natural'Min(SS.Get_CPU_Count, 8);
      Procs : SS.Process_Array_Ptr;
      
      --  Memory
      Tot_MB, Used_MB, Free_MB, Avail_MB,
      Buff_MB, Cache_MB, Swap_Tot_MB, Swap_Used_MB : Natural;
      Real_Used_MB : Natural;
      
      --  Disk
      Disk_Path : constant String := (if SS.Get_Platform = SS.Windows then "C:\" else "/");
      Disk_Total_GB, Disk_Used_GB : Float;
      
   begin
      Entity_System.Claim_Writing(Entity_List);
      
      --  Update CPU bars
      for C in 0 .. Num_Cores - 1 loop
         declare
            Usage : constant Float := SS.Get_CPU_Usage(C) / 100.0;
         begin
            Comps := Get_Entity_Components(Entity_List.all, CPU_Bar_Entities(C));
            if Comps /= null then
               PB_C := Progress_Bar_Component_T(
                  Get_Component(Comps.all, Progress_Bar_Component_T'Tag)
               );
               PB_C.Value := Usage;
               
               if Usage < 0.33 then
                  PB_C.Filled_Color := Green;
               elsif Usage < 0.66 then
                  PB_C.Filled_Color := Yellow;
               else
                  PB_C.Filled_Color := Red;
               end if;
               
               Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
            end if;
         end;
      end loop;
      
      --  Update Memory
      SS.Get_Memory_Detailed(Tot_MB, Used_MB, Free_MB, Avail_MB,
                             Buff_MB, Cache_MB, Swap_Tot_MB, Swap_Used_MB);
      Real_Used_MB := Tot_MB - Free_MB - Buff_MB - Cache_MB;
      
      declare
         Mem_Pct : constant Float := (if Tot_MB > 0 then Float(Real_Used_MB) / Float(Tot_MB) else 0.0);
         Swap_Pct : constant Float := (if Swap_Tot_MB > 0 then Float(Swap_Used_MB) / Float(Swap_Tot_MB) else 0.0);
      begin
         Comps := Get_Entity_Components(Entity_List.all, Mem_Label_Entity);
         if Comps /= null then
            Text_C := Text_Component_T(Get_Component(Comps.all, Text_Component_T'Tag));
            Text_C.Text := SU.To_Unbounded_String(
               "Memory: " & Img_F1(Float(Real_Used_MB) / 1024.0) & "G / " &
                            Img_F1(Float(Tot_MB) / 1024.0) & "G"
            );
            Add_Component(Comps.all, To_CID("Text"), Text_C);
         end if;
         
         Comps := Get_Entity_Components(Entity_List.all, RAM_Bar_Entity);
         if Comps /= null then
            PB_C := Progress_Bar_Component_T(Get_Component(Comps.all, Progress_Bar_Component_T'Tag));
            PB_C.Value := Mem_Pct;
            if Mem_Pct < 0.5 then PB_C.Filled_Color := Green;
            elsif Mem_Pct < 0.75 then PB_C.Filled_Color := Yellow;
            else PB_C.Filled_Color := Red;
            end if;
            Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
         end if;
         
         Comps := Get_Entity_Components(Entity_List.all, SWP_Bar_Entity);
         if Comps /= null then
            PB_C := Progress_Bar_Component_T(Get_Component(Comps.all, Progress_Bar_Component_T'Tag));
            PB_C.Value := Swap_Pct;
            if Swap_Pct < 0.5 then PB_C.Filled_Color := Green;
            elsif Swap_Pct < 0.75 then PB_C.Filled_Color := Yellow;
            else PB_C.Filled_Color := Red;
            end if;
            Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
         end if;
      end;
      
      --  Update Disk
      SS.Get_Disk_Space_GB(Disk_Path, Disk_Total_GB, Disk_Used_GB);
      declare
         Disk_Pct : constant Float := SS.Get_Disk_Usage(Disk_Path);
      begin
         Comps := Get_Entity_Components(Entity_List.all, Disk_Label_Entity);
         if Comps /= null then
            Text_C := Text_Component_T(Get_Component(Comps.all, Text_Component_T'Tag));
            Text_C.Text := SU.To_Unbounded_String(
               "Disk: " & Img_F1(Disk_Used_GB) & "G / " & Img_F1(Disk_Total_GB) & "G"
            );
            Add_Component(Comps.all, To_CID("Text"), Text_C);
         end if;
         
         Comps := Get_Entity_Components(Entity_List.all, Disk_Bar_Entity);
         if Comps /= null then
            PB_C := Progress_Bar_Component_T(Get_Component(Comps.all, Progress_Bar_Component_T'Tag));
            PB_C.Value := Disk_Pct;
            if Disk_Pct < 0.5 then PB_C.Filled_Color := Green;
            elsif Disk_Pct < 0.75 then PB_C.Filled_Color := Yellow;
            else PB_C.Filled_Color := Red;
            end if;
            Add_Component(Comps.all, To_CID("ProgressBar"), PB_C);
         end if;
      end;
      
      --  Update Processes
      Procs := SS.Get_Process_List;
      for R in 0 .. 9 loop
         declare
            Idx : constant Natural := R + Scroll_Offset;
         begin
            Comps := Get_Entity_Components(Entity_List.all, Proc_Row_Entities(R));
            if Comps /= null then
               Text_C := Text_Component_T(Get_Component(Comps.all, Text_Component_T'Tag));
               
               if Procs /= null and then Idx < Procs'Length then
                  declare
                     P : SS.Process_Info renames Procs(Idx + 1);
                     CPU_Pct : constant Natural := Natural'Min(100, Natural(P.CPU));
                     Mem_Pct : constant Natural := Natural'Min(100, Natural(P.Memory));
                     St : constant Character := (case P.State is
                        when SS.Running => 'R', when SS.Sleeping => 'S',
                        when SS.Stopped => 'T', when SS.Zombie => 'Z',
                        when SS.Unknown_State => '?');
                  begin
                     Text_C.Text := SU.To_Unbounded_String(
                        Pad(Img(P.PID), 7) &
                        Pad(SU.To_String(P.User), 10) &
                        Pad(Img(CPU_Pct) & "%", 6) &
                        Pad(Img(Mem_Pct) & "%", 6) &
                        St & "  " &
                        Pad(SU.To_String(P.Name), 30)
                     );
                     Text_C.Text_Color := (if CPU_Pct > 50 then Red elsif CPU_Pct > 20 then Gold else White);
                  end;
               else
                  Text_C.Text := SU.To_Unbounded_String("");
               end if;
               
               Add_Component(Comps.all, To_CID("Text"), Text_C);
            end if;
         end;
      end loop;
      
      if Procs /= null then
         SS.Free_Process_List(Procs);
      end if;
      
      Entity_System.Release_Writing;
   end Update_Stats;
   
   procedure Check_Keyboard is
      use Input_Handling;
      Event : Input_Event_t;
   begin
      Input_Buffer.Consume(Event);
      if Event.Char_Value = Character'Val(0) then return; end if;
      if Event.Cmd = Quit or Event.Char_Value = 'q' or Event.Char_Value = 'Q' then
         Running := False;
      elsif Event.Char_Value = 'j' or Event.Char_Value = 'J' then
         Scroll_Offset := Scroll_Offset + 1;
      elsif Event.Char_Value = 'k' or Event.Char_Value = 'K' then
         if Scroll_Offset > 0 then Scroll_Offset := Scroll_Offset - 1; end if;
      end if;
   end Check_Keyboard;
   
   task Display_Task;
   task body Display_Task is
      Next_Update : Time := Clock;
   begin
      while Running loop
         Update_Stats;
         WidgetBackgroundSystem(Entity_System);
         TextRenderSystem(Entity_System);
         ProgressBarRenderSystem(Entity_System);
         BufferCopySystem(Entity_System);
         DoubleBufferFlagSystem(Entity_System);
         BufferDrawSystem(Entity_System);
         Next_Update := Next_Update + Update_Interval;
         delay until Next_Update;
      end loop;
   end Display_Task;

   --------------------------------------------------------
   -- Main Loop
   --------------------------------------------------------
   Next_Update : Time := Clock;
   
begin
   Initialize_Demo;
   Input_Handling.Input_Reader.Start;
   
   while Running loop
      Check_Keyboard;
      delay 0.05;
   end loop;
   
   delay 0.1;
   Input_Handling.Input_Reader.Stop;
   Set_Cursor_Visible(True);
   Reset_Styling;
   Clear_Screen;

exception
   when others =>
      Running := False;
      delay 0.1;
      Input_Handling.Input_Reader.Stop;
      Set_Cursor_Visible(True);
      Reset_Styling;
      raise;
   
end HTop_Demo;
