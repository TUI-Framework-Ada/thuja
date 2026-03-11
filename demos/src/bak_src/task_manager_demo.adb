-------------------------------------------------------------------------------
--  Task_Manager_Demo.adb
--  
--  System monitoring demo using Thuja's ECS framework
--  Displays CPU, Memory, Disk usage with real-time updates using Widgets
-------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded;
with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;
with System_Stats;

procedure Task_Manager_Demo is

   package SU renames Ada.Strings.Unbounded;
   package SS renames System_Stats;

   --  Main entity system
   Entity_System : Entity_Components_PO;
   
   --  Entity IDs
   Render_Info_Entity : Entity_Id;
   Root_Widget_Entity : Entity_Id;
   
   --  System monitor widget entities
   Title_Widget_Entity : Entity_Id;
   CPU_Label_Entity : Entity_Id;
   CPU_Bar_Entity : Entity_Id;
   Mem_Label_Entity : Entity_Id;
   Mem_Bar_Entity : Entity_Id;
   Disk_Label_Entity : Entity_Id;
   Disk_Bar_Entity : Entity_Id;
   Info_Label_Entity : Entity_Id;

   Update_Interval : constant Time_Span := Milliseconds(1000);
   Running : constant Boolean := True;

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
      
   begin
      --  Initialize terminal
      Clear_Screen;
      Enable_VT_Processing;
      Set_Cursor_Visible (False);
      
      --  Create RenderInfo entity
      Render_Info_Entity := To_EID ("RenderInfo");
      Comps := Add_Entity (Entity_System, Render_Info_Entity);
      
      RI.Framebuffer_1 := Create_Buffer (80, 50);
      RI.Framebuffer_2 := Create_Buffer (80, 50);
      RI.Backbuffer := Create_Buffer (80, 50);
      RI.Drawing_FB := new Protected_DB;
      RI.Terminal_Width := 80;
      RI.Terminal_Height := 50;
      RI.Prev_Terminal_Width := 80;
      RI.Prev_Terminal_Height := 50;
      
      Add_Component (Comps.all, To_CID ("RenderInfo"), RI);
      
      --  Create root widget
      Root_Widget_Entity := To_EID ("RootWidget");
      Comps := Add_Entity (Entity_System, Root_Widget_Entity);
      
      Root_Widget.Position_X := 1;
      Root_Widget.Position_Y := 1;
      Root_Widget.Size_Width := 80;
      Root_Widget.Size_Height := 50;
      Root_Widget.Is_Visible := True;
      Root_Widget.Render_Buffer := Create_Buffer (80, 50);
      
      Add_Component (Comps.all, To_CID ("RootWidget"), Root_Widget_Component_T'(null record));
      Add_Component (Comps.all, To_CID ("Widget"), Root_Widget);
      
      BG_C.Background_Color := Black;
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      --  Title Widget
      Title_Widget_Entity := To_EID ("TitleWidget");
      Comps := Add_Entity (Entity_System, Title_Widget_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 2;
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 3;
      Widget_C.Is_Visible := True;
      Widget_C.Render_Buffer := Create_Buffer (76, 3);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String ("=== THUJA SYSTEM MONITOR ===");
      Text_C.Text_Color := Cyan;
      Text_C.Offset_X := 20;
      Text_C.Offset_Y := 2;
      Text_C.Is_Bold := True;
      
      Add_Component (Comps.all, To_CID ("Text"), Text_C);
      
      BG_C.Background_Color := Black;
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      --  Add to root children
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Title_Widget_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  CPU Label Widget
      CPU_Label_Entity := To_EID ("CPULabel");
      Comps := Add_Entity (Entity_System, CPU_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 6;
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (76, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String ("CPU Usage:");
      Text_C.Text_Color := White;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component (Comps.all, To_CID ("Text"), Text_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (CPU_Label_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  CPU Progress Bar Widget
      CPU_Bar_Entity := To_EID ("CPUBar");
      Comps := Add_Entity (Entity_System, CPU_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 7;
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (60, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Char := '=';
      PB_C.Empty_Char := ' ';
      PB_C.Filled_Color := Green;
      PB_C.Empty_Color := Gray;
      PB_C.Show_Percentage := True;
      
      Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (CPU_Bar_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Memory Label Widget
      Mem_Label_Entity := To_EID ("MemLabel");
      Comps := Add_Entity (Entity_System, Mem_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 9;
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (76, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String ("Memory Usage:");
      Text_C.Text_Color := White;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component (Comps.all, To_CID ("Text"), Text_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Mem_Label_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Memory Progress Bar Widget
      Mem_Bar_Entity := To_EID ("MemBar");
      Comps := Add_Entity (Entity_System, Mem_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 10;
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (60, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Color := Yellow;
      
      Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Mem_Bar_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Disk Label Widget
      Disk_Label_Entity := To_EID ("DiskLabel");
      Comps := Add_Entity (Entity_System, Disk_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 12;
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (76, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String ("Disk Usage (/):");
      Text_C.Text_Color := White;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := True;
      
      Add_Component (Comps.all, To_CID ("Text"), Text_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Disk_Label_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Disk Progress Bar Widget
      Disk_Bar_Entity := To_EID ("DiskBar");
      Comps := Add_Entity (Entity_System, Disk_Bar_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 13;
      Widget_C.Size_Width := 60;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (60, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      PB_C.Value := 0.0;
      PB_C.Filled_Color := Cyan;
      
      Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Disk_Bar_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Info Label Widget
      Info_Label_Entity := To_EID ("InfoLabel");
      Comps := Add_Entity (Entity_System, Info_Label_Entity);
      
      Widget_C.Position_X := 2;
      Widget_C.Position_Y := 15;
      Widget_C.Size_Width := 76;
      Widget_C.Size_Height := 1;
      Widget_C.Render_Buffer := Create_Buffer (76, 1);
      
      Add_Component (Comps.all, To_CID ("Widget"), Widget_C);
      
      Text_C.Text := SU.To_Unbounded_String ("Cores: 0 | Uptime: 0s");
      Text_C.Text_Color := Gray;
      Text_C.Offset_X := 1;
      Text_C.Offset_Y := 1;
      Text_C.Is_Bold := False;
      
      Add_Component (Comps.all, To_CID ("Text"), Text_C);
      Add_Component (Comps.all, To_CID ("BackgroundColor"), BG_C);
      
      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comps : Components_Ptr;
         RW : Widget_Component_T;
      begin
         Entity_System.Claim_Writing (Entity_List);
         Root_Comps := Get_Entity_Components (Entity_List.all, Root_Widget_Entity);
         RW := Widget_Component_T (
            Get_Component (Root_Comps.all, Widget_Component_T'Tag)
         );
         RW.Children.Append (Info_Label_Entity);
         Add_Component (Root_Comps.all, To_CID ("Widget"), RW);
         Entity_System.Release_Writing;
      end;
      
      --  Initialize CPU stats (first call returns 0)
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
      
      CPU_Usage : Float;
      Mem_Usage : Float;
      Disk_Usage : Float;
      Num_Cores : Natural;
      Uptime : Natural;
      
      Total_Mem, Used_Mem, Avail_Mem : Float;
      
      function Format_Uptime (Seconds : Natural) return String is
         Days : constant Natural := Seconds / 86400;
         Hours : constant Natural := (Seconds mod 86400) / 3600;
         Minutes : constant Natural := (Seconds mod 3600) / 60;
      begin
         if Days > 0 then
            return Natural'Image(Days) & "d " & Natural'Image(Hours) & "h";
         elsif Hours > 0 then
            return Natural'Image(Hours) & "h " & Natural'Image(Minutes) & "m";
         else
            return Natural'Image(Minutes) & "m";
         end if;
      end Format_Uptime;
      
   begin
      --  Get system stats
      CPU_Usage := SS.Get_CPU_Usage_Average;
      Mem_Usage := SS.Get_Memory_Usage;
      Disk_Usage := SS.Get_Disk_Usage ("/");
      Num_Cores := SS.Get_CPU_Count;
      Uptime := SS.Get_Uptime;
      SS.Get_Memory_GB (Total_Mem, Used_Mem, Avail_Mem);
      
      --  Claim write access to entity list
      Entity_System.Claim_Writing (Entity_List);
      
      --  Update CPU bar
      Comps := Get_Entity_Components (Entity_List.all, CPU_Bar_Entity);
      if Comps /= null then
         PB_C := Progress_Bar_Component_T (
            Get_Component (Comps.all, Progress_Bar_Component_T'Tag)
         );
         PB_C.Value := CPU_Usage;
         
         --  Color based on usage
         if CPU_Usage < 0.5 then
            PB_C.Filled_Color := Green;
         elsif CPU_Usage < 0.75 then
            PB_C.Filled_Color := Yellow;
         else
            PB_C.Filled_Color := Red;
         end if;
         
         Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      end if;
      
      --  Update Memory bar
      Comps := Get_Entity_Components (Entity_List.all, Mem_Bar_Entity);
      if Comps /= null then
         PB_C := Progress_Bar_Component_T (
            Get_Component (Comps.all, Progress_Bar_Component_T'Tag)
         );
         PB_C.Value := Mem_Usage;
         
         if Mem_Usage < 0.5 then
            PB_C.Filled_Color := Green;
         elsif Mem_Usage < 0.75 then
            PB_C.Filled_Color := Yellow;
         else
            PB_C.Filled_Color := Red;
         end if;
         
         Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      end if;
      
      --  Update Disk bar
      Comps := Get_Entity_Components (Entity_List.all, Disk_Bar_Entity);
      if Comps /= null then
         PB_C := Progress_Bar_Component_T (
            Get_Component (Comps.all, Progress_Bar_Component_T'Tag)
         );
         PB_C.Value := Disk_Usage;
         
         if Disk_Usage < 0.5 then
            PB_C.Filled_Color := Green;
         elsif Disk_Usage < 0.75 then
            PB_C.Filled_Color := Yellow;
         else
            PB_C.Filled_Color := Red;
         end if;
         
         Add_Component (Comps.all, To_CID ("ProgressBar"), PB_C);
      end if;
      
      --  Update Info Label
      Comps := Get_Entity_Components (Entity_List.all, Info_Label_Entity);
      if Comps /= null then
         Text_C := Text_Component_T (
            Get_Component (Comps.all, Text_Component_T'Tag)
         );
         Text_C.Text := SU.To_Unbounded_String (
            "Cores:" & Natural'Image(Num_Cores) & 
            " | Uptime: " & Format_Uptime(Uptime) &
            " | Memory:" & Natural'Image(Integer(Used_Mem)) & "/" & 
            Natural'Image(Integer(Total_Mem)) & "GB"
         );
         Add_Component (Comps.all, To_CID ("Text"), Text_C);
      end if;
      
      --  Release write access
      Entity_System.Release_Writing;
      
   end Update_Stats;

   --------------------------------------------------------
   -- Main Loop
   --------------------------------------------------------
   Next_Update : Time := Clock;
   
begin
   Initialize_Demo;
   
   while Running loop
      --  Update stats
      Update_Stats;
      
      --  Run ECS systems
      WidgetBackgroundSystem (Entity_System);
      TextRenderSystem (Entity_System);
      ProgressBarRenderSystem (Entity_System);
      BufferCopySystem (Entity_System);
      BufferDrawSystem (Entity_System);
      DoubleBufferFlagSystem (Entity_System);
      
      --  Wait for next update
      Next_Update := Next_Update + Update_Interval;
      delay until Next_Update;
   end loop;

exception
   when others =>
      Set_Cursor_Visible (True);
      Reset_Styling;
      Clear_Screen;
      raise;
      
end Task_Manager_Demo;