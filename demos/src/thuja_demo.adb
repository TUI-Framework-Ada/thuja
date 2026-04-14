with Ada.Wide_Wide_Text_IO;
with Input_Handling; use Input_Handling;
with Graphics;       use Graphics;
with ECS;            use ECS;
with IDs;            use IDs;
with Components;     use Components;
with Flex_Demo;
with Text_Editor;
with Console;
use type Text_Editor.Editor_Mode_T;
with htop;           use htop;
with Sort_Demo;
with standardized_tab_interface;    -- Defines the abstract tab interface (common API for all tabs)
with Thuja_demo_tab_htop;           -- Implementation of the HTop-style monitoring tab
with Thuja_demo_tab_editor;         -- Implementation of the text editor tab
with Thuja_demo_tab_flex;           -- Implementation of the flexbox demo tab
with Thuja_demo_tab_sort;           -- Implementation of the sound of sorting demo tab
with Thuja_demo_tab_keyboard;       -- Implementation of the keyboard / Ctrl-shortcut demo tab
with Terminal_Size;                 -- C bindings to get terminal size using tput/Win32 API

procedure Thuja_Demo is

   -- Concrete tab instances declared locally within the main procedure.
   HTop_Tab     : aliased Thuja_demo_tab_htop.Tab_T;
   Editor_Tab   : aliased Thuja_demo_tab_editor.Tab_T;
   Flex_Tab     : aliased Thuja_demo_tab_flex.Tab_T;
   Sort_Tab     : aliased Thuja_demo_tab_sort.Tab_T;
   Keyboard_Tab : aliased Thuja_demo_tab_keyboard.Tab_T;

   -- Array of polymorphic tab pointers using the interface access type.
   -- Unchecked_Access is used to bypass Ada accessibility checks.
   -- Should be safe since all tab objects outlive the array usage.
   type Tab_Array is array (0 .. 4) of standardized_tab_interface.Tab_Access;
   Tabs : constant Tab_Array :=
     [0 => HTop_Tab'Unchecked_Access,
      1 => Editor_Tab'Unchecked_Access,
      2 => Flex_Tab'Unchecked_Access,
      3 => Sort_Tab'Unchecked_Access,
      4 => Keyboard_Tab'Unchecked_Access];

   -- Local type aliases for readability and consistency
   subtype String_t is String;
   subtype Boolean_t is Boolean;
   subtype Character_t is Character;

   -- Terminal layout configuration constants
   Term_Width  : constant TUI_Width := TUI_Width (Terminal_Size.Get_Width);
   Term_Height : constant TUI_Height := TUI_Height (Terminal_Size.Get_Height);
   Content_Top : constant TUI_Height := 4;

   -- ECS world instance holding all entities and components
   World : Entity_Components_PO;

   ---------------------------------------------------------------------------
   --  Chrome (UI shell: help bar, tab bar, separator)
   ---------------------------------------------------------------------------
   procedure Create_Chrome is
      Discard : Components_Ptr;
   begin
      -- Create top help bar widget
      Discard :=
        Make_Widget (World, "chrome_help", TUI_Width'First, 1, Term_Width, 1);

      -- Create tab bar widget
      Discard :=
        Make_Widget
          (World, "chrome_tabbar", TUI_Width'First, 2, Term_Width, 1);

      -- Create separator line widget
      Discard :=
        Make_Widget (World, "chrome_sep", TUI_Width'First, 3, Term_Width, 1);
   end Create_Chrome;

   procedure Update_Chrome is
      EL          : Entity_Components_Ptr;
      Current_Tab : constant Natural := Get_Active_Tab (World);

      -- Color definitions for UI styling
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

      -- Labels for tab titles
      Labels : constant array (0 .. 4) of String_t (1 .. 14) :=
        ["    HTop      ",
         "  Text Editor ",
         "   Flexbox    ",
         " Sort Visual  ",
         "   Keyboard   "];

      -- Dynamically generates help text depending on active tab/mode
      function Help_Text return String_t is
      begin
         if Current_Tab = 1 and then Text_Editor.Mode = Text_Editor.Insert then
            return
              "INSERT: type to edit  |  ESC: back to Navigation"
              & "                         ";
         elsif Current_Tab = 2 then
            return
              " [ Prev  ] Next | j: justify  a: align  +/-: width  H/h: height  | Esc: Quit";
         elsif Current_Tab = 3 then
            return Sort_Demo.Help_Text;
         elsif Current_Tab = 4 then
            return
              " [ Prev  ] Next  |  Press any key or Ctrl+letter"
              & "                            ";
         else
            return
              " [ Prev  ] Next  |  Esc: Quit"
              & "                                    ";
         end if;
      end Help_Text;

   begin
      -- Lock ECS world for safe writing
      World.Claim_Writing (EL);

      -- Update help bar content
      declare
         CP : constant Components_Ptr :=
           Get_Entity_Components (EL.all, To_EID ("chrome_help"));
         W  : Widget_Component_T :=
           Widget_Component_T
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
      begin
         Fill_Row (W.Render_Buffer, 1, Help_BG);
         Write_To_Buffer
           (W.Render_Buffer, 1, 1, Help_Text, Help_FG, Help_BG, False);
         Add_Component (CP.all, To_CID ("WidgetComponent"), W);
      end;

      -- Update tab bar with active/inactive highlighting
      declare
         CP  : constant Components_Ptr :=
           Get_Entity_Components (EL.all, To_EID ("chrome_tabbar"));
         W   : Widget_Component_T :=
           Widget_Component_T
             (Get_Component (CP.all, To_CID ("WidgetComponent")));
         Col : TUI_Width := 2;
      begin
         Fill_Row (W.Render_Buffer, 1, Tab_BG_Inactive);
         for D in 0 .. 4 loop
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

      -- Draw separator line
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

      -- Release ECS write lock
      World.Release_Writing;
   end Update_Chrome;

   procedure Toggle_HTop_Backgrounds is
      EL : Entity_Components_Ptr;
      CP : Components_Ptr;

      procedure Add_BG (Entity : Components_Ptr; Color : Color_t) is
         BG : constant Background_Color_Component_T :=
           (Background_Color => Color);
      begin
         Add_Component (Entity.all, "Background_Color", BG);
      end Add_BG;
   begin
      World.Claim_Reading (EL);

      --  CPU labels & progress bars
      for C in 0 .. Max_Cores - 1 loop
         CP :=
           Get_Entity_Components
             (EL.all, To_EID ("cpulabel" & Thuja_demo_tab_htop.Img (C)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, Thuja_demo_tab_htop.BG_cpu);
            end if;
            CP.PO.Release_List;
         end if;
         CP :=
           Get_Entity_Components
             (EL.all, To_EID ("cpubar" & Thuja_demo_tab_htop.Img (C)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, Thuja_demo_tab_htop.BG_cpu);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_mem);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_mem);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_mem);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_disk);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_disk);
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
            Add_BG (CP, Thuja_demo_tab_htop.BG_prochead);
         end if;
         CP.PO.Release_List;
      end if;

      --  Process lines
      for R in 0 .. Num_Proc_Rows - 1 loop
         CP :=
           Get_Entity_Components
             (EL.all, To_EID ("procList" & Thuja_demo_tab_htop.Img (R)));
         if CP /= null then
            CP.PO.Claim_List;
            if Has_Component (CP.all, Background_Color_Component_T'Tag) then
               Remove_Component (CP.all, Background_Color_Component_T'Tag);
            else
               Add_BG (CP, Thuja_demo_tab_htop.BG_procbody);
            end if;
            CP.PO.Release_List;
         end if;
      end loop;

      World.Release_Reading;
   end Toggle_HTop_Backgrounds;

   procedure Toggle_HTop_Processes is
      EL  : Entity_Components_Ptr;
      CP  : Components_Ptr;
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
              Thuja_demo_tab_htop.BG_prochead);
         World.Claim_Writing (EL);
         Add_Component (CP.all, To_CID ("TabPage"), Tab);
         Thuja_demo_tab_htop.Add_Text
           (CP,
            Thuja_demo_tab_htop.Pad ("PID", 7)
            & Thuja_demo_tab_htop.Pad ("USER", 10)
            & Thuja_demo_tab_htop.Pad ("CPU%", 6)
            & Thuja_demo_tab_htop.Pad ("Mem%", 6)
            & "S Command",
            White,
            True);
      end if;
      Row := Row + 1;

      for R in 0 .. Num_Proc_Rows - 1 loop
         CP :=
           Get_Entity_Components
             (EL.all, To_EID ("procList" & Thuja_demo_tab_htop.Img (R)));
         if CP /= null then
            World.Release_Writing;
            Remove_Entity
              (World, To_EID ("procList" & Thuja_demo_tab_htop.Img (R)));
            World.Claim_Writing (EL);
         else
            World.Release_Writing;
            CP :=
              Make_Widget_With_BG
                (World,
                 "procList" & Thuja_demo_tab_htop.Img (R),
                 2,
                 TUI_Height (Row),
                 76,
                 1,
                 Thuja_demo_tab_htop.BG_procbody);
            World.Claim_Writing (EL);
            Add_Component (CP.all, To_CID ("TabPage"), Tab);
            Thuja_demo_tab_htop.Add_Text (CP, "", White, False);
            Row := Row + 1;
         end if;
      end loop;

      World.Release_Writing;
   end Toggle_HTop_Processes;

   ---------------------------------------------------------------------------
   --  Render pipeline (ECS systems)
   ---------------------------------------------------------------------------
   procedure Render is
   begin
      -- Execute rendering systems in order
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
   --  Main loop and application lifecycle
   ---------------------------------------------------------------------------
   Event   : Input_Event_t;
   Running : Boolean_t := True;

begin
   -- Initialize subsystems
   Text_Editor.Initialise;
   Sort_Demo.Initialise;
   Initialise;
   Flex_Demo.Initialise (Natural (Term_Width), Natural (Term_Height));

   -- Configure console for ANSI/VT rendering
   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   -- Initialize ECS world and UI chrome
   Initialize_World (World, Term_Width, Term_Height, Tab_Count => 5);
   Create_Chrome;

   -- Initialize all tabs via polymorphic dispatch
   for I in Tabs'Range loop
      Tabs (I).Create_Entities (World, Content_Top, Term_Width, Term_Height);
   end loop;

   -- Initial render setup
   Update_Chrome;
   TabInitSystem (World);
   ResetBackbufferSystem (World);
   Render;

   -- Start input system
   Input_Reader.Start;

   -- Main event loop
   while Running loop
      ClearWidgetBufferSystem (World);
      loop
         Input_Buffer.Consume (Event);
         exit when
           Event.Cmd = None and then Event.Char_Value = Character_t'Val (0);

         -- Tab switching (disabled in insert mode)
         if (Event.Char_Value = '[' or else Event.Char_Value = ']')
           and then Text_Editor.Mode /= Text_Editor.Insert
         then
            if Event.Char_Value = '[' then
               TabSwitchSystem (World, Prev);
            else
               TabSwitchSystem (World, Next);
            end if;

         -- HTop display controls
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

         -- Text editor tab logic
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

         -- Flexbox demo controls
         elsif Get_Active_Tab (World) = 2 then
            case Event.Char_Value is
               when 'j'    =>
                  Flex_Demo.Next_Justify;

               when 'a'    =>
                  Flex_Demo.Next_Align;

               when '+'    =>
                  Flex_Demo.Grow (Natural (Term_Width) - 6);

               when '-'    =>
                  Flex_Demo.Shrink;

               when 'H'    =>
                  Flex_Demo.Grow_Height
                     (Natural (Term_Height) - Natural (Content_Top) - 5);

               when 'h'    =>
                  Flex_Demo.Shrink_Height;

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

         -- Keyboard demo: forward every event to the tab so it can
         -- highlight keys and detect Ctrl/Alt shortcuts.  Double-ESC
         -- still quits via Event.Cmd = Quit.
         elsif Get_Active_Tab (World) = 4 then
            if Event.Cmd = Quit
              and then Event.Char_Value = Character_t'Val (27)
            then
               Running := False;
               exit;
            end if;
            Thuja_demo_tab_keyboard.Handle_Event (World, Event);

         -- Global quit handling
         else
            if Event.Cmd = Quit or else Event.Char_Value = Character_t'Val (27)
            then
               Running := False;
               exit;
            end if;
         end if;
      end loop;

      -- Update active tab and re-render
      declare
         Active : constant Natural := Get_Active_Tab (World);
      begin
         Tabs (Active).Update (World);
      end;

      --  TODO: Merge into Tabs
--      if Get_Active_Tab (World) = 3 then
--         Update_Sort_Display;
--      end if;

      Render;

      delay 0.05;
   end loop;

   -- Shutdown and restore console state
   Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;
   Ada.Wide_Wide_Text_IO.Flush;

end Thuja_Demo;
