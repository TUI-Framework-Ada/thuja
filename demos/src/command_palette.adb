--  command_palette.adb
--  Demonstrates per-widget command sets with tab cycling.
--  7 panels: 6 selectable (each with unique command sets),
--  1 non-selectable info panel.  Status lines show available
--  commands, pending key buffer, and last activated command.
--  Activated commands flash the panel bright yellow for 0.5s.

with Input_Handling; use Input_Handling;
with Graphics; use Graphics;
with ECS; use ECS;
with IDs; use IDs;
with Components; use Components;
with Selection;
use type Selection.Result_Kind_t;
with Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;

procedure Command_Palette is

   package SU renames Ada.Strings.Unbounded;

   subtype String_t is String;
   subtype Character_t is Character;
   subtype Natural_t is Natural;
   subtype Boolean_t is Boolean;

   --  Terminal dimensions
   Term_Width  : constant TUI_Width  := 80;
   Term_Height : constant TUI_Height := 24;

   --  ECS world
   World : Entity_Components_PO;

   ---------------------------------------------------------------------------
   --  Panel definitions
   ---------------------------------------------------------------------------

   type Panel_Info is record
      Name   : SU.Unbounded_String;
      Label  : SU.Unbounded_String;
      X      : TUI_Width;
      Y      : TUI_Height;
      W      : TUI_Width;
      H      : TUI_Height;
      Color  : Color_t;
      Order  : Natural;
   end record;

   Panels : constant array (1 .. 6) of Panel_Info := (
      (Name  => SU.To_Unbounded_String ("panel_a"),
       Label => SU.To_Unbounded_String ("Files"),
       X => 2,  Y => 4,  W => 24, H => 4,
       Color => Steel_Blue, Order => 1),

      (Name  => SU.To_Unbounded_String ("panel_b"),
       Label => SU.To_Unbounded_String ("View"),
       X => 28, Y => 4,  W => 24, H => 4,
       Color => (Red => 180, Green => 80, Blue => 80), Order => 2),

      (Name  => SU.To_Unbounded_String ("panel_c"),
       Label => SU.To_Unbounded_String ("Sensors"),
       X => 54, Y => 4,  W => 24, H => 5,
       Color => (Red => 80, Green => 160, Blue => 80), Order => 3),

      (Name  => SU.To_Unbounded_String ("panel_d"),
       Label => SU.To_Unbounded_String ("Network"),
       X => 2,  Y => 10, W => 26, H => 5,
       Color => Gold, Order => 4),

      (Name  => SU.To_Unbounded_String ("panel_e"),
       Label => SU.To_Unbounded_String ("Build"),
       X => 30, Y => 10, W => 22, H => 6,
       Color => Purple, Order => 5),

      (Name  => SU.To_Unbounded_String ("panel_f"),
       Label => SU.To_Unbounded_String ("Config"),
       X => 54, Y => 10, W => 24, H => 5,
       Color => Teal, Order => 6)
   );

   --  Original panel colors (for reverting after flash)
   Panel_Default_Colors : array (1 .. 6) of Color_t;

   --  Flash state
   Flash_Color   : constant Color_t := (Red => 255, Green => 255, Blue => 100);
   Flash_Panel   : Natural := 0;    --  0 = no flash active
   Flash_Start   : Ada.Calendar.Time;
   Flash_Duration : constant Duration := 0.5;

   ---------------------------------------------------------------------------
   --  Command set helpers
   ---------------------------------------------------------------------------

   function Cmd (Keys : String_t; Name : String_t) return Widget_Command_Entry_T is
   begin
      return (Keys => SU.To_Unbounded_String (Keys),
              Name => SU.To_Unbounded_String (Name));
   end Cmd;

   --  Build the command vector for each panel
   function Panel_Commands (Index : Positive) return Widget_Command_Vectors.Vector is
      V : Widget_Command_Vectors.Vector;
   begin
      case Index is
         when 1 =>  --  Files
            V.Append (Cmd ("n", "New File"));
            V.Append (Cmd ("s", "Save"));
            V.Append (Cmd ("e", "Export"));
            V.Append (Cmd ("rn", "Rename"));
            V.Append (Cmd ("ff", "Find File"));
            V.Append (Cmd ("del", "Delete All"));
         when 2 =>  --  View
            V.Append (Cmd ("i", "Zoom In"));
            V.Append (Cmd ("o", "Zoom Out"));
            V.Append (Cmd ("d", "Dark Mode"));
            V.Append (Cmd ("gt", "Grid Toggle"));
            V.Append (Cmd ("jl", "Jump To Line"));
            V.Append (Cmd ("max", "Fullscreen"));
         when 3 =>  --  Sensors
            V.Append (Cmd ("u", "Update"));
            V.Append (Cmd ("r", "Read Sensor"));
            V.Append (Cmd ("l", "List All"));
            V.Append (Cmd ("pl", "Plot Data"));
            V.Append (Cmd ("nm", "New Marker"));
            V.Append (Cmd ("cal", "Calibrate"));
         when 4 =>  --  Network
            V.Append (Cmd ("t", "Trace Route"));
            V.Append (Cmd ("p", "Ping Host"));
            V.Append (Cmd ("h", "Host Info"));
            V.Append (Cmd ("dn", "DNS Lookup"));
            V.Append (Cmd ("cv", "Connect VPN"));
            V.Append (Cmd ("lsn", "List Nodes"));
         when 5 =>  --  Build
            V.Append (Cmd ("m", "Make"));
            V.Append (Cmd ("r", "Run"));
            V.Append (Cmd ("c", "Clean"));
            V.Append (Cmd ("wt", "Watch Tests"));
            V.Append (Cmd ("df", "Deploy Fast"));
            V.Append (Cmd ("log", "View Log"));
         when 6 =>  --  Config
            V.Append (Cmd ("v", "Validate"));
            V.Append (Cmd ("e", "Edit Config"));
            V.Append (Cmd ("s", "Show All"));
            V.Append (Cmd ("as", "Auto Save"));
            V.Append (Cmd ("qw", "Quick Wizard"));
            V.Append (Cmd ("rst", "Reset Defaults"));
         when others =>
            null;
      end case;
      return V;
   end Panel_Commands;

   --  Format a command list as "key:Name key:Name ..."
   function Format_Commands (Index : Positive) return String_t is
      Cmds   : constant Widget_Command_Vectors.Vector := Panel_Commands (Index);
      Result : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      for I in 0 .. Natural_t (Cmds.Length) - 1 loop
         if I > 0 then
            SU.Append (Result, "  ");
         end if;
         SU.Append (Result, SU.To_String (Cmds (I).Keys));
         SU.Append (Result, ":");
         SU.Append (Result, SU.To_String (Cmds (I).Name));
      end loop;
      return SU.To_String (Result);
   end Format_Commands;

   ---------------------------------------------------------------------------
   --  Entity creation
   ---------------------------------------------------------------------------

   procedure Create_Render_Info is
      Comp_Ptr    : Components_Ptr;
      RI          : Render_Info_Component_T;
      Root_W      : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width  := Term_Width;
      RI.Terminal_Height := Term_Height;
      RI.Prev_Terminal_Width  := Natural (Term_Width);
      RI.Prev_Terminal_Height := Natural (Term_Height);
      RI.Backbuffer    := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_1 := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_2 := Create_Buffer (Term_Width, Term_Height);
      RI.Drawing_FB    := new Protected_DB;

      --  Sentinel backbuffer for full initial render
      for X in TUI_Width'First .. Term_Width loop
         for Y in TUI_Height'First .. Term_Height loop
            Set_Buffer_Pixel (RI.Backbuffer, X, Y,
               (Char             => Character_t'Val (1),
                Char_Color       => White,
                Background_Color => White,
                Is_Bold          => True,
                Is_Italic        => False,
                Is_Underline     => False,
                Is_Strikethrough => False));
         end loop;
      end loop;

      Add_Component (Comp_Ptr.all, To_CID ("RenderInfo"), RI);

      --  Root widget
      Comp_Ptr := Add_Entity (World, To_EID ("root"));
      Root_W.Position_X    := TUI_Width'First;
      Root_W.Position_Y    := TUI_Height'First;
      Root_W.Size_Width    := Term_Width;
      Root_W.Size_Height   := Term_Height;
      Root_W.Render_Buffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Root_W);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_Marker);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
         Background_Color_Component_T'(Component_T with
            Background_Color => Black));
   end Create_Render_Info;

   --  Create a simple text entity (status lines, info panel text)
   procedure Create_Text_Entity (
      Entity_Name  : String_t;
      X            : TUI_Width;
      Y            : TUI_Height;
      Width        : TUI_Width;
      Initial_Text : String_t := "";
      FG_Color     : Color_t := White;
      BG_Color     : Color_t := Black
   ) is
      Comp_Ptr : Components_Ptr;
      Widget   : Widget_Component_T;
      Txt      : Text_Component_T;
      BG       : Background_Color_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID (Entity_Name));

      Widget.Position_X    := X;
      Widget.Position_Y    := Y;
      Widget.Size_Width    := Width;
      Widget.Size_Height   := 1;
      Widget.Is_Visible    := True;
      Widget.Render_Buffer := Create_Buffer (Width, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      Txt.Text       := SU.To_Unbounded_String (Initial_Text);
      Txt.Text_Color := FG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Txt);

      BG.Background_Color := BG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
   end Create_Text_Entity;

   --  Update text content of a text entity (caller must hold lock)
   procedure Update_Text (
      Entity_List : Entity_Components_Ptr;
      Entity_Name : String_t;
      New_Text    : String_t
   ) is
      Comp_Ptr : Components_Ptr;
      Txt      : Text_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List.all, To_EID (Entity_Name));
      if Comp_Ptr /= null and then
         Has_Component (Comp_Ptr.all, To_CID ("TextComponent"))
      then
         Txt := Text_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("TextComponent")));
         Txt.Text := SU.To_Unbounded_String (New_Text);
         Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Txt);
      end if;
   end Update_Text;

   --  Create the non-selectable info panel
   procedure Create_Info_Panel is
      Comp_Ptr : Components_Ptr;
      W        : Widget_Component_T;
      BG       : Background_Color_Component_T;
      Txt      : Text_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID ("info_panel"));

      W.Position_X    := 2;
      W.Position_Y    := 2;
      W.Size_Width    := 76;
      W.Size_Height   := 1;
      W.Is_Visible    := True;
      W.Render_Buffer := Create_Buffer (76, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), W);

      BG.Background_Color := (Red => 60, Green => 60, Blue => 60);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);

      Txt.Text := SU.To_Unbounded_String (
         " Non-selectable | ESC: Quit | TAB: Cycle focus | Type keys for commands");
      Txt.Text_Color := White;
      Txt.Is_Bold := True;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Txt);
   end Create_Info_Panel;

   --  Create all 6 selectable panels with command sets
   procedure Create_Panels is
      Entity_List : Entity_Components_Ptr;
      Root_Comp   : Components_Ptr;
      Root_W      : Widget_Component_T;
   begin
      for I in Panels'Range loop
         declare
            P        : Panel_Info renames Panels (I);
            Name     : constant String_t := SU.To_String (P.Name);
            Comp_Ptr : Components_Ptr;
            W        : Widget_Component_T;
            BG       : Background_Color_Component_T;
            Txt      : Text_Component_T;
            Sel      : Selectable_Component_T;
            Cmd_Set  : Command_Set_Component_T;
         begin
            Comp_Ptr := Add_Entity (World, To_EID (Name));

            W.Position_X    := P.X;
            W.Position_Y    := P.Y;
            W.Size_Width    := P.W;
            W.Size_Height   := P.H;
            W.Has_Focus     := (P.Order = 1);
            W.Is_Visible    := True;
            W.Render_Buffer := Create_Buffer (P.W, P.H);
            Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), W);

            BG.Background_Color := P.Color;
            Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);

            --  Panel label at top-left
            Txt.Text       := P.Label;
            Txt.Text_Color := White;
            Txt.Is_Bold    := True;
            Txt.Offset_X   := 1;
            Txt.Offset_Y   := TUI_Height'First;
            Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Txt);

            --  Selectable component
            Sel.Tab_Order := P.Order;
            Add_Component (Comp_Ptr.all, To_CID ("SelectableComponent"), Sel);

            --  Command set component
            Cmd_Set.Commands := Panel_Commands (I);
            Add_Component (Comp_Ptr.all, To_CID ("CommandSetComponent"), Cmd_Set);

            --  Store default color for flash revert
            Panel_Default_Colors (I) := P.Color;
         end;
      end loop;

      --  Register all entities as children of root
      World.Claim_Writing (Entity_List);
      Root_Comp := Get_Entity_Components (Entity_List.all, To_EID ("root"));
      Root_W := Widget_Component_T (
         Get_Component (Root_Comp.all, To_CID ("WidgetComponent")));

      Root_W.Children.Append (To_EID ("info_panel"));
      for I in Panels'Range loop
         Root_W.Children.Append (To_EID (SU.To_String (Panels (I).Name)));
      end loop;
      Root_W.Children.Append (To_EID ("commands_line"));
      Root_W.Children.Append (To_EID ("buffer_line"));
      Root_W.Children.Append (To_EID ("status_line"));

      Add_Component (Root_Comp.all, To_CID ("WidgetComponent"), Root_W);
      World.Release_Writing;
   end Create_Panels;

   --  Activate command table for the initially focused panel (Panel A)
   procedure Activate_Initial_Commands is
   begin
      Selection.Activate_Widget_Commands (Panel_Commands (1));
   end Activate_Initial_Commands;

   ---------------------------------------------------------------------------
   --  Flash management
   ---------------------------------------------------------------------------

   --  Start a flash on a panel (caller must hold lock)
   procedure Start_Flash (
      Entity_List : Entity_Components_Ptr;
      Panel_Index : Positive
   ) is
      Name     : constant String_t := SU.To_String (Panels (Panel_Index).Name);
      Comp_Ptr : Components_Ptr;
      BG       : Background_Color_Component_T;
   begin
      Flash_Panel := Panel_Index;
      Flash_Start := Ada.Calendar.Clock;

      Comp_Ptr := Get_Entity_Components (Entity_List.all, To_EID (Name));
      if Comp_Ptr /= null then
         BG.Background_Color := Flash_Color;
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;
   end Start_Flash;

   --  Check and revert flash if duration elapsed (caller must hold lock)
   --  Returns True if the flash was reverted (needs re-render)
   function Check_Flash_Revert (
      Entity_List : Entity_Components_Ptr
   ) return Boolean_t is
      Name     : String_t (1 .. 20);
      Name_Len : Natural_t;
      Comp_Ptr : Components_Ptr;
      BG       : Background_Color_Component_T;
   begin
      if Flash_Panel = 0 then
         return False;
      end if;

      if Ada.Calendar.Clock - Flash_Start >= Flash_Duration then
         declare
            Full_Name : constant String_t :=
               SU.To_String (Panels (Flash_Panel).Name);
         begin
            Name_Len := Full_Name'Length;
            Name (1 .. Name_Len) := Full_Name;

            Comp_Ptr := Get_Entity_Components (
               Entity_List.all, To_EID (Name (1 .. Name_Len)));
            if Comp_Ptr /= null then
               BG.Background_Color := Panel_Default_Colors (Flash_Panel);
               Add_Component (Comp_Ptr.all,
                  To_CID ("BackgroundColorComponent"), BG);
            end if;
         end;
         Flash_Panel := 0;
         return True;
      end if;

      return False;
   end Check_Flash_Revert;

   --  Find which panel index is currently focused
   function Focused_Panel_Index (
      Entity_List : Entity_Components_Ptr
   ) return Natural is
      Comp_Ptr : Components_Ptr;
   begin
      for I in Panels'Range loop
         Comp_Ptr := Get_Entity_Components (
            Entity_List.all, To_EID (SU.To_String (Panels (I).Name)));
         if Comp_Ptr /= null and then
            Has_Component (Comp_Ptr.all, To_CID ("WidgetComponent"))
         then
            declare
               W : Widget_Component_T renames Widget_Component_T (
                  Get_Component_Ptr (Comp_Ptr, Widget_Component_T'Tag).all);
            begin
               if W.Has_Focus then
                  return I;
               end if;
            end;
         end if;
      end loop;
      return 0;
   end Focused_Panel_Index;

   --  Update the commands_line to show the focused panel's commands
   procedure Update_Commands_Display (
      Entity_List : Entity_Components_Ptr;
      Focus_Idx   : Natural
   ) is
   begin
      if Focus_Idx in Panels'Range then
         Update_Text (Entity_List, "commands_line",
            "[" & SU.To_String (Panels (Focus_Idx).Label) & "] " &
            Format_Commands (Focus_Idx));
      else
         Update_Text (Entity_List, "commands_line", "No panel focused");
      end if;
   end Update_Commands_Display;

   ---------------------------------------------------------------------------
   --  Focus border — paint the outer edge of the focused panel red
   ---------------------------------------------------------------------------

   Focus_Border_Color : constant Color_t := Red;

   procedure Draw_Focus_Border is
      Entity_List : Entity_Components_Ptr;
      Comp_Ptr    : Components_Ptr;
      Px          : Pixel_t;
   begin
      World.Claim_Reading (Entity_List);

      for I in Panels'Range loop
         Comp_Ptr := Get_Entity_Components (
            Entity_List.all, To_EID (SU.To_String (Panels (I).Name)));
         if Comp_Ptr /= null and then
            Has_Component (Comp_Ptr.all, Widget_Component_T'Tag)
         then
            declare
               W : Widget_Component_T renames Widget_Component_T (
                  Get_Component_Ptr (Comp_Ptr, Widget_Component_T'Tag).all);
            begin
               if W.Has_Focus then
                  --  Top and bottom rows
                  for X in TUI_Width'First .. W.Size_Width loop
                     Px := Get_Buffer_Pixel (W.Render_Buffer, X, TUI_Height'First);
                     Px.Background_Color := Focus_Border_Color;
                     Set_Buffer_Pixel (W.Render_Buffer, X, TUI_Height'First, Px);

                     Px := Get_Buffer_Pixel (W.Render_Buffer, X, W.Size_Height);
                     Px.Background_Color := Focus_Border_Color;
                     Set_Buffer_Pixel (W.Render_Buffer, X, W.Size_Height, Px);
                  end loop;

                  --  Left and right columns (inner rows only)
                  for Y in TUI_Height'First + 1 .. W.Size_Height - 1 loop
                     Px := Get_Buffer_Pixel (W.Render_Buffer, TUI_Width'First, Y);
                     Px.Background_Color := Focus_Border_Color;
                     Set_Buffer_Pixel (W.Render_Buffer, TUI_Width'First, Y, Px);

                     Px := Get_Buffer_Pixel (W.Render_Buffer, W.Size_Width, Y);
                     Px.Background_Color := Focus_Border_Color;
                     Set_Buffer_Pixel (W.Render_Buffer, W.Size_Width, Y, Px);
                  end loop;

                  exit;  --  Only one panel can be focused
               end if;
            end;
         end if;
      end loop;

      World.Release_Reading;
   end Draw_Focus_Border;

   ---------------------------------------------------------------------------
   --  Render pipeline
   ---------------------------------------------------------------------------

   procedure Render is
   begin
      WidgetBackgroundSystem (World);
      Draw_Focus_Border;
      TextRenderSystem (World);
      BufferCopySystem (World);
      DoubleBufferFlagSystem (World);
      BufferDrawSystem (World);
   end Render;

   ---------------------------------------------------------------------------
   --  Main loop
   ---------------------------------------------------------------------------

   Event        : Input_Event_t;
   Running      : Boolean_t := True;
   Needs_Render : Boolean_t := False;
   Tab_Pressed  : Boolean_t := False;
   Entity_List  : Entity_Components_Ptr;
   Cmd_Result   : Selection.Handler_Result_t;
   Focus_Idx    : Natural;
   Buf_Str      : SU.Unbounded_String;

begin
   Graphics.Clear_Screen;

   --  Create all entities
   Create_Render_Info;

   --  Create status lines before panels (panels register them as children)
   Create_Text_Entity ("commands_line", 2, 18, 76,
      "[Files] " & Format_Commands (1), White, Black);
   Create_Text_Entity ("buffer_line", 2, 20, 76, "Buffer:", Gray, Black);
   Create_Text_Entity ("status_line", 2, 22, 76, "Ready.", White, Black);

   Create_Info_Panel;
   Create_Panels;
   Activate_Initial_Commands;

   --  Initial render
   Render;

   --  Start input reader
   Input_Reader.Start;

   --  Main loop
   while Running loop
      Tab_Pressed  := False;
      Needs_Render := False;

      --  Drain all pending input events
      loop
         Input_Buffer.Consume (Event);
         exit when Event.Cmd = None and Event.Char_Value = Character_t'Val (0);

         --  Global: ESC quits
         if Event.Cmd = Quit or Event.Char_Value = Character_t'Val (27) then
            Running := False;
            exit;
         end if;

         --  Global: Tab cycles focus
         if Event.Cmd = Tab then
            Tab_Pressed := True;

         --  Feed printable keys into the per-widget command state machine
         elsif Event.Char_Value >= ' ' and Event.Char_Value <= '~' then
            Cmd_Result := Selection.Process_Key (Event.Char_Value);

            if Cmd_Result.Kind = Selection.Command_Activated then
               --  A command was completed
               World.Claim_Writing (Entity_List);

               Focus_Idx := Focused_Panel_Index (Entity_List);
               if Focus_Idx in Panels'Range then
                  Start_Flash (Entity_List, Focus_Idx);
               end if;

               --  Update status line
               Update_Text (Entity_List, "status_line",
                  "Activated: " &
                  SU.To_String (Cmd_Result.Command_Name) &
                  " (" & SU.To_String (Cmd_Result.Keys) & ")");

               --  Clear buffer display
               Update_Text (Entity_List, "buffer_line", "Buffer:");

               World.Release_Writing;
               Needs_Render := True;

            elsif Cmd_Result.Kind = Selection.Keys_Passed_Through then
               --  Keys didn't match any command — clear buffer display
               World.Claim_Writing (Entity_List);
               Update_Text (Entity_List, "buffer_line", "Buffer: (no match)");
               World.Release_Writing;
               Needs_Render := True;

            else
               --  No_Result: still buffering, update buffer display
               Buf_Str := Selection.Get_Pending_Buffer;
               if SU.Length (Buf_Str) > 0 then
                  World.Claim_Writing (Entity_List);
                  Update_Text (Entity_List, "buffer_line",
                     "Buffer: " & SU.To_String (Buf_Str) & "...");
                  World.Release_Writing;
                  Needs_Render := True;
               end if;
            end if;
         end if;
      end loop;

      --  Process Tab: cycle focus and update command display
      if Tab_Pressed and Running then
         SelectionSystem (World, Tab_Pressed => True);

         World.Claim_Writing (Entity_List);
         Focus_Idx := Focused_Panel_Index (Entity_List);
         Update_Commands_Display (Entity_List, Focus_Idx);
         Update_Text (Entity_List, "buffer_line", "Buffer:");
         Update_Text (Entity_List, "status_line", "Switched to " &
            (if Focus_Idx in Panels'Range
             then SU.To_String (Panels (Focus_Idx).Label)
             else "unknown"));
         World.Release_Writing;
         Needs_Render := True;
      end if;

      --  Check flash revert
      if Flash_Panel > 0 then
         World.Claim_Writing (Entity_List);
         if Check_Flash_Revert (Entity_List) then
            Needs_Render := True;
         end if;
         World.Release_Writing;
      end if;

      --  Render once per loop iteration if anything changed
      if Needs_Render and Running then
         Render;
      end if;

      delay 0.01;
   end loop;

   --  Cleanup
   Input_Reader.Stop;
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;

end Command_Palette;
