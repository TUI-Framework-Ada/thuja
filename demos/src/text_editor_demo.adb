--  ================================================================================
--  Update documentation here with current instructions & list of features still
--  in-progress.
--  ================================================================================
with Ada.Wide_Wide_Text_IO;
with Ada.Containers.Vectors;
with Components;
with Console;
with ECS;
with Graphics;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
use Graphics;
with IDs;
use type IDs.Entity_ID_Vector.Vector;
with Flexbox;
with Input_Handling;

procedure Text_Editor_Demo is

   Loop_Count : constant Positive := 6000; -- ~3 minutes at 30 FPS in case can't exit

   --  ECS Entity storage
   Entities_PO  : ECS.Entity_Components_PO;
   Entities_Ptr : ECS.Entity_Components_Ptr;

   --------------------------------------------------------
   -- ENTITY DEFINITIONS
   --------------------------------------------------------
   E_RenderInfo  : constant IDs.Entity_Id := IDs.To_EID ("RenderInfo");
   E_Root        : constant IDs.Entity_Id := IDs.To_EID ("Root");
   E_TitleBar    : constant IDs.Entity_Id := IDs.To_EID ("TitleBar");
   E_Editor      : constant IDs.Entity_Id := IDs.To_EID ("Editor");
   E_StatusBar   : constant IDs.Entity_Id := IDs.To_EID ("StatusBar");

   C_RenderInfo  : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_RenderInfo);
   C_Root        : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Root);
   C_TitleBar    : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_TitleBar);
   C_Editor      : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_Editor);
   C_StatusBar   : constant ECS.Components_Ptr := ECS.Add_Entity (Entities_PO, E_StatusBar);

   --------------------------------------------------------
   -- LINE BUFFER
   -- One Unbounded_String per line. The editor always has
   -- at least one line so the cursor always has a home.
   --------------------------------------------------------
   package Line_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Unbounded_String);

   Lines : Line_Vectors.Vector;

   --------------------------------------------------------
   -- CURSOR STATE
   -- Current_Line  : which line the cursor is on (0-based)
   -- Current_Col   : actual column on that line (0-based)
   -- Sticky_Col    : the column we are trying to be on;
   --                 preserved when moving across short lines
   --------------------------------------------------------
   Current_Line : Natural := 0;
   Current_Col  : Natural := 0;
   Sticky_Col   : Natural := 0;

   --------------------------------------------------------
   -- EDITOR CONSTANTS
   -- Gutter_Width: "999 " = 4 chars, covers up to 999 lines
   -- Text_Width  : remaining columns available for text
   --------------------------------------------------------
   Gutter_Width : constant Positive := 4;
   Editor_Width : constant Positive := 80;
   Text_Width   : constant Positive := Editor_Width - Gutter_Width - 2;

   --------------------------------------------------------
   -- MODE
   --------------------------------------------------------
   type Editor_Mode_T is (Navigation, Insert);
   Mode : Editor_Mode_T := Navigation;

   Should_Quit : Boolean := False;

   --------------------------------------------------------
   -- HELPER: Clamp_Col
   -- After any vertical movement, clamp Current_Col to the
   -- actual line length while preserving Sticky_Col.
   --------------------------------------------------------
   procedure Clamp_Col is
      Line_Len : constant Natural :=
         Length (Lines (Current_Line));
   begin
      if Line_Len = 0 then
         Current_Col := 0;
      elsif Current_Col > Line_Len then
         Current_Col := Line_Len;
      end if;
   end Clamp_Col;

   --------------------------------------------------------
   -- HELPER: Build_Editor_Text
   -- Builds the full display string that goes into the
   -- TextComponent. Each screen line is:
   --   <gutter> <text content up to Text_Width chars>
   -- Active lines get a number, empty lines get "~".
   -- The cursor is rendered as a "|" character.
   -- Text that is longer than Text_Width wraps onto the
   -- next screen line, which creates a new gutter number.
   --------------------------------------------------------
   function Build_Editor_Text return Unbounded_String is

      Result      : Unbounded_String := Null_Unbounded_String;
      Editor_Rows : constant Natural := 22;

      function Num_Gutter (N : Positive) return String is
         Img : constant String := Positive'Image (N);
         Raw : constant String := Img (Img'First + 1 .. Img'Last);
         Pad : String (1 .. Gutter_Width) := (others => ' ');
      begin
         if Raw'Length >= Gutter_Width then
            Pad := Raw (Raw'First .. Raw'First + Gutter_Width - 2) & " ";
         else
            Pad (Gutter_Width - Raw'Length .. Gutter_Width - 1) := Raw;
            Pad (Gutter_Width) := ' ';
         end if;
         return Pad;
      end Num_Gutter;

      function Tilde_Gutter return String is
         Pad : String (1 .. Gutter_Width) := (others => ' ');
      begin
         Pad (1) := '~';
         return Pad;
      end Tilde_Gutter;

   begin
      --  One screen row per vector entry, always numbered
      for L in 0 .. Natural (Lines.Length) - 1 loop
         declare
            Raw_Line  : constant String := To_String (Lines (L));
            Cursor_At : constant Integer :=
              (if L = Current_Line
               then Integer (Natural'Min (Current_Col, Raw_Line'Length))
               else -1);
            Row       : Unbounded_String :=
              To_Unbounded_String (Num_Gutter (L + 1));
         begin
            --  Append text with cursor injected at the right position
            for I in Raw_Line'Range loop
               if Cursor_At >= 0 and then I = Raw_Line'First + Cursor_At then
                  Append (Row, '|');
               end if;
               Append (Row, Raw_Line (I));
            end loop;

            --  Cursor sitting after the last character
            if Cursor_At >= 0 and then Cursor_At = Raw_Line'Length then
               Append (Row, '|');
            end if;

            Append (Result, Row);
            Append (Result, Character'Val (10));
         end;
      end loop;

      --  Fill remaining visible rows with ~
      for Row in Natural (Lines.Length) .. Editor_Rows - 1 loop
         Append (Result, Tilde_Gutter);
         Append (Result, Character'Val (10));
      end loop;

      return Result;
   end Build_Editor_Text;

   --------------------------------------------------------
   -- HELPER: Status_Text
   --------------------------------------------------------
   function Status_Text return Unbounded_String is
   begin
      case Mode is
         when Navigation =>
            return To_Unbounded_String
               ("NAVIGATION   w/a/s/d to move  |  i to insert  |  ESC to quit");
         when Insert =>
            return To_Unbounded_String
               ("INSERT   type to edit  |  ESC to return to Navigation");
      end case;
   end Status_Text;

   --------------------------------------------------------
   -- COMPONENT DEFINITIONS
   --------------------------------------------------------

   Comp_RenderInfo : constant Components.Render_Info_Component_T := (
      Terminal_Width       => 80,
      Terminal_Height      => 24,
      Prev_Terminal_Width  => 80,
      Prev_Terminal_Height => 24,
      Framebuffer_1        => (Width => 80, Height => 24, Data => new Pixel_Array),
      Framebuffer_2        => (Width => 80, Height => 24, Data => new Pixel_Array),
      Drawing_FB           => new Graphics.Protected_DB,
      Backbuffer           => (Width => 80, Height => 24, Data => new Pixel_Array)
   );

   Comp_Root_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 24,
      Children      => IDs.Entity_ID_Vector.To_Vector (E_TitleBar, 1)
                       & E_Editor & E_StatusBar,
      Render_Buffer => (Width => 80, Height => 24, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Root_Marker : constant Components.Root_Widget_Component_T := (null record);

   Comp_Root_Flex : constant Components.Flex_Layout_Component_T := (
      Flex_Container => (
         Width      => 80,
         Height     => 24,
         Direction  => Flexbox.Column,
         Justify    => Flexbox.Flex_Start,
         Align      => Flexbox.Stretch,
         Item_Count => 3,
         Items      => new Flexbox.Flex_Item_Array'(
            1 => (
               Related_Entity => E_TitleBar,
               Flex_Basis     => 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            2 => (
               Related_Entity => E_Editor,
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 22,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            ),
            3 => (
               Related_Entity => E_StatusBar,
               Flex_Basis     => 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               Computed_Size  => 1,
               Cross_Size     => 80,
               Position_X     => 0,
               Position_Y     => 0
            )
         )
      ),
      Is_Dirty => True
   );

   Comp_TitleBar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 1,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_TitleBar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   Comp_TitleBar_Text : constant Components.Text_Component_T := (
      Text             => To_Unbounded_String ("  Thuja Text Editor"),
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => True,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_TitleBar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   Comp_Editor_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 2,
      Size_Width    => 80,
      Size_Height   => 22,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 22, Data => new Pixel_Array),
      Has_Focus     => True,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_Editor_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Gray
   );

   Comp_Editor_Text : Components.Text_Component_T := (
      Text             => Build_Editor_Text,
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_Editor_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

   Comp_StatusBar_Widget : constant Components.Widget_Component_T := (
      Position_X    => 1,
      Position_Y    => 23,
      Size_Width    => 80,
      Size_Height   => 1,
      Children      => IDs.Entity_ID_Vector.Empty_Vector,
      Render_Buffer => (Width => 80, Height => 1, Data => new Pixel_Array),
      Has_Focus     => False,
      Is_Visible    => True,
      Is_Enabled    => True
   );

   Comp_StatusBar_BG : constant Components.Background_Color_Component_T := (
      Background_Color => Graphics.Blue
   );

   Comp_StatusBar_Text : Components.Text_Component_T := (
      Text             => Status_Text,
      Text_Color       => Graphics.White,
      Offset_X         => 1,
      Offset_Y         => 1,
      Is_Bold          => False,
      Is_Italic        => False,
      Is_Underline     => False,
      Is_Strikethrough => False
   );

   Comp_StatusBar_PositionMode : constant Components.Position_Mode_Component_T := (
      Mode => Components.Flex
   );

begin
   --  Seed the line buffer with one empty line so the cursor
   --  always has somewhere to live from frame one
   Lines.Append (Null_Unbounded_String);

   Console.Enable_VT_Processing;
   Console.Set_Cursor_Visible (False);
   Graphics.Save_Cursor_Position;
   Graphics.Clear_Screen;
   Ada.Wide_Wide_Text_IO.Flush;

   --------------------------------------------------------
   -- REGISTER ALL COMPONENTS
   --------------------------------------------------------
   Entities_PO.Claim_Writing (Entities_Ptr);

   ECS.Add_Component (C_RenderInfo.all, IDs.To_CID ("RenderInfo"),               Comp_RenderInfo);

   ECS.Add_Component (C_Root.all,       IDs.To_CID ("WidgetComponent"),           Comp_Root_Widget);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("RootWidget"),                Comp_Root_Marker);
   ECS.Add_Component (C_Root.all,       IDs.To_CID ("FlexLayoutComponent"),       Comp_Root_Flex);

   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("WidgetComponent"),           Comp_TitleBar_Widget);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("BackgroundColorComponent"),  Comp_TitleBar_BG);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("TextComponent"),             Comp_TitleBar_Text);
   ECS.Add_Component (C_TitleBar.all,   IDs.To_CID ("PositionMode"),              Comp_TitleBar_PositionMode);

   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("WidgetComponent"),           Comp_Editor_Widget);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("BackgroundColorComponent"),  Comp_Editor_BG);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("TextComponent"),             Comp_Editor_Text);
   ECS.Add_Component (C_Editor.all,     IDs.To_CID ("PositionMode"),              Comp_Editor_PositionMode);

   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("WidgetComponent"),           Comp_StatusBar_Widget);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("BackgroundColorComponent"),  Comp_StatusBar_BG);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("TextComponent"),             Comp_StatusBar_Text);
   ECS.Add_Component (C_StatusBar.all,  IDs.To_CID ("PositionMode"),              Comp_StatusBar_PositionMode);

   Entities_PO.Release_Writing;

   --------------------------------------------------------
   -- START INPUT READER
   --------------------------------------------------------
   Input_Handling.Input_Reader.Start;

   --------------------------------------------------------
   -- MAIN LOOP
   --------------------------------------------------------
   for Loop_Index in 1 .. Loop_Count loop

      --------------------------------------------------------
      -- PROCESS INPUT
      --------------------------------------------------------
      declare
         Event     : Input_Handling.Input_Event_t;
         Got_Input : Boolean := True;
      begin
         while Got_Input loop
            Input_Handling.Input_Buffer.Consume (Event);

            if Event.Char_Value = Character'Val (0) then
               Got_Input := False;

            elsif Mode = Insert then
               case Event.Cmd is

                  when Input_Handling.Quit =>
                     --  ESC: back to Navigation, clamp cursor to line end
                     Mode := Navigation;
                     Clamp_Col;

                  when Input_Handling.Enter =>
                     --  Split current line at cursor position
                     declare
                        Current_Text : constant String :=
                           To_String (Lines (Current_Line));
                        Before : constant String :=
                           Current_Text
                              (Current_Text'First ..
                               Current_Text'First + Current_Col - 1);
                        After  : constant String :=
                           Current_Text
                              (Current_Text'First + Current_Col ..
                               Current_Text'Last);
                     begin
                        Lines.Replace_Element
                           (Current_Line, To_Unbounded_String (Before));
                        Lines.Insert
                           (Current_Line + 1, To_Unbounded_String (After));
                        Current_Line := Current_Line + 1;
                        Current_Col  := 0;
                        Sticky_Col   := 0;
                     end;

                  when others =>
                     --  ASCII VAL to backspace (delete) text.
                     --  ASCII (127) & (8) are both used here because 
                     --  It seems that Windows only sees (8) so (127)
                     --  could be removed but that will be for testing
                     --  later with Linux.
                     if Event.Char_Value = Character'Val (127)
                       or else Event.Char_Value = Character'Val (8)
                     then
                        if Current_Col > 0 then
                           --  Delete character before cursor on same line
                           declare
                              S : constant String :=
                                To_String (Lines (Current_Line));
                           begin
                              Lines.Replace_Element
                                (Current_Line,
                                 To_Unbounded_String
                                   (S (S'First .. S'First + Current_Col - 2)
                                    & S (S'First + Current_Col .. S'Last)));
                              Current_Col := Current_Col - 1;
                              Sticky_Col := Current_Col;
                           end;

                           --  Reflow: current line is now shorter than Text_Width,
                           --  pull characters back from the start of the next line
                           if Current_Line < Natural (Lines.Length) - 1 then
                              declare
                                 Current_Len : constant Natural :=
                                   Length (Lines (Current_Line));
                                 Space       : constant Natural :=
                                   Text_Width - Current_Len;
                                 Next_Str    : constant String :=
                                   To_String (Lines (Current_Line + 1));
                              begin
                                 if Space > 0 and then Next_Str'Length > 0 then
                                    declare
                                       Pull_Count : constant Natural :=
                                         Natural'Min (Space, Next_Str'Length);
                                       Pull       : constant String :=
                                         Next_Str
                                           (Next_Str'First
                                            ..
                                              Next_Str'First + Pull_Count - 1);
                                       Remaining  : constant String :=
                                         Next_Str
                                           (Next_Str'First
                                            + Pull_Count
                                            .. Next_Str'Last);
                                    begin
                                       Lines.Replace_Element
                                         (Current_Line,
                                          To_Unbounded_String
                                            (To_String (Lines (Current_Line))
                                             & Pull));
                                       if Remaining'Length = 0 then
                                          Lines.Delete (Current_Line + 1);
                                       else
                                          Lines.Replace_Element
                                            (Current_Line + 1,
                                             To_Unbounded_String (Remaining));
                                       end if;
                                    end;
                                 end if;
                              end;
                           end if;

                        elsif Current_Line > 0 then
                           --  At column 0: merge with line above
                           declare
                              Above_Len : constant Natural :=
                                Length (Lines (Current_Line - 1));
                              Merged    : constant Unbounded_String :=
                                Lines (Current_Line - 1)
                                & Lines (Current_Line);
                           begin
                              Lines.Replace_Element (Current_Line - 1, Merged);
                              Lines.Delete (Current_Line);
                              Current_Line := Current_Line - 1;
                              Current_Col := Above_Len;
                              Sticky_Col := Current_Col;
                           end;

                           --  Reflow after merge: split overflow back out if
                           --  the merged line exceeds Text_Width
                           declare
                              Merged_Str : constant String :=
                                To_String (Lines (Current_Line));
                           begin
                              if Merged_Str'Length > Text_Width then
                                 declare
                                    Keep     : constant String :=
                                      Merged_Str
                                        (Merged_Str'First
                                         .. Merged_Str'First + Text_Width - 1);
                                    Overflow : constant String :=
                                      Merged_Str
                                        (Merged_Str'First
                                         + Text_Width
                                         .. Merged_Str'Last);
                                 begin
                                    Lines.Replace_Element
                                      (Current_Line,
                                       To_Unbounded_String (Keep));
                                    if Current_Line
                                      = Natural (Lines.Length) - 1
                                    then
                                       Lines.Append
                                         (To_Unbounded_String (Overflow));
                                    else
                                       Lines.Replace_Element
                                         (Current_Line + 1,
                                          To_Unbounded_String
                                            (Overflow
                                             & To_String
                                                 (Lines (Current_Line + 1))));
                                    end if;
                                 end;
                              end if;
                           end;
                        end if;

                     elsif Event.Char_Value >= ' ' then
                        --  Insert printable character at cursor position
                        declare
                           S : constant String :=
                             To_String (Lines (Current_Line));
                        begin
                           Lines.Replace_Element
                             (Current_Line,
                              To_Unbounded_String
                                (S (S'First .. S'First + Current_Col - 1)
                                 & Event.Char_Value
                                 & S (S'First + Current_Col .. S'Last)));
                           Current_Col := Current_Col + 1;
                           Sticky_Col := Current_Col;

                           --  Check if the line has grown beyond Text_Width.
                           --  This handles both end-of-line typing and mid-line insertion.
                           declare
                              Updated : constant String :=
                                To_String (Lines (Current_Line));
                           begin
                              if Updated'Length > Text_Width then
                                 declare
                                    Keep     : constant String :=
                                      Updated
                                        (Updated'First
                                         .. Updated'First + Text_Width - 1);
                                    Overflow : constant String :=
                                      Updated
                                        (Updated'First
                                         + Text_Width
                                         .. Updated'Last);
                                 begin
                                    Lines.Replace_Element
                                      (Current_Line,
                                       To_Unbounded_String (Keep));

                                    --  Push overflow onto next line, creating one if needed
                                    if Current_Line
                                      = Natural (Lines.Length) - 1
                                    then
                                       Lines.Append
                                         (To_Unbounded_String (Overflow));
                                    else
                                       --  Prepend overflow to the start of the next line
                                       Lines.Replace_Element
                                         (Current_Line + 1,
                                          To_Unbounded_String
                                            (Overflow
                                             & To_String
                                                 (Lines (Current_Line + 1))));
                                    end if;

                                    --  If cursor crossed into overflow, move it to next line
                                    if Current_Col >= Text_Width then
                                       Current_Line := Current_Line + 1;
                                       Current_Col := Current_Col - Text_Width;
                                       Sticky_Col := Current_Col;
                                    end if;
                                 end;
                              end if;
                           end;
                        end;
                     end if;
               end case;

            else
               --  NAVIGATION MODE
               case Event.Cmd is

                  when Input_Handling.Quit =>
                     Should_Quit := True;

                  when others =>
                     case Event.Char_Value is

                        when 'i' =>
                           Mode := Insert;

                        when 'w' =>
                           --  Move up
                           if Current_Line > 0 then
                              Current_Line := Current_Line - 1;
                              Current_Col  := Natural'Min
                                 (Sticky_Col,
                                  Length (Lines (Current_Line)));
                           end if;

                        when 's' =>
                           --  Move down
                           if Current_Line < Natural (Lines.Length) - 1 then
                              Current_Line := Current_Line + 1;
                              Current_Col  := Natural'Min
                                 (Sticky_Col,
                                  Length (Lines (Current_Line)));
                           end if;

                        when 'a' =>
                           --  Move left, wrap to end of previous line
                           if Current_Col > 0 then
                              Current_Col := Current_Col - 1;
                              Sticky_Col  := Current_Col;
                           elsif Current_Line > 0 then
                              Current_Line := Current_Line - 1;
                              Current_Col  := Length (Lines (Current_Line));
                              Sticky_Col   := Current_Col;
                           end if;

                        when 'd' =>
                           --  Move right, wrap to start of next line
                           declare
                              Line_Len : constant Natural :=
                                 Length (Lines (Current_Line));
                           begin
                              if Current_Col < Line_Len then
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
         end loop;
      end;

      exit when Should_Quit;

      --------------------------------------------------------
      -- REBUILD TEXT COMPONENTS
      --------------------------------------------------------
      Comp_Editor_Text.Text    := Build_Editor_Text;
      Comp_StatusBar_Text.Text := Status_Text;

      Entities_PO.Claim_Writing (Entities_Ptr);
      ECS.Add_Component (C_Editor.all,    IDs.To_CID ("TextComponent"), Comp_Editor_Text);
      ECS.Add_Component (C_StatusBar.all, IDs.To_CID ("TextComponent"), Comp_StatusBar_Text);
      Entities_PO.Release_Writing;

      --------------------------------------------------------
      -- SYSTEMS
      --------------------------------------------------------
      ECS.TerminalResizeSystem   (Entities_PO);
      ECS.FlexLayoutSystem       (Entities_PO);
      ECS.WidgetBackgroundSystem (Entities_PO);
      ECS.TextRenderSystem       (Entities_PO);
      ECS.BufferCopySystem       (Entities_PO);
      ECS.BufferDrawSystem       (Entities_PO);
      ECS.DoubleBufferFlagSystem (Entities_PO);

      delay Duration (0.033);
   end loop;

   --------------------------------------------------------
   -- SHUTDOWN
   --------------------------------------------------------
   Input_Handling.Input_Reader.Stop;

   Console.Set_Cursor_Visible (True);
   Graphics.Restore_Cursor_Position;
   Ada.Wide_Wide_Text_IO.Flush;

end Text_Editor_Demo;