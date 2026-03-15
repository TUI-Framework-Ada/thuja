with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Text_Editor is

   ----------------------------------------------------------------------------
   --  Text_Editor.adb
   --
   --  PURPOSE
   --  -------
   --  Implements all core buffer state and editing operations for the Thuja
   --  text editor widget. All logic here is moved directly from the original
   --  text_editor_demo.adb and decoupled from ECS and rendering concerns.
   ----------------------------------------------------------------------------

   --------------------------------------------------------
   -- Initialise
   --------------------------------------------------------
   procedure Initialise is
   begin
      Lines.Clear;
      Lines.Append (Null_Unbounded_String);
      Current_Line := 0;
      Current_Col  := 0;
      Sticky_Col   := 0;
      Mode         := Navigation;
   end Initialise;

   --------------------------------------------------------
   -- Clamp_Col
   --------------------------------------------------------
   procedure Clamp_Col is
      Line_Len : constant Natural := Length (Lines (Current_Line));
   begin
      if Line_Len = 0 then
         Current_Col := 0;
      elsif Current_Col > Line_Len then
         Current_Col := Line_Len;
      end if;
   end Clamp_Col;

   --------------------------------------------------------
   -- Reflow_From
   --------------------------------------------------------
   procedure Reflow_From (Start_Line : Natural) is
      L : Natural := Start_Line;
   begin
      loop
         exit when L >= Natural (Lines.Length);

         declare
            Current_Str : constant String := To_String (Lines (L));
         begin
            exit when Current_Str'Length <= Text_Width;

            declare
               Keep     : constant String :=
                  Current_Str (Current_Str'First ..
                               Current_Str'First + Text_Width - 1);
               Overflow : constant String :=
                  Current_Str (Current_Str'First + Text_Width ..
                               Current_Str'Last);
            begin
               Lines.Replace_Element (L, To_Unbounded_String (Keep));

               if L = Natural (Lines.Length) - 1 then
                  Lines.Append (To_Unbounded_String (Overflow));
                  exit;
               else
                  Lines.Replace_Element
                     (L + 1,
                      To_Unbounded_String (
                         Overflow & To_String (Lines (L + 1))
                      ));
               end if;
            end;
         end;

         L := L + 1;
      end loop;
   end Reflow_From;

   --------------------------------------------------------
   -- Reflow_Up_From
   --------------------------------------------------------
   procedure Reflow_Up_From (Start_Line : Natural) is
      L : Natural := Start_Line;
   begin
      loop
         exit when L >= Natural (Lines.Length) - 1;

         declare
            Current_Len : constant Natural := Length (Lines (L));
            Space       : constant Natural := Text_Width - Current_Len;
            Next_Str    : constant String  := To_String (Lines (L + 1));
         begin
            exit when Space = 0 or else Next_Str'Length = 0;

            declare
               Pull_Count : constant Natural :=
                  Natural'Min (Space, Next_Str'Length);
               Pull       : constant String :=
                  Next_Str (Next_Str'First ..
                            Next_Str'First + Pull_Count - 1);
               Remaining  : constant String :=
                  Next_Str (Next_Str'First + Pull_Count ..
                            Next_Str'Last);
            begin
               Lines.Replace_Element
                  (L,
                   To_Unbounded_String (
                      To_String (Lines (L)) & Pull
                   ));

               if Remaining'Length = 0 then
                  Lines.Delete (L + 1);
                  exit;
               else
                  Lines.Replace_Element
                     (L + 1, To_Unbounded_String (Remaining));
               end if;
            end;
         end;

         L := L + 1;
      end loop;
   end Reflow_Up_From;

   --------------------------------------------------------
   -- Insert_Char
   --------------------------------------------------------
   procedure Insert_Char (C : Character) is
      S : constant String := To_String (Lines (Current_Line));
   begin
      Lines.Replace_Element
         (Current_Line,
          To_Unbounded_String (
             S (S'First .. S'First + Current_Col - 1)
             & C
             & S (S'First + Current_Col .. S'Last)
          ));
      Current_Col := Current_Col + 1;
      Sticky_Col  := Current_Col;

      Reflow_From (Current_Line);

      if Current_Col >= Text_Width then
         declare
            Next_Line : constant Natural := Current_Line + 1;
         begin
            if Next_Line < Natural (Lines.Length) then
               Current_Line := Next_Line;
               Current_Col  := Current_Col - Text_Width;
               Sticky_Col   := Current_Col;
            else
               Current_Col := Length (Lines (Current_Line));
               Sticky_Col  := Current_Col;
            end if;
         end;
      end if;
   end Insert_Char;

   --------------------------------------------------------
   -- Handle_Backspace
   --------------------------------------------------------
   procedure Handle_Backspace is
   begin
      if Current_Col > 0 then
         declare
            S : constant String := To_String (Lines (Current_Line));
         begin
            Lines.Replace_Element
               (Current_Line,
                To_Unbounded_String (
                   S (S'First .. S'First + Current_Col - 2)
                   & S (S'First + Current_Col .. S'Last)
                ));
            Current_Col := Current_Col - 1;
            Sticky_Col  := Current_Col;
         end;

      elsif Current_Line > 0 then
         declare
            Above_Len : constant Natural :=
               Length (Lines (Current_Line - 1));
            Merged    : constant Unbounded_String :=
               Lines (Current_Line - 1) & Lines (Current_Line);
         begin
            Lines.Replace_Element (Current_Line - 1, Merged);
            Lines.Delete (Current_Line);
            Current_Line := Current_Line - 1;
            Current_Col  := Above_Len;
            Sticky_Col   := Current_Col;
         end;

         Reflow_From (Current_Line);
      end if;
   end Handle_Backspace;

   --------------------------------------------------------
   -- Handle_Enter
   --------------------------------------------------------
   procedure Handle_Enter is
      Current_Text : constant String := To_String (Lines (Current_Line));
      Before       : constant String :=
         Current_Text (Current_Text'First ..
                       Current_Text'First + Current_Col - 1);
      After        : constant String :=
         Current_Text (Current_Text'First + Current_Col ..
                       Current_Text'Last);
   begin
      Lines.Replace_Element (Current_Line, To_Unbounded_String (Before));
      Lines.Insert (Current_Line + 1, To_Unbounded_String (After));
      Current_Line := Current_Line + 1;
      Current_Col  := 0;
      Sticky_Col   := 0;
   end Handle_Enter;

   --------------------------------------------------------
   -- Handle_Tab
   --------------------------------------------------------
   procedure Handle_Tab is
      Tab_Size : constant Positive := 4;
   begin
      for T in 1 .. Tab_Size loop
         declare
            S : constant String := To_String (Lines (Current_Line));
         begin
            Lines.Replace_Element
               (Current_Line,
                To_Unbounded_String (
                   S (S'First .. S'First + Current_Col - 1)
                   & ' '
                   & S (S'First + Current_Col .. S'Last)
                ));
            Current_Col := Current_Col + 1;
            Sticky_Col  := Current_Col;

            Reflow_From (Current_Line);

            if Current_Col >= Text_Width then
               declare
                  Next_Line : constant Natural := Current_Line + 1;
               begin
                  if Next_Line < Natural (Lines.Length) then
                     Current_Line := Next_Line;
                     Current_Col  := Current_Col - Text_Width;
                     Sticky_Col   := Current_Col;
                  else
                     Current_Col := Length (Lines (Current_Line));
                     Sticky_Col  := Current_Col;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Handle_Tab;

   --------------------------------------------------------
   -- Handle_Navigation
   --------------------------------------------------------
   procedure Handle_Navigation (C : Character) is
   begin
      case C is
         when 'i' =>
            Mode := Insert;

         when 'w' =>
            if Current_Line > 0 then
               Current_Line := Current_Line - 1;
               Current_Col  := Natural'Min
                  (Sticky_Col, Length (Lines (Current_Line)));
            end if;

         when 's' =>
            if Current_Line < Natural (Lines.Length) - 1 then
               Current_Line := Current_Line + 1;
               Current_Col  := Natural'Min
                  (Sticky_Col, Length (Lines (Current_Line)));
            end if;

         when 'a' =>
            if Current_Col > 0 then
               Current_Col := Current_Col - 1;
               Sticky_Col  := Current_Col;
            elsif Current_Line > 0 then
               Current_Line := Current_Line - 1;
               Current_Col  := Length (Lines (Current_Line));
               Sticky_Col   := Current_Col;
            end if;

         when 'd' =>
            declare
               Line_Len : constant Natural :=
                  Length (Lines (Current_Line));
            begin
               if Current_Col < Line_Len then
                  Current_Col := Current_Col + 1;
                  Sticky_Col  := Current_Col;
               elsif Current_Line < Natural (Lines.Length) - 1 then
                  Current_Line := Current_Line + 1;
                  Current_Col  := 0;
                  Sticky_Col   := 0;
               end if;
            end;

         when others => null;
      end case;
   end Handle_Navigation;

   --------------------------------------------------------
   -- Build_Editor_Text
   --------------------------------------------------------
   function Build_Editor_Text
      (Scroll_Offset : Natural;
       Visible_Rows  : Natural)
      return Unbounded_String
   is
      Result : Unbounded_String := Null_Unbounded_String;

      function Num_Gutter (N : Positive) return String is
         Img : constant String := Positive'Image (N);
         Raw : constant String := Img (Img'First + 1 .. Img'Last);
         Pad : String (1 .. Gutter_Width) := [others => ' '];
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
         Pad : String (1 .. Gutter_Width) := [others => ' '];
      begin
         Pad (1) := '~';
         return Pad;
      end Tilde_Gutter;

      --  Last line index to render (clamped to buffer length)
      Last_Line : constant Natural :=
         Natural'Min
            (Scroll_Offset + Visible_Rows - 1,
             Natural (Lines.Length) - 1);

   begin
      --  Render only the lines within the visible window
      for L in Scroll_Offset .. Last_Line loop
         declare
            Raw_Line  : constant String  := To_String (Lines (L));
            Cursor_At : constant Integer :=
               (if L = Current_Line
                then Integer (Natural'Min (Current_Col, Raw_Line'Length))
                else -1);
            Row       : Unbounded_String :=
               To_Unbounded_String (Num_Gutter (L + 1));
         begin
            for I in Raw_Line'Range loop
               if Cursor_At >= 0
                  and then I = Raw_Line'First + Cursor_At
               then
                  Append (Row, '|');
               end if;
               Append (Row, Raw_Line (I));
            end loop;

            if Cursor_At >= 0
               and then Cursor_At = Raw_Line'Length
            then
               Append (Row, '|');
            end if;

            Append (Result, Row);
            Append (Result, Character'Val (10));
         end;
      end loop;

      --  Fill remaining visible rows below the buffer with ~
      for Row in (Last_Line - Scroll_Offset + 1) .. Visible_Rows - 1 loop
         Append (Result, Tilde_Gutter);
         Append (Result, Character'Val (10));
      end loop;

      return Result;
   end Build_Editor_Text;

   --------------------------------------------------------
   -- Status_Text
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

end Text_Editor;