with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
use Ada.Strings.Unbounded;

package Text_Editor is

   ----------------------------------------------------------------------------
   --  Text_Editor
   --
   --  PURPOSE
   --  -------
   --  Provides all core buffer state and editing operations for the Thuja
   --  text editor widget. This package is intentionally decoupled from ECS
   --  and rendering concerns so it can be used by any demo or widget that
   --  needs text editing functionality.
   --
   --  RESPONSIBILITIES
   --  ----------------
   --  - Owns the line buffer (Vector of Unbounded_String)
   --  - Owns cursor state (Current_Line, Current_Col, Sticky_Col)
   --  - Owns editor mode (Navigation / Insert)
   --  - Provides all editing operations (insert, delete, enter, tab)
   --  - Provides reflow procedures for maintaining line width invariants
   --  - Provides Build_Editor_Text for rendering the buffer to a string
   ----------------------------------------------------------------------------

   --------------------------------------------------------
   -- LINE BUFFER
   --------------------------------------------------------
   package Line_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Ada.Strings.Unbounded.Unbounded_String);

   --  The line buffer. One Unbounded_String per screen row.
   --  Always contains at least one line so the cursor always
   --  has a home.
   Lines : Line_Vectors.Vector;

   --------------------------------------------------------
   -- EDITOR CONSTANTS
   -- Gutter_Width : width of the line number column
   -- Editor_Width : total widget width in columns
   -- Text_Width   : columns available for text content
   --------------------------------------------------------
   Gutter_Width : constant Positive := 4;
   Editor_Width : constant Positive := 80;
   Text_Width   : constant Positive := Editor_Width - Gutter_Width - 2;

   --------------------------------------------------------
   -- CURSOR STATE
   -- Current_Line : 0-based index of the line the cursor is on
   -- Current_Col  : 0-based column the cursor is at
   -- Sticky_Col   : intended column preserved across short lines
   --------------------------------------------------------
   Current_Line : Natural := 0;
   Current_Col  : Natural := 0;
   Sticky_Col   : Natural := 0;

   --------------------------------------------------------
   -- MODE
   --------------------------------------------------------
   type Editor_Mode_T is (Navigation, Insert);
   Mode : Editor_Mode_T := Navigation;

   --------------------------------------------------------
   -- SUBPROGRAMS
   --------------------------------------------------------

   --  Initialise the buffer with a single empty line.
   --  Must be called once before any other operation.
   procedure Initialise;

   --  Clamp Current_Col to the actual length of Current_Line
   --  while leaving Sticky_Col unchanged. Call after any
   --  vertical cursor movement.
   procedure Clamp_Col;

   --  Cascade overflow downward from Start_Line.
   --  If the line at Start_Line exceeds Text_Width it is split
   --  and the overflow is prepended to the next line. Continues
   --  until no line exceeds Text_Width.
   procedure Reflow_From (Start_Line : Natural);

   --  Cascade pull-up from Start_Line.
   --  If the line at Start_Line is shorter than Text_Width and
   --  a next line exists, characters are pulled from the start
   --  of the next line to fill the gap. Continues until every
   --  line is either full or there is nothing left to pull.
   procedure Reflow_Up_From (Start_Line : Natural);

   --  Handle a printable character insertion at the current
   --  cursor position. Advances the cursor and triggers reflow.
   procedure Insert_Char (C : Character);

   --  Handle a backspace at the current cursor position.
   --  Deletes the character before the cursor, or merges with
   --  the line above if at column 0.
   procedure Handle_Backspace;

   --  Handle Enter. Splits the current line at the cursor
   --  position and moves the cursor to the start of the new line.
   procedure Handle_Enter;

   --  Handle Tab. Inserts Tab_Size spaces at the cursor position,
   --  triggering reflow after each space insertion.
   procedure Handle_Tab;

   --  Handle a navigation keypress. Moves the cursor according
   --  to the w/a/s/d convention and updates Sticky_Col.
   procedure Handle_Navigation (C : Character);

   --  Build the full display string for the editor widget.
   --  Each line in the buffer is prefixed with its line number
   --  in the gutter. Lines beyond the end of the buffer show ~.
   --  The cursor is rendered as | at the current position.
   --  Scroll_Offset controls which line appears at the top of
   --  the visible area.
   function Build_Editor_Text
      (Scroll_Offset : Natural;
       Visible_Rows  : Natural)
      return Ada.Strings.Unbounded.Unbounded_String;

   --  Return the appropriate status bar text for the current mode.
   function Status_Text
      return Ada.Strings.Unbounded.Unbounded_String;

end Text_Editor;