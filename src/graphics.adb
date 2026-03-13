--  Package Body for Graphics
with Interfaces.C; use Interfaces.C;
with System;
with System.Storage_Elements;
with Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions;

package body Graphics is

   use Ada.Characters.Conversions;

   function Trim (S : String) return String is (S (S'First + 1 .. S'Last));

   function "+" (P : Pixel_t) return Wide_Wide_String is
      RESET : constant String := CSI & "0m";
      FG : constant String := CSI & "38;2;" &
        Trim (P.Char_Color.Red'Image) & ";" &
        Trim (P.Char_Color.Green'Image) & ";" &
        Trim (P.Char_Color.Blue'Image) & "m";
      BG : constant String := CSI & "48;2;" &
        Trim (P.Background_Color.Red'Image) & ";" &
        Trim (P.Background_Color.Green'Image) & ";" &
        Trim (P.Background_Color.Blue'Image) & "m";
      BOLD : constant String := CSI & "1m";
      ITALIC : constant String := CSI & "3m";
      UNDERLINE : constant String := CSI & "4m";
      STRIKETHROUGH : constant String := CSI & "9m";

      FORMAT : constant String :=
        FG &
        BG &
        (if P.Is_Bold then BOLD else "") &
        (if P.Is_Italic then ITALIC else "") &
        (if P.Is_Underline then UNDERLINE else "") &
        (if P.Is_Strikethrough then STRIKETHROUGH else "");
   begin
      return Ada.Characters.Conversions.To_Wide_Wide_String (
         FORMAT & P.Char & RESET
      );
   end "+";

   --  Protected object for Buffer_Ptr for thread-safe access
   protected body Protected_DB is
      entry Wait (V : out Boolean)
         when not Changing is
      begin
         Changing := True;
         V := Draw_From_1;
      end Wait;

      entry Post
         when Changing is
      begin
         Changing := False;
      end Post;

      procedure Swap is
      begin
         Draw_From_1 := not Draw_From_1;
      end Swap;

      entry Read (V : out Boolean)
        when not Changing is
      begin
         V := Draw_From_1;
      end Read;
   end Protected_DB;

   --  Buffer_T Constructor - Allocates memory in the 2D pixel array, initializing record fields
   function Create_Buffer (Width  : TUI_Width;
                           Height : TUI_Height)
                           return Buffer_T
   is
      --  Declares variable to be returned using Buffer_T
      New_Buffer : Buffer_T;
   begin
      --  Store dimensions in the record
      New_Buffer.Width := Width;
      New_Buffer.Height := Height;
      New_Buffer.Data := new Pixel_Array;

      return New_Buffer;
   end Create_Buffer;

   --  Writes a new pixel value into the buffer at the (X,Y) coordinates
   procedure Set_Buffer_Pixel (B : in Out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel_t)
   is
   begin
      --  Writes new pixel into buffer "P" being the value Pixel
      B.Data.all (X, Y) := P;
   end Set_Buffer_Pixel;

   --  Reads and returns the pixel value from the buffer at the (X, Y) coordinates
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel_t
   is
   begin
      --  Returns value read from the array
      return B.Data.all (X, Y);
   end Get_Buffer_Pixel;

   --=============================================================================
   -- Implementation for Linux using regular ANSI escape codes (Confirm rationale)
   --=============================================================================

   --  Hides the terminal cursor
   procedure Hide_Cursor is
   begin
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (CSI & "?25l"));
      Ada.Wide_Wide_Text_IO.Flush; -- Force it to hide NOW
   end Hide_Cursor;

   --  Shows the terminal cursor
   procedure Show_Cursor is
   begin
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (CSI & "?25h"));
      Ada.Wide_Wide_Text_IO.Flush; -- Force it to show NOW
   end Show_Cursor;

   procedure Save_Cursor_Position is
   begin
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (CSI & "s"));
   end Save_Cursor_Position;

   procedure Restore_Cursor_Position is
   begin
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (CSI & "u"));
   end Restore_Cursor_Position;

   --  Sends ANSI code to the terminal to wipe the screen.
   --  This should be run once before any of the systems.
   -- NOTE: Removed both hide cursors temporarily as hide/show
   -- do not need to be re-called
   procedure Clear_Screen is
   begin
      -- Enable VT processing first so ANSI sequences are honoured
      -- VT_Processing only needs to be run once at start of any demo
      -- Enable_VT_Processing;
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (
         CSI & "?1049h" &   --  Switch to alternate screen buffer
         -- CSI & "?25l" &     --  Hide cursor (ANSI)
         CSI & "0m" &       --  Reset formatting
         CSI & "2J" &       --  Clear screen
         CSI & "1;1H"));     --  Move to top-left
      --  Also hide cursor via Win32 API as a fallback
      -- Set_Cursor_Visible (False);
      Ada.Wide_Wide_Text_IO.Flush;
   end Clear_Screen;

   --  Resets terminal to normal state (resets colors and typefaces)
   procedure Reset_Styling is
   begin
      --  Reset all styling / attributes
      Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (CSI & "0m"));
   end Reset_Styling;

end Graphics;
