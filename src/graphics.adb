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

   function "=" (A, B : Pixel_t) return Boolean is
   begin
      return
        A.Char = B.Char and then (
          (if A.Char = ' '
           then A.Char_Color = B.Char_Color and
             A.Is_Bold = B.Is_Bold and
             A.Is_Italic = B.Is_Italic
           else True) and
          A.Background_Color = B.Background_Color and
          A.Is_Underline = B.Is_Underline and
          A.Is_Strikethrough = B.Is_Strikethrough
        );
   end "=";

   --  Protected object for Buffer_Ptr for thread-safe access
   protected body Protected_DB is
      entry Swap
        when not Drawing is
      begin
         Framebuffer_Index := Framebuffer_Index + 1;
      end Swap;

      procedure Start_Draw is
      begin
         Drawing := True;
      end Start_Draw;

      procedure End_Draw is
      begin
         Drawing := False;
      end End_Draw;

      function Front return Framebuffer_Index_t is
      begin
         return Framebuffer_Index;
      end Front;

      function Back return Framebuffer_Index_t is
      begin
         return Framebuffer_Index + 1;
      end Back;
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
      New_Buffer.Data := new Pixel_Array (1 .. Width, 1 .. Height); --  Allocates memory for the pixel array based on dimensions

      return New_Buffer;
   end Create_Buffer;

   --  Writes a new pixel value into the buffer at the (X,Y) coordinates
   procedure Set_Buffer_Pixel (B : in Out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel_t)
   is
   begin
      if X > B.Width or else Y > B.Height then
         return; -- Adds boundary
      end if;
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
      if X > B.Width or else Y > B.Height then
         return (others => <>); -- Adds boundary
      end if;
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

      procedure Write_To_Buffer
   (Buf  : in out Buffer_T;
      Col         : in     TUI_Width;
      Row         : in     TUI_Height;
      Text        : in     String;
      FG          : in     Color_t;
      BG          : in     Color_t;
      Transparent : in     Boolean;
      Bold        : in     Boolean := False)
   is
      X : TUI_Width := Col;
   begin
      for I in Text'Range loop
         exit when X > Buf.Width;
         Set_Buffer_Pixel (Buf, X, Row,
            (Char             => Text (I),
            Char_Color       => FG,
            Background_Color => BG,
            Background_Transparent => Transparent,
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
            Background_Transparent => False,
            Is_Bold          => False,
            Is_Italic        => False,
            Is_Underline     => False,
            Is_Strikethrough => False));
      end loop;
   end Fill_Row;

end Graphics;
