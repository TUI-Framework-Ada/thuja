--  Package Body for Graphics
with Ada.Text_IO;
with Interfaces.C; -- Not implemented yet, but may be needed for future FFI

package body Graphics is

   --  ANSI escape sequence prefix
   CSI : constant String := Character'Val (16#1B#) & '[';

      --=============================================================================
   --TODO: Implement these for Windows using Win32 API calls
   procedure Enable_VT_Processing is
   begin
      --  On Windows, this would enable ANSI escape sequence processing
      --  On Unix/Linux/macOS, ANSI sequences work by default
      --  This is a no-op stub for cross-platform compatibility
      null;
   end Enable_VT_Processing;

  
   procedure Set_Cursor_Visible (Visible : Boolean) is
   begin
      --  On Windows, this would use Win32 API to show/hide cursor
      --  On Unix/Linux/macOS, we use ANSI sequences instead
      --  This is a no-op stub since ANSI sequences handle it
      null;
   end Set_Cursor_Visible;
--=============================================================================


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

   --  Hides the terminal cursor
   procedure Hide_Cursor is
   begin
      Ada.Text_IO.Put (CSI & "?25l");
   end Hide_Cursor;

   --  Shows the terminal cursor
   procedure Show_Cursor is
   begin
      Ada.Text_IO.Put (CSI & "?25h");
   end Show_Cursor;

   procedure Save_Cursor_Position is
   begin
      Ada.Text_IO.Put (CSI & "s");
   end Save_Cursor_Position;

   procedure Restore_Cursor_Position is
   begin
      Ada.Text_IO.Put (CSI & "u");
   end Restore_Cursor_Position;

   --  Sends ANSI code to the terminal to wipe the screen.
   --  This should be run once before any of the systems.
   procedure Clear_Screen is
   begin
      --  Enable VT processing first so ANSI sequences are honoured
      Enable_VT_Processing;
      Ada.Text_IO.Put (
         CSI & "?1049h" &   --  Switch to alternate screen buffer
         CSI & "?25l" &     --  Hide cursor (ANSI)
         CSI & "0m" &       --  Reset formatting
         CSI & "2J" &       --  Clear screen
         CSI & "1;1H");     --  Move to top-left
      --  Also hide cursor via Win32 API as a fallback
      Set_Cursor_Visible (False);
   end Clear_Screen;

   --  Resets terminal to normal state (resets colors and typefaces)
   procedure Reset_Styling is
   begin
      --  Reset all styling / attributes
      Ada.Text_IO.Put (CSI & "0m");
   end Reset_Styling;

end Graphics;
