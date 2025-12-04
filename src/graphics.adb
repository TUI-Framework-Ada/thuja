--  Package Body for Graphics
with Ada.Text_IO;

package body Graphics is

   --  ANSI escape sequence prefix
   CSI : constant String := Character'Val (16#1B#) & '[';

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
      B.Data (X, Y) := P;
   end Set_Buffer_Pixel;

   --  Reads and returns the pixel value from the buffer at the (X, Y) coordinates
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel_t
   is
   begin
      --  Returns value read from the array
      return B.Data (X, Y);
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

   --  Sends ANSI code to the terminal to wipe the screen.
   --  This should be run once before any of the systems.
   procedure Clear_Screen is
   begin
      --  Clear formatting, clear screen, move cursor to top-left, hide cursor
      Ada.Text_IO.Put (CSI & "0m" & CSI & "2J" & CSI & "1;1H");
      Hide_Cursor;
   end Clear_Screen;

   --  Resets terminal to normal state (shows cursor, resets colors)
   procedure Reset_Terminal is
   begin
      --  Reset all attributes, show cursor
      Ada.Text_IO.Put (CSI & "0m");
      Show_Cursor;
   end Reset_Terminal;

end Graphics;