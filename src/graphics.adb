-- Package Body for Graphics
with Ada.Text_IO;
with Interfaces.C;


package body Graphics is

   --  Win32 Console API bindings.
   --  On Windows 10, ANSI/VT sequences require enabling
   --  ENABLE_VIRTUAL_TERMINAL_PROCESSING via SetConsoleMode.
   --  Cursor visibility is set via both the Win32 API and ANSI
   --  sequences for maximum compatibility across terminal hosts.

   type Win_HANDLE is new Interfaces.C.ptrdiff_t;
   type Win_BOOL   is new Interfaces.C.int;
   type Win_DWORD  is new Interfaces.C.unsigned_long;

   type Console_Cursor_Info_t is record
      Size     : Win_DWORD;
      Visible  : Win_BOOL;
   end record;
   pragma Convention (C, Console_Cursor_Info_t);

   use type Interfaces.C.int;

   STD_OUTPUT_HANDLE : constant Interfaces.C.int := -11;

   ENABLE_VIRTUAL_TERMINAL_PROCESSING : constant Win_DWORD := 16#0004#;

   function GetStdHandle (Std : Interfaces.C.int) return Win_HANDLE;
   pragma Import (StdCall, GetStdHandle, "GetStdHandle");

   function GetConsoleMode
     (H    : Win_HANDLE;
      Mode : access Win_DWORD) return Win_BOOL;
   pragma Import (StdCall, GetConsoleMode, "GetConsoleMode");

   function SetConsoleMode
     (H    : Win_HANDLE;
      Mode : Win_DWORD) return Win_BOOL;
   pragma Import (StdCall, SetConsoleMode, "SetConsoleMode");

   function GetConsoleCursorInfo
     (H    : Win_HANDLE;
      Info : access Console_Cursor_Info_t) return Win_BOOL;
   pragma Import (StdCall, GetConsoleCursorInfo, "GetConsoleCursorInfo");

   function SetConsoleCursorInfo
     (H    : Win_HANDLE;
      Info : access Console_Cursor_Info_t) return Win_BOOL;
   pragma Import (StdCall, SetConsoleCursorInfo, "SetConsoleCursorInfo");

   --  Enable ANSI/VT escape sequence processing on the console.
   --  Must be called before any ANSI output on Windows 10.
   procedure Enable_VT_Processing is
      H      : constant Win_HANDLE := GetStdHandle (STD_OUTPUT_HANDLE);
      Mode   : aliased Win_DWORD;
      Ignore : Win_BOOL;
   begin
      Ignore := GetConsoleMode (H, Mode'Access);
      Mode := Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      Ignore := SetConsoleMode (H, Mode);
   end Enable_VT_Processing;

   procedure Set_Cursor_Visible (Visible : Boolean) is
      H      : constant Win_HANDLE := GetStdHandle (STD_OUTPUT_HANDLE);
      Info   : aliased Console_Cursor_Info_t;
      Ignore : Win_BOOL;
   begin
      Ignore := GetConsoleCursorInfo (H, Info'Access);
      Info.Visible := (if Visible then 1 else 0);
      Ignore := SetConsoleCursorInfo (H, Info'Access);
   end Set_Cursor_Visible;

   -- Buffer_T Constructor - Allocates memory in the 2D pixel array, intializing record fields
   function Create_Buffer (Width : TUI_Width;
                           Height : TUI_Height)
                           return Buffer_T
   is

      -- Declares variable to be returned using Buffer_T
      New_Buffer : Buffer_T;

   begin

      -- Store dimensions in the record
      New_Buffer.Width := Width;
      New_Buffer.Height := Height;

      return New_Buffer;
   end Create_Buffer;

   -- Writes a new pixel value into the buffer at the (X,Y) coordinates
   procedure Set_Buffer_Pixel (B : in out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel_t)
   is
   begin
      -- Writes new pixel into buffer "P" being the value Pixel
      B.Data (X, Y) := P;
   end Set_Buffer_Pixel;

   -- Reads and returns the pixel value from the buffer at the (X, Y) coordinates
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel_t
   is
   begin
      -- Returns value read from the array
      return B.Data (X, Y);
   end Get_Buffer_Pixel;

   --  Enter fullscreen TUI mode.
   procedure Clear_Screen is
      CSI : constant String := Character'Val (16#1B#) & '[';
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

   --  Exit fullscreen TUI mode.
   procedure Restore_Screen is
      CSI : constant String := Character'Val (16#1B#) & '[';
   begin
      Set_Cursor_Visible (True);
      Ada.Text_IO.Put (
         CSI & "?25h" &     --  Show cursor (ANSI)
         CSI & "0m" &       --  Reset formatting
         CSI & "?1049l");   --  Restore main screen buffer
   end Restore_Screen;

end Graphics;
