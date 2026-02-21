with Interfaces.C; use Interfaces.C;

package body Console is

   -- Win32 Types and Constants
   type HANDLE is new System.Address;
   type BOOL is new int;
   type DWORD is new unsigned;

   -- If GetStdHandle fails, it returns 0 this is a 'NULL' check
   INVALID_HANDLE_VALUE : constant HANDLE := HANDLE(System.Null_Address);

   -- A magic number telling windows that it wants to handle Standard output of the terminal
   STD_OUTPUT_HANDLE    : constant DWORD := 4294967285; -- -11

   -- Hex value is a bit flag to telling windows to interpret ANSI sequences
   -- such sequences like ESC[31m instead of printing them as text
   ENABLE_VIRTUAL_TERMINAL_PROCESSING : constant DWORD := 16#0004#;

   -- Cursor Info Structure
   type CONSOLE_CURSOR_INFO is record
      Size    : DWORD; -- Size of cursor 1-100
      Visible : BOOL;  -- 0 for hidden, 1 for visible
   end record;

   -- Pragma tells Ada to arrange memory similar to a C 'struct'
   -- so Windows API can interpret it correctly
   pragma Convention (C, CONSOLE_CURSOR_INFO);

   -- Win32 API Imports that link to Windows kernel32.dll
   function GetStdHandle (nStdHandle : DWORD) return HANDLE
     with Import, Convention => Stdcall, External_Name => "GetStdHandle";

   function GetConsoleMode (hConsoleHandle : HANDLE; lpMode : access DWORD) return BOOL
     with Import, Convention => Stdcall, External_Name => "GetConsoleMode";

   function SetConsoleMode (hConsoleHandle : HANDLE; dwMode : DWORD) return BOOL
     with Import, Convention => Stdcall, External_Name => "SetConsoleMode";

   function SetConsoleCursorInfo (hConsoleHandle : HANDLE;
                                 lpConsoleCursorInfo : access CONSOLE_CURSOR_INFO) return BOOL
     with Import, Convention => Stdcall, External_Name => "SetConsoleCursorInfo";

   --=============================================================================
   -- Implementation for Windows using Win32 API calls to allow for ANSI codes
   --=============================================================================
   procedure Enable_VT_Processing is
      H    : constant HANDLE := GetStdHandle(STD_OUTPUT_HANDLE); -- Get permission to edit terminal
      Mode : aliased DWORD; -- Holds current terminal settings
      Res  : BOOL;
      pragma Warnings (Off, Res);
   begin
      if H /= INVALID_HANDLE_VALUE then
         -- Get current settings first Mode'Acess points to the variable
         Res := GetConsoleMode(H, Mode'Access);
         -- Bitwise OR to enable VT processing
         Res := SetConsoleMode(H, Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
      end if;
   end Enable_VT_Processing;

   --=============================================================================
   -- Implementation for Windows using Win32 API calls for hiding/showing cursor
   --=============================================================================
   procedure Set_Cursor_Visible (Visible : Boolean) is
      H    : constant HANDLE := GetStdHandle(STD_OUTPUT_HANDLE);
      Info : aliased CONSOLE_CURSOR_INFO; -- Creates a record to send to Windows
      Res  : BOOL;
      pragma Warnings (Off, Res);
   begin
      if H /= INVALID_HANDLE_VALUE then
         -- Windows requires the cursor size to be valid (1-100) even when hiding
         -- Do not set to 0 otherwise could fail, 25 is a standard size
         Info.Size := 25;
         -- True or False to set cursor visibility
         Info.Visible := (if Visible then 1 else 0);

         -- Send record to OS, even if Res isn't utilized it's to satisfy return type
         Res := SetConsoleCursorInfo(H, Info'Access);
      end if;
   end Set_Cursor_Visible;

end Console;
