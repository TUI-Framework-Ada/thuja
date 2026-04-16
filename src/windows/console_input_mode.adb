with Interfaces.C; use Interfaces.C;
with System;

package body Console_Input_Mode is

   type HANDLE is new System.Address;
   type BOOL   is new int;
   type DWORD  is new unsigned;

   INVALID_HANDLE_VALUE : constant HANDLE := HANDLE (System.Null_Address);
   STD_INPUT_HANDLE     : constant DWORD  := 4294967286; -- (DWORD)(-10)

   ENABLE_VIRTUAL_TERMINAL_INPUT : constant DWORD := 16#0200#;

   function GetStdHandle (nStdHandle : DWORD) return HANDLE
     with Import, Convention => Stdcall, External_Name => "GetStdHandle";

   function GetConsoleMode (hConsoleHandle : HANDLE; lpMode : access DWORD) return BOOL
     with Import, Convention => Stdcall, External_Name => "GetConsoleMode";

   function SetConsoleMode (hConsoleHandle : HANDLE; dwMode : DWORD) return BOOL
     with Import, Convention => Stdcall, External_Name => "SetConsoleMode";

   procedure Enable_VT_Input is
      H    : constant HANDLE := GetStdHandle (STD_INPUT_HANDLE);
      Mode : aliased DWORD;
      Res  : BOOL;
      pragma Warnings (Off, Res);
   begin
      if H /= INVALID_HANDLE_VALUE then
         Res := GetConsoleMode (H, Mode'Access);
         Res := SetConsoleMode (H, Mode or ENABLE_VIRTUAL_TERMINAL_INPUT);
      end if;
   end Enable_VT_Input;

end Console_Input_Mode;
