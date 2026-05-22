package Console is

   -- Win32 API VT Processing to enable ANSI codes
   procedure Enable_VT_Processing;

   -- Procedure utilizing Win32 API to set cursor visibility (True or False)
   procedure Set_Cursor_Visible (Visible : Boolean);

end Console;
