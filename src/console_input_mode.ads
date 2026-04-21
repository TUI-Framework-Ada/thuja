package Console_Input_Mode is

   --  Put the console's stdin into a mode where key presses arrive as the
   --  byte sequence Input_Handling.Parse_Input expects.
   --
   --  On Windows this enables ENABLE_VIRTUAL_TERMINAL_INPUT so that, for
   --  example, Alt+key arrives as ESC followed by the key character.
   --  On Linux a terminal in canonical-off mode already delivers bytes in
   --  that form, so the Linux body is a no-op.
   procedure Enable_VT_Input;

end Console_Input_Mode;
