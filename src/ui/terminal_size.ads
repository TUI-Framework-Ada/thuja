package terminal_size is

   --  Returns the current terminal width in columns.
   --  Falls back to 80 if detection fails.
   function Get_Width return Natural;

   --  Returns the current terminal height in rows.
   --  Falls back to 50 if detection fails.
   function Get_Height return Natural;

end terminal_size;
