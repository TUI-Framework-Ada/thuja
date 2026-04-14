with Interfaces.C;

--  Terminal_Size (Body)
--  C bindings to get the terminal size using tput on Linux
--  and the Win32 Console API on Windows.

package body Terminal_Size is

   function Get_Width_C return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "get_terminal_width";

   function Get_Height_C return Interfaces.C.int
   with
     Import        => True,
     Convention    => C,
     External_Name => "get_terminal_height";

   function Get_Width return Natural is
   begin
      return Natural (Get_Width_C);
   end Get_Width;

   function Get_Height return Natural is
   begin
      return Natural (Get_Height_C);
   end Get_Height;
end Terminal_Size;

