--  Thuja - Terminal User Interface Widget Library
--  Package Body

with Graphics; use Graphics;
with Ada.Text_IO;

package body Thuja is

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   procedure Log is
      --Pix : constant Pixel_t := ('A', Red, Black, True);
      -- This is to set these specific fields, and use defaults for everything else
      Pix : constant Pixel_t := (Char       => 'A', 
                              Char_Color => Red, 
                              Is_Bold    => True,
                              Is_Italic  => False,
                              Is_Underline => False, 
                              others     => <>);
   begin
      Ada.Text_IO.Put_Line (Pix'Image);
   end Log;

end Thuja;
