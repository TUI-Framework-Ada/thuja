--  Thuja - Terminal User Interface Widget Library
--  Package Body

with Graphics; use Graphics;
with Ada.Text_IO;

package body Thuja is

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   procedure Log is
      Pix : constant Pixel_t := ('A', Red, Black, True);
   begin
      Ada.Text_IO.Put_Line (Pix'Image);
   end Log;

end Thuja;
